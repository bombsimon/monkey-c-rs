//! Source-snippet diagnostics shared by every subcommand.
//!
//! All rendering goes through [`Renderer`], which carries the resolved colour
//! choice so that a single `--color` decision applies to every report — reading
//! a file, parsing it, and linting it — rather than each call site guessing.

use ariadne::{Color, Config, IndexType, Label, Report, ReportKind, Source};
use monkey_c_parser::parser::{ParseOutput, Parser, ParserError};

use std::error;
use std::fmt;
use std::fs;
use std::io::{self, Read};
use std::ops::Range;
use std::path::Path;

use crate::runner::Target;

/// Renders diagnostics to stderr, with or without ANSI colour.
#[derive(Debug, Clone, Copy)]
pub struct Renderer {
    color: bool,
}

impl Renderer {
    pub fn new(color: bool) -> Self {
        Self { color }
    }

    pub fn color(&self) -> bool {
        self.color
    }

    /// The `ariadne` configuration for this renderer. Public so callers that
    /// build their own reports (the linter's findings) match these ones;
    /// `ariadne` filters explicit label colours through it, so turning colour
    /// off here is enough to make a whole report monochrome. Spans are byte
    /// offsets, as the parser produces them, so no report has to rescan the
    /// source to translate them.
    pub fn config(&self) -> Config {
        Config::default()
            .with_color(self.color)
            .with_index_type(IndexType::Byte)
    }

    /// Read `path`'s bytes and decode as UTF-8, reporting an already-printed
    /// diagnostic if the file isn't valid UTF-8 (e.g. a Latin-1 source file with
    /// a stray accented character in a comment).
    pub fn read_source(&self, path: &Path) -> io::Result<String> {
        self.decode_source(&path.display().to_string(), fs::read(path)?)
    }

    /// Read stdin to completion and decode as UTF-8, with the same diagnostic as
    /// [`Renderer::read_source`] on failure.
    pub fn read_stdin_source(&self) -> io::Result<String> {
        let mut bytes = Vec::new();
        io::stdin().read_to_end(&mut bytes)?;

        self.decode_source("<stdin>", bytes)
    }

    /// Decode `bytes` as UTF-8, or print a labelled snippet pointing at the
    /// first invalid byte (mirroring rustc's "stream did not contain valid
    /// UTF-8" diagnostic) and return an already-reported error.
    fn decode_source(&self, label: &str, bytes: Vec<u8>) -> io::Result<String> {
        String::from_utf8(bytes).map_err(|e| {
            let offset = e.utf8_error().valid_up_to();
            let bytes = e.into_bytes();
            let bad_byte = bytes[offset];
            // Everything before `offset` survives the lossy decode unchanged, and the bad byte
            // becomes one replacement character.
            let replaced = offset..offset + char::REPLACEMENT_CHARACTER.len_utf8();
            let lossy = String::from_utf8_lossy(&bytes).into_owned();

            Report::build(ReportKind::Error, (label, replaced.clone()))
                .with_config(self.config())
                .with_message("stream did not contain valid UTF-8")
                .with_label(
                    Label::new((label, replaced))
                        .with_message(format!("byte `{bad_byte}` is not valid utf-8"))
                        .with_color(Color::Red),
                )
                .finish()
                .eprint((label, Source::from(lossy)))
                .expect("ariadne write to stderr");

            already_reported()
        })
    }

    /// Read a target's source, from stdin or its file.
    pub fn read_target(&self, target: Target<'_>) -> io::Result<String> {
        match target {
            Target::Stdin => self.read_stdin_source(),
            Target::File(path) => self.read_source(path),
        }
    }

    /// Parse `source`, reporting a parse error before returning it.
    pub fn parse(&self, label: &str, source: &str) -> io::Result<ParseOutput> {
        Parser::new(source)
            .parse()
            .map_err(|error| self.parse_error(label, source, &error))
    }

    /// Print a [`ParserError`] as a labelled source snippet pointing at the offending token, and
    /// return the error that says it's been reported.
    pub fn parse_error(&self, label: &str, source: &str, err: &ParserError) -> io::Error {
        let range = widen_to_char(source, err.span.start..err.span.end);

        Report::build(ReportKind::Error, (label, range.clone()))
            .with_config(self.config())
            .with_message("parse error")
            .with_label(
                Label::new((label, range))
                    .with_message(&err.message)
                    .with_color(Color::Red),
            )
            .finish()
            .eprint((label, Source::from(source)))
            .expect("ariadne write to stderr");

        already_reported()
    }

    /// Render a warning that the formatter could not preserve the comment at
    /// `[byte_start, byte_end)` — its content would be dropped from the
    /// formatted output. Points at the original comment so the user can see
    /// exactly what is at risk, mirroring the parse-error / UTF-8 diagnostics.
    pub fn lost_comment(&self, label: &str, source: &str, byte_start: usize, byte_end: usize) {
        let range = widen_to_char(source, byte_start..byte_end);

        Report::build(ReportKind::Warning, (label, range.clone()))
            .with_config(self.config())
            .with_message("comment cannot be preserved by the formatter")
            .with_label(
                Label::new((label, range))
                    .with_message("this comment would be dropped from the output")
                    .with_color(Color::Yellow),
            )
            .finish()
            .eprint((label, Source::from(source)))
            .expect("ariadne write to stderr");
    }
}

/// Clamp a byte range to `source` and widen it when empty, e.g. at end-of-file, to cover at least
/// one whole char so `ariadne` always has something to underline.
fn widen_to_char(source: &str, range: Range<usize>) -> Range<usize> {
    let range = range.start.min(source.len())..range.end.min(source.len());
    if !range.is_empty() || source.is_empty() {
        return range;
    }

    if range.start < source.len() {
        return range.start..source.ceil_char_boundary(range.start + 1);
    }

    source.floor_char_boundary(source.len() - 1)..source.len()
}

/// The payload that marks an error as already rendered.
///
/// This is a distinct type rather than a message or an [`io::ErrorKind`] so the
/// marker cannot be confused with an unrelated failure: every error built from
/// something that isn't an `io::Error` lands in `ErrorKind::Other` too, and
/// matching on the kind alone would silently swallow all of them.
#[derive(Debug)]
struct AlreadyReported;

impl fmt::Display for AlreadyReported {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("already reported")
    }
}

impl error::Error for AlreadyReported {}

/// An error that has already been reported to the user (via `ariadne`) and
/// shouldn't be printed again by the caller.
pub fn already_reported() -> io::Error {
    io::Error::other(AlreadyReported)
}

/// Whether `error` was already rendered as a diagnostic, and so must not be
/// printed a second time as bare text.
pub fn is_already_reported(error: &io::Error) -> bool {
    error
        .get_ref()
        .is_some_and(|inner| inner.is::<AlreadyReported>())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn already_reported_errors_are_recognised() {
        assert!(is_already_reported(&already_reported()));
        assert!(!is_already_reported(&io::Error::from(
            io::ErrorKind::NotFound
        )));
    }

    #[test]
    fn an_unrelated_wrapped_error_is_not_treated_as_reported() {
        // Anything wrapped with `io::Error::other` also has kind `Other`, so a
        // real failure — a failed directory walk, a language server that won't
        // start — must not be mistaken for a diagnostic that was already
        // printed, or it would be swallowed and leave a bare non-zero exit code.
        assert!(!is_already_reported(&io::Error::other("walk failed")));
    }

    #[test]
    fn an_empty_range_widens_to_the_char_after_it() {
        // "å" is two bytes, so covering it takes both.
        assert_eq!(widen_to_char("aåb", 1..1), 1..3);
    }

    #[test]
    fn an_empty_range_at_the_end_widens_to_the_last_char() {
        assert_eq!(widen_to_char("abå", 4..4), 2..4);
    }

    #[test]
    fn a_range_past_the_end_is_clamped_to_the_source() {
        assert_eq!(widen_to_char("abc", 1..9), 1..3);
        assert_eq!(widen_to_char("abå", 7..9), 2..4);
    }

    #[test]
    fn a_non_empty_range_is_kept() {
        assert_eq!(widen_to_char("abc", 0..2), 0..2);
        assert_eq!(widen_to_char("", 0..0), 0..0);
    }
}
