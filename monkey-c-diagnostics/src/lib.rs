use ariadne::{Color, Label, Report, ReportKind, Source};
use monkey_c_parser::parser::ParserError;

use std::fs;
use std::io::{self, Read};
use std::path::Path;

/// Read `path`'s bytes and decode as UTF-8, reporting an already-printed
/// diagnostic if the file isn't valid UTF-8 (e.g. a Latin-1 source file with
/// a stray accented character in a comment).
pub fn read_source(path: &Path) -> io::Result<String> {
    decode_source(&path.display().to_string(), fs::read(path)?)
}

/// Read stdin to completion and decode as UTF-8, with the same diagnostic as
/// [`read_source`] on failure.
pub fn read_stdin_source() -> io::Result<String> {
    let mut bytes = Vec::new();
    io::stdin().read_to_end(&mut bytes)?;

    decode_source("<stdin>", bytes)
}

/// Decode `bytes` as UTF-8, or print a labelled snippet pointing at the
/// first invalid byte (mirroring rustc's "stream did not contain valid
/// UTF-8" diagnostic) and return an already-reported error.
fn decode_source(label: &str, bytes: Vec<u8>) -> io::Result<String> {
    String::from_utf8(bytes).map_err(|e| {
        let offset = e.utf8_error().valid_up_to();
        let bytes = e.into_bytes();
        let bad_byte = bytes[offset];
        let char_pos = std::str::from_utf8(&bytes[..offset])
            .expect("prefix up to `valid_up_to` is valid UTF-8")
            .chars()
            .count();
        let lossy = String::from_utf8_lossy(&bytes).into_owned();

        Report::build(ReportKind::Error, (label, char_pos..char_pos + 1))
            .with_message("stream did not contain valid UTF-8")
            .with_label(
                Label::new((label, char_pos..char_pos + 1))
                    .with_message(format!("byte `{bad_byte}` is not valid utf-8"))
                    .with_color(Color::Red),
            )
            .finish()
            .eprint((label, Source::from(lossy)))
            .expect("ariadne write to stderr");

        already_reported()
    })
}

/// Render a [`ParserError`] as a labelled source snippet pointing at the
/// offending token.
pub fn render_parse_error(label: &str, source: &str, err: &ParserError) {
    let range = byte_range_to_char_range(source, err.span.start, err.span.end);
    let range = clamp_range(range, source.chars().count());

    Report::build(ReportKind::Error, (label, range.clone()))
        .with_message("parse error")
        .with_label(
            Label::new((label, range))
                .with_message(&err.message)
                .with_color(Color::Red),
        )
        .finish()
        .eprint((label, Source::from(source)))
        .expect("ariadne write to stderr");
}

/// Widen a (possibly empty, e.g. at end-of-file) char range to at least one
/// char so `ariadne` always has something to underline.
fn clamp_range(range: std::ops::Range<usize>, len: usize) -> std::ops::Range<usize> {
    if len == 0 {
        return 0..0;
    }

    let end = range.end.clamp(1, len);
    let start = range.start.min(end - 1);

    start..end
}

/// An error that has already been reported to the user (via `ariadne`) and
/// shouldn't be printed again by the caller.
pub fn already_reported() -> io::Error {
    io::Error::other("already reported")
}

/// Convert a `[byte_start, byte_end)` range in `source` into the matching
/// `[char_start, char_end)` range. `ariadne` indexes `Source` by char, so
/// passing byte offsets unchanged misplaces labels whenever the source
/// contains any multibyte UTF-8 character earlier in the file.
pub fn byte_range_to_char_range(
    source: &str,
    byte_start: usize,
    byte_end: usize,
) -> std::ops::Range<usize> {
    let mut char_start = 0;
    let mut char_end = 0;
    let mut byte_pos = 0;

    for c in source.chars() {
        if byte_pos == byte_start {
            char_start = char_end;
        }

        if byte_pos == byte_end {
            return char_start..char_end;
        }

        byte_pos += c.len_utf8();
        char_end += 1;
    }

    if byte_pos == byte_start {
        char_start = char_end;
    }

    char_start..char_end
}
