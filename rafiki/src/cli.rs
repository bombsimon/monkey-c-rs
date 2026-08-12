//! The command-line surface.
//!
//! Flags that also exist as `rafiki.toml` keys are modelled as `Option`s (or as
//! a `--flag`/`--no-flag` pair for booleans) rather than as values with
//! defaults. That is what makes "flag beats file beats default" expressible: a
//! flag the user did not pass is `None` and so does not overlay the file, which
//! a `default_value_t` could never distinguish from an explicit choice.

use clap::{Args, Parser, Subcommand, ValueEnum};
use monkey_c_config::{ConfigSource, FilesConfig, FormatConfig, LintConfig};

use std::path::PathBuf;

use crate::color::{self, Stream};
use crate::diagnostics::Renderer;

/// Formatter, linter and language server for the Monkey C language.
#[derive(Debug, Parser)]
#[command(name = "rafiki", version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,

    #[command(flatten)]
    pub global: GlobalArgs,
}

#[derive(Debug, Subcommand)]
pub enum Command {
    /// Format Monkey C source code.
    Fmt(FmtArgs),

    /// Lint Monkey C source code.
    Lint(LintArgs),

    /// Instrument, run and report Monkey C test coverage.
    #[command(subcommand)]
    Coverage(CoverageCommand),

    /// Run the language server on stdio.
    Server,

    /// Print a shell completion script.
    Completions(CompletionsArgs),
}

/// Options accepted before or after any subcommand.
#[derive(Debug, Args)]
#[command(next_help_heading = "Global options")]
pub struct GlobalArgs {
    /// Read settings from this file instead of discovering a `rafiki.toml`.
    #[arg(long, global = true, value_name = "PATH")]
    pub config: Option<PathBuf>,

    /// Ignore any `rafiki.toml` and use the built-in defaults.
    #[arg(long, global = true, conflicts_with = "config")]
    pub no_config: bool,

    /// When to use colour in diagnostics.
    #[arg(long, global = true, value_name = "WHEN", default_value = "auto")]
    pub color: ColorWhen,
}

impl GlobalArgs {
    /// Where this run takes its `rafiki.toml` from.
    pub fn config_source(&self) -> ConfigSource {
        ConfigSource::from_flags(self.config.clone(), self.no_config)
    }

    /// The diagnostic renderer for this run. Diagnostics go to stderr, so the
    /// colour decision is made against stderr.
    pub fn renderer(&self) -> Renderer {
        Renderer::new(color::enabled(self.color, Stream::Stderr))
    }

    /// Whether to colour output written to stdout, which is a separate question
    /// from stderr — `rafiki fmt --diff | less` should stay plain even when the
    /// diagnostics beside it are coloured.
    pub fn stdout_color(&self) -> bool {
        color::enabled(self.color, Stream::Stdout)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum ColorWhen {
    /// Colour when writing to a terminal.
    Auto,
    Always,
    Never,
}

#[derive(Debug, Args)]
pub struct FmtArgs {
    /// Files or directories to format. Directories are walked recursively for
    /// `.mc` files. Use `-` to read from stdin and write the result to stdout.
    #[arg(default_value = ".")]
    pub paths: Vec<PathBuf>,

    /// Check whether each file is already formatted, without writing. Exits 1
    /// when any file would change.
    #[arg(short, long)]
    pub check: bool,

    /// Print a unified diff of what would change, without writing. Exits 1 when
    /// any file would change.
    #[arg(short, long)]
    pub diff: bool,

    #[command(flatten)]
    pub format: FormatArgs,

    #[command(flatten)]
    pub files: FileArgs,
}

#[derive(Debug, Args)]
#[command(after_help = available_rules_help())]
pub struct LintArgs {
    /// Files or directories to lint. Directories are walked recursively for
    /// `.mc` files. Use `-` to read from stdin.
    #[arg(default_value = ".")]
    pub paths: Vec<PathBuf>,

    /// Apply machine-applicable fixes in place. Ignored for stdin input.
    #[arg(long)]
    pub fix: bool,

    /// Only run these rules, as a comma-separated list. Repeatable.
    #[arg(long, value_delimiter = ',', value_name = "RULE")]
    pub enable: Vec<String>,

    /// Silence these rules, as a comma-separated list. Repeatable. A rule in
    /// both lists is silenced.
    #[arg(long, value_delimiter = ',', value_name = "RULE")]
    pub disable: Vec<String>,

    /// List every available rule and exit.
    #[arg(long)]
    pub list_rules: bool,

    #[command(flatten)]
    pub files: FileArgs,
}

impl LintArgs {
    pub fn to_config(&self) -> LintConfig {
        LintConfig::from_flags(&self.enable, &self.disable)
    }
}

/// The `Available rules:` block appended to `lint --help`, built from the single
/// source of truth in [`monkey_c_linter::rules::ALL`] so it cannot list a rule
/// that does not exist.
fn available_rules_help() -> String {
    let mut help = String::from("Available rules:\n");

    for rule in monkey_c_linter::rules::ALL {
        help.push_str("  ");
        help.push_str(rule);
        help.push('\n');
    }

    help
}

/// The `[format]` keys as flags.
#[derive(Debug, Args)]
#[command(next_help_heading = "Formatting")]
pub struct FormatArgs {
    /// Target line width before wrapping. [default: 111]
    #[arg(short = 'l', long, value_name = "COLUMNS")]
    pub line_width: Option<usize>,

    /// Align enum values and fat commas (`=>`). [default: enabled]
    #[arg(long, overrides_with = "no_alignment")]
    pub alignment: bool,

    /// Do not align enum values and fat commas.
    #[arg(long, overrides_with = "alignment")]
    pub no_alignment: bool,

    /// Wrap multiple declarations similar to the Prettier formatter.
    /// [default: disabled]
    #[arg(short = 'w', long, overrides_with = "no_wrap_declarations")]
    pub wrap_declarations: bool,

    /// Do not wrap multiple declarations.
    #[arg(long, overrides_with = "wrap_declarations")]
    pub no_wrap_declarations: bool,
}

impl FormatArgs {
    pub fn to_config(&self) -> FormatConfig {
        FormatConfig {
            line_width: self.line_width,
            alignment: flag_pair(self.alignment, self.no_alignment),
            wrap_declarations: flag_pair(self.wrap_declarations, self.no_wrap_declarations),
        }
    }
}

/// The `[files]` keys as flags.
#[derive(Debug, Args)]
#[command(next_help_heading = "File selection")]
pub struct FileArgs {
    /// Skip paths matching this gitignore-syntax glob. Repeatable.
    #[arg(long, value_name = "GLOB")]
    pub exclude: Vec<String>,

    /// Skip files ignored by `.gitignore`. [default: enabled]
    #[arg(long, overrides_with = "no_respect_gitignore")]
    pub respect_gitignore: bool,

    /// Include files that `.gitignore` would exclude.
    #[arg(long, overrides_with = "respect_gitignore")]
    pub no_respect_gitignore: bool,
}

impl FileArgs {
    pub fn to_config(&self) -> FilesConfig {
        FilesConfig {
            exclude: (!self.exclude.is_empty()).then(|| self.exclude.clone()),
            respect_gitignore: flag_pair(self.respect_gitignore, self.no_respect_gitignore),
        }
    }
}

#[derive(Debug, Subcommand)]
pub enum CoverageCommand {
    /// Rewrite sources with coverage probes into an output directory.
    Instrument(CoverageInstrumentArgs),

    /// Print per-file coverage from a captured simulator log.
    Report(CoverageReportArgs),

    /// Instrument, build, run under the simulator, and report in one step.
    Test(CoverageTestArgs),
}

#[derive(Debug, Args)]
pub struct CoverageInstrumentArgs {
    /// Monkey C source files or directories to instrument. Defaults to the
    /// whole project (the nearest ancestor holding `manifest.xml`), so the
    /// tool can be run from any subdirectory of it.
    pub files: Vec<PathBuf>,

    /// Directory that receives the instrumented copies. Cleared on each run
    /// so stale instrumented files never leak into a build. Defaults to
    /// `{repo_root}/bin/coverage`.
    #[arg(long)]
    pub out: Option<PathBuf>,

    /// Additional annotation names (beyond `test` and `release`) whose
    /// declarations should be skipped, e.g. `--exclude-annotation foo,bar`
    /// or repeated `--exclude-annotation foo --exclude-annotation bar`.
    #[arg(long, value_delimiter = ',')]
    pub exclude_annotation: Vec<String>,

    /// Jungle file describing the project, copied and rewritten into
    /// `{out}/coverage.jungle` so the instrumented build can be compiled with
    /// `monkeyc -f`. Defaults to `{repo_root}/monkey.jungle`.
    #[arg(long)]
    pub jungle: Option<PathBuf>,
}

#[derive(Debug, Args)]
pub struct CoverageReportArgs {
    /// Captured simulator output containing COVHIT lines (e.g. from
    /// `monkeydo -t`). Pass `-` to read from stdin, e.g.
    /// `monkeydo … -t | rafiki coverage report -`.
    pub log: PathBuf,

    /// Directory produced by `instrument`, holding coverage-manifest.tsv.
    /// Defaults to `{repo_root}/bin/coverage`.
    #[arg(long)]
    pub dir: Option<PathBuf>,
}

#[derive(Debug, Args)]
pub struct CoverageTestArgs {
    /// Monkey C source files or directories to instrument. Defaults to the
    /// whole project (the nearest ancestor holding `manifest.xml`), so the
    /// tool can be run from any subdirectory of it.
    pub files: Vec<PathBuf>,

    /// Device to build and run for, e.g. `fr965`. Passed to `monkeyc -d` and
    /// `monkeydo`.
    #[arg(short = 'd', long)]
    pub device: String,

    /// Developer key used to sign the build. Passed to `monkeyc -y`.
    #[arg(short = 'y', long)]
    pub key: PathBuf,

    /// Directory that receives the instrumented copies. Cleared on each run
    /// so stale instrumented files never leak into a build. Defaults to
    /// `{repo_root}/bin/coverage`.
    #[arg(long)]
    pub out: Option<PathBuf>,

    /// Additional annotation names (beyond `test` and `release`) whose
    /// declarations should be skipped, e.g. `--exclude-annotation foo,bar`
    /// or repeated `--exclude-annotation foo --exclude-annotation bar`.
    #[arg(long, value_delimiter = ',')]
    pub exclude_annotation: Vec<String>,

    /// Jungle file describing the project, copied and rewritten into
    /// `{out}/coverage.jungle` so the instrumented build can be compiled with
    /// `monkeyc -f`. Defaults to `{repo_root}/monkey.jungle`.
    #[arg(long)]
    pub jungle: Option<PathBuf>,

    /// Print the monkeyc/monkeydo commands this would run, without running
    /// them.
    #[arg(long)]
    pub dry_run: bool,

    /// If the first `monkeydo` attempt produces no coverage hits, launch the
    /// simulator with `connectiq` (installed alongside `monkeyc`/`monkeydo`)
    /// and retry once. Left off by default because `connectiq` brings an
    /// already-running simulator's window to the front, which is only worth
    /// doing when `monkeydo` actually needed it.
    #[arg(long)]
    pub start_simulator: bool,

    /// Seconds to wait for the simulator to come up before retrying. Only
    /// used with `--start-simulator`.
    #[arg(long, default_value_t = 5)]
    pub simulator_boot_time: u64,

    /// Extra arguments forwarded to `monkeyc` verbatim, after `--`, e.g.
    /// `-- -O 3 -w`.
    #[arg(last = true)]
    pub monkeyc_args: Vec<String>,
}

#[derive(Debug, Args)]
pub struct CompletionsArgs {
    /// The shell to generate a completion script for.
    pub shell: clap_complete::Shell,
}

/// Collapse a `--flag`/`--no-flag` pair into an overlay value. Neither given is
/// `None`, which leaves the underlying layer's value in place. The two flags
/// override each other in clap, so at most one is ever set.
fn flag_pair(yes: bool, no: bool) -> Option<bool> {
    match (yes, no) {
        (true, _) => Some(true),
        (_, true) => Some(false),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::CommandFactory;

    #[test]
    fn the_command_tree_is_valid() {
        // Catches conflicting flags, duplicate short options and bad
        // `overrides_with` targets, which clap only validates at runtime.
        Cli::command().debug_assert();
    }

    fn fmt_args(args: &[&str]) -> FmtArgs {
        let cli = Cli::try_parse_from(args).expect("arguments parse");
        match cli.command {
            Command::Fmt(args) => args,
            other => panic!("expected fmt, got {other:?}"),
        }
    }

    #[test]
    fn an_unset_flag_does_not_overlay() {
        let config = fmt_args(&["rafiki", "fmt"]).format.to_config();
        assert_eq!(config, FormatConfig::default());
    }

    #[test]
    fn a_negative_flag_overlays_false() {
        let config = fmt_args(&["rafiki", "fmt", "--no-alignment"])
            .format
            .to_config();
        assert_eq!(config.alignment, Some(false));
        assert_eq!(config.wrap_declarations, None);
    }

    #[test]
    fn the_last_of_a_flag_pair_wins() {
        let config = fmt_args(&["rafiki", "fmt", "--no-alignment", "--alignment"])
            .format
            .to_config();
        assert_eq!(config.alignment, Some(true));

        let config = fmt_args(&["rafiki", "fmt", "--alignment", "--no-alignment"])
            .format
            .to_config();
        assert_eq!(config.alignment, Some(false));
    }

    #[test]
    fn global_flags_are_accepted_after_the_subcommand() {
        let cli = Cli::try_parse_from(["rafiki", "fmt", "--color", "never"]).expect("parses");
        assert_eq!(cli.global.color, ColorWhen::Never);
    }

    #[test]
    fn global_flags_are_accepted_before_the_subcommand() {
        let cli = Cli::try_parse_from(["rafiki", "--color", "never", "fmt"]).expect("parses");
        assert_eq!(cli.global.color, ColorWhen::Never);
    }

    #[test]
    fn config_and_no_config_conflict() {
        Cli::try_parse_from(["rafiki", "fmt", "--no-config", "--config", "x.toml"])
            .expect_err("mutually exclusive");
    }

    #[test]
    fn fmt_defaults_to_the_current_directory() {
        assert_eq!(fmt_args(&["rafiki", "fmt"]).paths, vec![PathBuf::from(".")]);
    }

    #[test]
    fn lint_rule_lists_are_comma_separated() {
        let cli = Cli::try_parse_from(["rafiki", "lint", "--disable", "a,b"]).expect("parses");
        let Command::Lint(args) = cli.command else {
            panic!("expected lint");
        };

        assert_eq!(args.disable, vec!["a", "b"]);
    }

    #[test]
    fn coverage_instrument_parses_its_flags() {
        let cli = Cli::try_parse_from([
            "rafiki",
            "coverage",
            "instrument",
            "--out",
            "bin/cov",
            "--exclude-annotation",
            "debug,background",
            "src/",
        ])
        .expect("parses");
        let Command::Coverage(CoverageCommand::Instrument(args)) = cli.command else {
            panic!("expected coverage instrument");
        };

        assert_eq!(args.out, Some(PathBuf::from("bin/cov")));
        assert_eq!(args.exclude_annotation, vec!["debug", "background"]);
        assert_eq!(args.files, vec![PathBuf::from("src/")]);
    }

    #[test]
    fn coverage_report_reads_the_log_positionally() {
        let cli = Cli::try_parse_from(["rafiki", "coverage", "report", "-"]).expect("parses");
        let Command::Coverage(CoverageCommand::Report(args)) = cli.command else {
            panic!("expected coverage report");
        };

        assert_eq!(args.log, PathBuf::from("-"));
        assert_eq!(args.dir, None);
    }

    #[test]
    fn coverage_test_forwards_trailing_args_to_monkeyc() {
        let cli = Cli::try_parse_from([
            "rafiki", "coverage", "test", "-d", "fr965", "-y", "key.der", "--", "-O", "3",
        ])
        .expect("parses");
        let Command::Coverage(CoverageCommand::Test(args)) = cli.command else {
            panic!("expected coverage test");
        };

        assert_eq!(args.device, "fr965");
        assert_eq!(args.key, PathBuf::from("key.der"));
        assert_eq!(args.monkeyc_args, vec!["-O", "3"]);
    }
}
