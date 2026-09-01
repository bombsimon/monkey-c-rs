//! `rafiki coverage` — instrument, run and report Monkey C test coverage.
//!
//! Connect IQ has no native coverage support, so coverage is obtained by
//! rewriting the source before compilation: [`monkey_c_coverage::instrument`]
//! splices a probe after every function body's opening brace, a generated
//! runtime module prints each probe's id the first time it executes, and
//! `report` joins the captured simulator log back against a manifest of
//! those probes.
//!
//! Everything anchors to the Connect IQ project root — the nearest ancestor
//! holding `manifest.xml` — rather than the working directory, since `fmt`
//! and `lint`'s "current directory" default makes no sense for a build
//! pipeline that also needs to find `monkey.jungle` and write build output
//! under a fixed `bin/` regardless of which subdirectory it is invoked from.

use comfy_table::Table;
use monkey_c_config::FilesSettings;
use monkey_c_coverage::{
    FunctionSite, coverage_jungle, instrument, manifest_line, parse_hits, parse_manifest_line,
    runtime_module,
};

use std::collections::{BTreeMap, HashSet};
use std::env;
use std::fs;
use std::io::{self, Write};
use std::path::{Component, Path, PathBuf};
use std::process::Output;
use std::time::Duration;

use crate::cli::{
    CoverageCommand, CoverageInstrumentArgs, CoverageTestArgs, GlobalArgs, OutFormat,
};
use crate::diagnostics::{self, Renderer};
use crate::discovery;

const MANIFEST_FILE: &str = "coverage-manifest.tsv";

pub fn run(global: &GlobalArgs, command: &CoverageCommand) -> io::Result<bool> {
    let renderer = global.renderer();

    match command {
        CoverageCommand::Instrument(args) => run_instrument(&renderer, args).map(|_| true),
        CoverageCommand::Report(args) => run_report(
            &renderer,
            args.dir.as_deref(),
            &args.log,
            args.out_format,
            args.out.as_deref(),
        )
        .map(|_| true),
        CoverageCommand::Test(args) => run_test(&renderer, args),
    }
}

/// Instrument, compile, run under the simulator, and report — see
/// [`CoverageCommand::Test`]. `device` and `key` are forwarded to
/// `monkeyc`/`monkeydo` verbatim; everything else is the same as
/// `instrument`, reused here so the compile and run steps see exactly the
/// paths `instrument` just wrote.
fn run_test(renderer: &Renderer, args: &CoverageTestArgs) -> io::Result<bool> {
    let out = run_instrument(
        renderer,
        &CoverageInstrumentArgs {
            files: args.files.clone(),
            out: args.instrument_out.clone(),
            exclude_annotation: args.exclude_annotation.clone(),
            jungle: args.jungle.clone(),
        },
    )?;

    let jungle_path = out.join("coverage.jungle");
    let binary_path = out.join("cov.prg");
    let log_path = out.join("run.log");

    let mut monkeyc_command = vec![
        "monkeyc".to_string(),
        "-f".to_string(),
        jungle_path.display().to_string(),
        "-d".to_string(),
        args.device.clone(),
        "-o".to_string(),
        binary_path.display().to_string(),
        "-y".to_string(),
        args.key.display().to_string(),
        "--unit-test".to_string(),
    ];
    monkeyc_command.extend(args.monkeyc_args.iter().cloned());

    let monkeydo_command = [
        "monkeydo".to_string(),
        binary_path.display().to_string(),
        args.device.clone(),
        "-t".to_string(),
    ];

    if args.dry_run {
        eprintln!("{}", monkeyc_command.join(" "));
        eprintln!("{}", monkeydo_command.join(" "));
        if args.start_simulator {
            eprintln!("connectiq  # only if the monkeydo attempt above produces no coverage hits");
        }

        return Ok(true);
    }

    run_monkeyc(&monkeyc_command)?;

    let mut monkeydo_output = run_captured(&monkeydo_command)?;
    let mut stdout = String::from_utf8_lossy(&monkeydo_output.stdout).into_owned();
    let mut hits = parse_hits(&stdout);

    if hits.is_empty() && args.start_simulator {
        eprintln!(
            "monkeydo produced no coverage hits; starting the simulator with `connectiq` and retrying once"
        );

        start_simulator(Duration::from_secs(args.simulator_boot_time))?;
        monkeydo_output = run_captured(&monkeydo_command)?;
        stdout = String::from_utf8_lossy(&monkeydo_output.stdout).into_owned();
        hits = parse_hits(&stdout);
    }

    if hits.is_empty() {
        // Nothing ran at all — even after a retry, if `--start-simulator` was
        // given. A "0/N covered" table would dress this up as a real (if
        // terrible) coverage result, when actually the build never executed;
        // show what monkeydo said instead and stop here, same as a `monkeyc`
        // failure, rather than handing it to `report`.
        println!();
        print_captured(&monkeydo_output);
        println!();

        return Err(diagnostics::already_reported());
    }

    // Coverage hits only tell us code ran, not whether the tests it ran
    // passed — a failing assertion still exercises (and covers) the code
    // around it — and the exit status can't be trusted either (`monkeydo -t`
    // can return non-zero even after a full `PASSED` summary). The actual
    // verdict is monkeydo's own `PASSED (passed=1, failed=0, errors=0)` /
    // `FAILED (...)` summary line.
    let summary = test_summary(&stdout);
    let failed = summary.is_some_and(summary_failed);

    if failed {
        // Something's wrong: show everything monkeydo printed to help debug —
        // `summary` is part of this, so it isn't also printed on its own.
        // Coverage is still meaningful here (the code did run), so this
        // still falls through to `report` below rather than stopping.
        println!();
        print_captured(&monkeydo_output);
        println!();
    } else if let Some(summary) = summary {
        // A clean run stays otherwise quiet, but the one-line verdict is
        // exactly what a CI log should show without digging further.
        println!("\n{summary}\n");
    }

    fs::write(&log_path, &monkeydo_output.stdout)?;

    run_report(
        renderer,
        Some(&out),
        &log_path,
        args.out_format,
        args.out.as_deref(),
    )?;

    // A failing test is a finding, not a broken invocation — it is already
    // printed above, so the command only needs to report "not clean" here.
    Ok(!failed)
}

/// Find monkeydo's own test-runner summary line, e.g. `PASSED (passed=1,
/// failed=0, errors=0)` or `FAILED (passed=0, failed=0, errors=1)`.
fn test_summary(stdout: &str) -> Option<&str> {
    stdout
        .lines()
        .map(str::trim)
        .find(|line| line.starts_with("PASSED (") || line.starts_with("FAILED ("))
}

/// Whether a summary line found by [`test_summary`] reports any failed or
/// errored test — the actual pass/fail verdict (see [`run_test`] for why
/// neither coverage hits nor the process exit status capture it).
fn summary_failed(summary: &str) -> bool {
    let Some(counts) = summary
        .strip_prefix("PASSED (")
        .or_else(|| summary.strip_prefix("FAILED ("))
        .and_then(|s| s.strip_suffix(')'))
    else {
        return false;
    };

    let count = |field: &str| {
        counts
            .split(", ")
            .find_map(|part| part.strip_prefix(field))
            .and_then(|n| n.parse::<u32>().ok())
            .unwrap_or(0)
    };

    count("failed=") > 0 || count("errors=") > 0
}

/// Run `command` (`monkeyc`), capturing its output instead of showing it
/// live, and only printing it — then failing — if it exits non-zero. A
/// compile failure is always worth seeing; a clean build isn't.
fn run_monkeyc(command: &[String]) -> io::Result<()> {
    let output = run_captured(command)?;

    if !output.status.success() {
        print_captured(&output);

        return Err(diagnostics::already_reported());
    }

    Ok(())
}

/// Run `command` to completion, capturing rather than showing its output.
fn run_captured(command: &[String]) -> io::Result<Output> {
    std::process::Command::new(&command[0])
        .args(&command[1..])
        .output()
        .map_err(|e| io::Error::new(e.kind(), format!("{}: {e}", command[0])))
}

/// Write a captured child's stdout and stderr to our own, in that order,
/// dropping any stdout line starting with `COVHIT ` — the probe markers
/// `report` reads from the log file `print_captured` never touches, not
/// something worth showing a human staring at a failure.
fn print_captured(output: &Output) {
    for line in String::from_utf8_lossy(&output.stdout).lines() {
        if !line.starts_with("COVHIT ") {
            println!("{line}");
        }
    }

    _ = io::stderr().write_all(&output.stderr);
}

/// Launch the Connect IQ Simulator via `connectiq` (installed alongside
/// `monkeyc`/`monkeydo`) and give it `boot_time` to come up before the
/// caller retries. `connectiq` is idempotent — safe to call when the
/// simulator is already running — but it does bring that window to the
/// front, which is why callers only reach for this once a first `monkeydo`
/// attempt has already come back with no coverage hits, rather than
/// unconditionally.
fn start_simulator(boot_time: Duration) -> io::Result<()> {
    std::process::Command::new("connectiq")
        .spawn()
        .map_err(|e| io::Error::new(e.kind(), format!("connectiq: {e}")))?;

    std::thread::sleep(boot_time);

    Ok(())
}

fn run_instrument(renderer: &Renderer, args: &CoverageInstrumentArgs) -> io::Result<PathBuf> {
    let exclude_annotation: Vec<&str> =
        args.exclude_annotation.iter().map(String::as_str).collect();
    let root = project_root()?;
    let out = args
        .out
        .clone()
        .unwrap_or_else(|| root.join("bin/coverage"));
    let jungle = args
        .jungle
        .clone()
        .unwrap_or_else(|| root.join("monkey.jungle"));

    // A previous run's files would otherwise be recycled into the next build
    // (and report) even after their sources were renamed or removed.
    if out.exists() {
        fs::remove_dir_all(&out)?;
    }

    let paths: Vec<PathBuf> = if args.files.is_empty() {
        vec![root.clone()]
    } else {
        // Always ignore any explicit path that names our own output
        // directory — it holds generated files from a previous run.
        args.files.iter().filter(|p| **p != out).cloned().collect()
    };

    let files = collect_mc_files(&root, &paths)?;
    if files.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "no input files",
        ));
    }

    let mut manifest = String::new();
    let mut next_id = 0;

    for file in &files {
        let source = renderer.read_source(file)?;
        let dest = mirrored_path(file, &root);
        let name = dest.to_string_lossy().into_owned();
        let result = instrument(&source, &name, next_id, &exclude_annotation).map_err(|e| {
            renderer.parse_error(&name, &source, &e);
            diagnostics::already_reported()
        })?;

        next_id += result.sites.len();
        for site in &result.sites {
            manifest.push_str(&manifest_line(site));
            manifest.push('\n');
        }

        write(&out.join(&dest), &result.source)?;
    }

    write(&out.join("AutoGeneratedCov.mc"), &runtime_module(next_id))?;
    write(&out.join(MANIFEST_FILE), &manifest)?;

    let jungle_source = renderer.read_source(&jungle)?;
    // `manifest.xml` isn't copied into `out` the way `.mc` sources are, so
    // the copied jungle needs a path back to wherever it actually lives.
    let manifest_path = relative_path(&out, &root.join("manifest.xml"));
    let coverage_jungle_source = coverage_jungle(&jungle_source, &manifest_path.to_string_lossy())
        .map_err(|e| {
            renderer.parse_error(&jungle.display().to_string(), &jungle_source, &e);
            diagnostics::already_reported()
        })?;

    write(&out.join("coverage.jungle"), &coverage_jungle_source)?;

    eprintln!(
        "instrumented {next_id} function bodies across {} files",
        files.len()
    );

    Ok(out)
}

/// The Connect IQ project root: the nearest ancestor holding `manifest.xml`.
/// Everything anchors here rather than the working directory, so the
/// mirrored tree, `--out` and the manifest paths come out the same wherever
/// the tool is invoked from.
fn project_root() -> io::Result<PathBuf> {
    let mut dir = env::current_dir()?;

    loop {
        // Canonicalized so it strips against the canonicalized paths
        // `collect_mc_files` produces even where `current_dir` doesn't
        // already resolve symlinks itself.
        if dir.join("manifest.xml").is_file() {
            return fs::canonicalize(dir);
        }

        if !dir.pop() {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                "no manifest.xml in this directory or any parent — run from inside a Connect IQ project",
            ));
        }
    }
}

/// Resolve `paths` into a deterministic, canonicalized list of `.mc` files
/// under `root`, reusing [`discovery::collect`] for the walk itself: hidden
/// directories are skipped, named files are accepted whatever their
/// extension, and `bin/` — where the compiler and Monkey C Optimizer write
/// build artifacts, including our own `--out` default — is always excluded
/// so a whole-project walk never re-instruments a previous run's output.
/// Every path is canonicalized before being returned, so overlapping inputs
/// (an explicit file plus a directory containing it, or `source/A.mc`
/// alongside `./source/A.mc`) dedupe to one entry instead of instrumenting
/// the same file twice under different ids.
fn collect_mc_files(root: &Path, paths: &[PathBuf]) -> io::Result<Vec<PathBuf>> {
    let settings = FilesSettings {
        exclude: vec!["bin/**".to_string()],
        respect_gitignore: false,
    };

    let files = discovery::collect(paths, &settings, root)?;

    let mut files: Vec<PathBuf> = files
        .iter()
        .map(fs::canonicalize)
        .collect::<io::Result<_>>()?;

    files.sort();
    files.dedup();

    Ok(files)
}

fn run_report(
    renderer: &Renderer,
    dir: Option<&Path>,
    log: &Path,
    format: OutFormat,
    out: Option<&Path>,
) -> io::Result<()> {
    let dir = match dir {
        Some(dir) => dir.to_path_buf(),
        None => project_root()?.join("bin/coverage"),
    };

    let sites: Vec<FunctionSite> = renderer
        .read_source(&dir.join(MANIFEST_FILE))?
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            parse_manifest_line(line).ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("bad manifest line: {line}"),
                )
            })
        })
        .collect::<io::Result<_>>()?;

    if sites.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "manifest contains no reportable sites",
        ));
    }

    let log = if log == Path::new("-") {
        renderer.read_stdin_source()?
    } else {
        renderer.read_source(log)?
    };

    let hits: HashSet<usize> = parse_hits(&log);

    let mut by_file: BTreeMap<&str, Vec<&FunctionSite>> = BTreeMap::new();
    for site in &sites {
        by_file.entry(&site.file).or_default().push(site);
    }

    let covered = sites.iter().filter(|s| hits.contains(&s.id)).count();

    let report = match format {
        OutFormat::Text => render_text(&by_file, &hits, covered, sites.len()),
        OutFormat::Lcov => render_lcov(&by_file, &hits),
    };

    write_report(out, &report)?;

    // Zero hits across every site means the instrumented build never ran or
    // its output was not captured — a broken pipeline rather than 0%
    // coverage.
    if covered == 0 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "no coverage hits in the log — did the instrumented build run and was its output captured?",
        ));
    }

    Ok(())
}

/// The default `--out-format text`: one row per file plus a `TOTAL` summary
/// line.
fn render_text(
    by_file: &BTreeMap<&str, Vec<&FunctionSite>>,
    hits: &HashSet<usize>,
    covered: usize,
    total: usize,
) -> String {
    let mut table = Table::new();
    table
        .load_style(comfy_table::presets::UTF8_FULL.with_rounded_corners())
        .set_header(vec!["FILE", "COVERED", "MISSED"]);

    for (file, file_sites) in by_file {
        let hit = file_sites.iter().filter(|s| hits.contains(&s.id)).count();
        let missed: Vec<&str> = file_sites
            .iter()
            .filter(|s| !hits.contains(&s.id))
            .map(|s| s.name.as_str())
            .collect();

        // One function per line rather than space-joined, so a file with a
        // dozen missed functions doesn't run the row off the screen.
        table.add_row(vec![
            (*file).to_string(),
            format!("{hit}/{}", file_sites.len()),
            missed.join("\n"),
        ]);
    }

    format!(
        "{table}\n\nTOTAL {covered}/{total} functions executed ({:.0}%)\n",
        100.0 * covered as f64 / total as f64
    )
}

/// The `--out-format lcov` report: an LCOV `.info` file, understood by
/// `genhtml`, VS Code's Coverage Gutters, Codecov and Coveralls. Coverage
/// here is function-level only (see the crate-level docs on
/// `monkey_c_coverage`), which `FN`/`FNDA` records capture directly without
/// needing per-line `DA` data the instrumentation never collected.
fn render_lcov(by_file: &BTreeMap<&str, Vec<&FunctionSite>>, hits: &HashSet<usize>) -> String {
    let mut report = String::new();

    for (file, file_sites) in by_file {
        report.push_str(&format!("SF:{file}\n"));

        for site in file_sites {
            report.push_str(&format!("FN:{},{}\n", site.line, site.name));
        }

        let mut hit_count = 0;
        for site in file_sites {
            let hit = usize::from(hits.contains(&site.id));
            hit_count += hit;
            report.push_str(&format!("FNDA:{hit},{}\n", site.name));
        }

        report.push_str(&format!("FNF:{}\n", file_sites.len()));
        report.push_str(&format!("FNH:{hit_count}\n"));
        report.push_str("end_of_record\n");
    }

    report
}

/// Write a rendered report to `out`, or stdout when no path was given.
fn write_report(out: Option<&Path>, report: &str) -> io::Result<()> {
    match out {
        Some(path) => write(path, report),
        None => {
            print!("{report}");
            Ok(())
        }
    }
}

fn write(path: &Path, contents: &str) -> io::Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .map_err(|e| io::Error::new(e.kind(), format!("{}: {e}", parent.display())))?;
    }

    fs::write(path, contents)
        .map_err(|e| io::Error::new(e.kind(), format!("{}: {e}", path.display())))
}

/// Map an input path (already canonicalized by [`collect_mc_files`]) to the
/// relative path its instrumented copy takes under the output directory,
/// anchored at `root`. Anchoring first (rather than just dropping `.`/`..`
/// components from whatever path the caller passed) is what actually
/// prevents collisions: `shared/A.mc` and `../shared/A.mc` are the same file
/// relative to the working directory but different ones relative to `root`,
/// so they land at distinct destinations instead of one overwriting the
/// other. Anything outside `root` — a path traversal a user shouldn't be
/// passing — falls back to mirroring the absolute path so nothing escapes
/// the output directory.
fn mirrored_path(path: &Path, root: &Path) -> PathBuf {
    let relative = path.strip_prefix(root).unwrap_or(path);

    let mut safe = PathBuf::new();
    for component in relative.components() {
        if let Component::Normal(part) = component {
            safe.push(part);
        }
    }

    safe
}

/// A relative path from directory `from` to `to`, both absolute: walk up to
/// their common ancestor with `..`, then back down. Used to repoint
/// `project.manifest` in the generated jungle, since `--out` can sit at any
/// depth under the project root.
fn relative_path(from: &Path, to: &Path) -> PathBuf {
    let from: Vec<_> = from.components().collect();
    let to: Vec<_> = to.components().collect();
    let common = from.iter().zip(&to).take_while(|(a, b)| a == b).count();

    let mut relative = PathBuf::new();
    for _ in &from[common..] {
        relative.push("..");
    }

    for component in &to[common..] {
        relative.push(component);
    }

    relative
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn render_lcov_emits_one_record_per_file() {
        let hit = FunctionSite {
            id: 0,
            file: "source/A.mc".into(),
            name: "A.hit".into(),
            line: 2,
        };
        let missed = FunctionSite {
            id: 1,
            file: "source/A.mc".into(),
            name: "A.missed".into(),
            line: 5,
        };
        let sites = [&hit, &missed];
        let mut by_file: BTreeMap<&str, Vec<&FunctionSite>> = BTreeMap::new();
        by_file.insert("source/A.mc", sites.to_vec());
        let hits = HashSet::from([0]);

        let report = render_lcov(&by_file, &hits);

        assert_eq!(
            report,
            "SF:source/A.mc\n\
             FN:2,A.hit\n\
             FN:5,A.missed\n\
             FNDA:1,A.hit\n\
             FNDA:0,A.missed\n\
             FNF:2\n\
             FNH:1\n\
             end_of_record\n"
        );
    }

    #[test]
    fn mirrored_path_strips_the_project_root() {
        let root = Path::new("/project");
        assert_eq!(
            mirrored_path(Path::new("/project/source/A.mc"), root),
            PathBuf::from("source/A.mc")
        );
    }

    #[test]
    fn mirrored_path_falls_back_to_the_absolute_path_outside_root() {
        let root = Path::new("/project");
        assert_eq!(
            mirrored_path(Path::new("/other/A.mc"), root),
            PathBuf::from("other/A.mc")
        );
    }

    #[test]
    fn relative_path_walks_up_to_the_common_ancestor() {
        assert_eq!(
            relative_path(
                Path::new("/project/bin/coverage"),
                Path::new("/project/manifest.xml")
            ),
            PathBuf::from("../../manifest.xml")
        );
    }

    #[test]
    fn a_summary_reporting_failures_is_recognised() {
        assert!(summary_failed("FAILED (passed=0, failed=1, errors=0)"));
        assert!(summary_failed("FAILED (passed=0, failed=0, errors=1)"));
        assert!(!summary_failed("PASSED (passed=1, failed=0, errors=0)"));
    }

    #[test]
    fn test_summary_finds_the_verdict_line_among_other_output() {
        let stdout = "some noise\nPASSED (passed=2, failed=0, errors=0)\nmore noise\n";
        assert_eq!(
            test_summary(stdout),
            Some("PASSED (passed=2, failed=0, errors=0)")
        );
    }
}
