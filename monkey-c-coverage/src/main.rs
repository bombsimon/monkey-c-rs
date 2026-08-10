use clap::{Parser, Subcommand};
use monkey_c_coverage::{
    FunctionSite, coverage_jungle, instrument, manifest_line, parse_hits, parse_manifest_line,
    runtime_module,
};
use monkey_c_diagnostics::{already_reported, read_source, read_stdin_source, render_parse_error};

use std::collections::{BTreeMap, HashSet};
use std::env;
use std::fs;
use std::io::{self, Write};
use std::path::{Component, Path, PathBuf};
use std::process::{ExitCode, Output};
use std::time::Duration;

const MANIFEST_FILE: &str = "coverage-manifest.tsv";

#[derive(Parser)]
#[command(version, about = "Test-coverage instrumentation for Monkey C")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Rewrite sources with coverage probes into an output directory.
    Instrument {
        /// Directory that receives the instrumented copies. Cleared on each
        /// run so stale instrumented files never leak into a build.
        /// Defaults to `{repo_root}/bin/coverage`.
        #[arg(long)]
        out: Option<PathBuf>,
        /// Additional annotation names (beyond `test` and `release`) whose
        /// declarations should be skipped, e.g. `--exclude-annotation foo,bar`
        /// or repeated `--exclude-annotation foo --exclude-annotation bar`.
        #[arg(long, value_delimiter = ',')]
        exclude_annotation: Vec<String>,
        /// Jungle file describing the project, copied and rewritten into
        /// `{out}/coverage.jungle` so the instrumented build can be compiled
        /// with `monkeyc -f`. Defaults to `{repo_root}/monkey.jungle`.
        #[arg(long)]
        jungle: Option<PathBuf>,
        /// Monkey C source files or directories to instrument. Defaults to
        /// the whole project (the nearest ancestor holding `manifest.xml`),
        /// so the tool can be run from any subdirectory of it.
        files: Vec<PathBuf>,
    },
    /// Print per-file coverage from a captured simulator log.
    Report {
        /// Directory produced by `instrument`, holding coverage-manifest.tsv.
        /// Defaults to `{repo_root}/bin/coverage`.
        #[arg(long)]
        dir: Option<PathBuf>,
        /// Captured simulator output containing COVHIT lines (e.g. from
        /// monkeydo -t). Pass `-` to read from stdin, e.g.
        /// `monkeydo … -t | monkey-c-coverage report -`.
        log: PathBuf,
    },
    /// Instrument, build, run under the simulator, and report in one step.
    Test {
        /// Device to build and run for, e.g. `fr965`. Passed to `monkeyc -d`
        /// and `monkeydo`.
        #[arg(short = 'd', long)]
        device: String,
        /// Developer key used to sign the build. Passed to `monkeyc -y`.
        #[arg(short = 'y', long)]
        key: PathBuf,
        /// Directory that receives the instrumented copies. Cleared on each
        /// run so stale instrumented files never leak into a build.
        /// Defaults to `{repo_root}/bin/coverage`.
        #[arg(long)]
        out: Option<PathBuf>,
        /// Additional annotation names (beyond `test` and `release`) whose
        /// declarations should be skipped, e.g. `--exclude-annotation foo,bar`
        /// or repeated `--exclude-annotation foo --exclude-annotation bar`.
        #[arg(long, value_delimiter = ',')]
        exclude_annotation: Vec<String>,
        /// Jungle file describing the project, copied and rewritten into
        /// `{out}/coverage.jungle` so the instrumented build can be compiled
        /// with `monkeyc -f`. Defaults to `{repo_root}/monkey.jungle`.
        #[arg(long)]
        jungle: Option<PathBuf>,
        /// Print the monkeyc/monkeydo commands this would run, without
        /// running them.
        #[arg(long)]
        dry_run: bool,
        /// If the first `monkeydo` attempt produces no coverage hits, launch
        /// the simulator with `connectiq` (installed alongside
        /// `monkeyc`/`monkeydo`) and retry once. Left off by default because
        /// `connectiq` brings an already-running simulator's window to the
        /// front, which is only worth doing when `monkeydo` actually needed
        /// it.
        #[arg(long)]
        start_simulator: bool,
        /// Seconds to wait for the simulator to come up before retrying.
        /// Only used with `--start-simulator`.
        #[arg(long, default_value_t = 5)]
        simulator_boot_time: u64,
        /// Monkey C source files or directories to instrument. Defaults to
        /// the whole project (the nearest ancestor holding `manifest.xml`),
        /// so the tool can be run from any subdirectory of it.
        files: Vec<PathBuf>,
        /// Extra arguments forwarded to `monkeyc` verbatim, after `--`,
        /// e.g. `-- -O 3 -w`.
        #[arg(last = true)]
        monkeyc_args: Vec<String>,
    },
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    match run(&cli.command) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            // Parse and invalid-UTF-8 errors are already rendered via ariadne
            // by `render_parse_error` / `read_source`.
            if error.kind() != io::ErrorKind::Other {
                eprintln!("{error}");
            }

            ExitCode::from(1)
        }
    }
}

fn run(command: &Command) -> io::Result<()> {
    match command {
        Command::Instrument {
            out,
            exclude_annotation,
            jungle,
            files,
        } => run_instrument(out.as_deref(), exclude_annotation, jungle.as_deref(), files).map(drop),
        Command::Report { dir, log } => run_report(dir.as_deref(), log),
        Command::Test {
            device,
            key,
            out,
            exclude_annotation,
            jungle,
            dry_run,
            start_simulator,
            simulator_boot_time,
            files,
            monkeyc_args,
        } => run_test(&TestOptions {
            device,
            key,
            out: out.as_deref(),
            exclude_annotation,
            jungle: jungle.as_deref(),
            files,
            monkeyc_args,
            dry_run: *dry_run,
            start_simulator: *start_simulator,
            simulator_boot_time: *simulator_boot_time,
        }),
    }
}

/// Arguments for [`run_test`], grouped to keep the function signature
/// readable — see [`Command::Test`] for what each one means.
struct TestOptions<'a> {
    device: &'a str,
    key: &'a Path,
    out: Option<&'a Path>,
    exclude_annotation: &'a [String],
    jungle: Option<&'a Path>,
    files: &'a [PathBuf],
    monkeyc_args: &'a [String],
    dry_run: bool,
    start_simulator: bool,
    simulator_boot_time: u64,
}

/// Instrument, compile, run under the simulator, and report — see
/// [`Command::Test`]. `device` and `key` are forwarded to `monkeyc`/`monkeydo`
/// verbatim; everything else is the same as `instrument`, reused here so the
/// compile and run steps see exactly the paths `instrument` just wrote.
fn run_test(options: &TestOptions) -> io::Result<()> {
    let out = run_instrument(
        options.out,
        options.exclude_annotation,
        options.jungle,
        options.files,
    )?;

    let jungle_path = out.join("coverage.jungle");
    let binary_path = out.join("cov.prg");
    let log_path = out.join("run.log");

    let mut monkeyc_command = vec![
        "monkeyc".to_string(),
        "-f".to_string(),
        jungle_path.display().to_string(),
        "-d".to_string(),
        options.device.to_string(),
        "-o".to_string(),
        binary_path.display().to_string(),
        "-y".to_string(),
        options.key.display().to_string(),
        "--unit-test".to_string(),
    ];
    monkeyc_command.extend(options.monkeyc_args.iter().cloned());

    let monkeydo_command = [
        "monkeydo".to_string(),
        binary_path.display().to_string(),
        options.device.to_string(),
        "-t".to_string(),
    ];

    if options.dry_run {
        eprintln!("{}", monkeyc_command.join(" "));
        eprintln!("{}", monkeydo_command.join(" "));
        if options.start_simulator {
            eprintln!("connectiq  # only if the monkeydo attempt above produces no coverage hits");
        }

        return Ok(());
    }

    run_monkeyc(&monkeyc_command)?;

    let mut monkeydo_output = run_captured(&monkeydo_command)?;
    let mut stdout = String::from_utf8_lossy(&monkeydo_output.stdout).into_owned();
    let mut hits = parse_hits(&stdout);

    if hits.is_empty() && options.start_simulator {
        eprintln!(
            "monkeydo produced no coverage hits; starting the simulator with `connectiq` and retrying once"
        );

        start_simulator(Duration::from_secs(options.simulator_boot_time))?;
        monkeydo_output = run_captured(&monkeydo_command)?;
        stdout = String::from_utf8_lossy(&monkeydo_output.stdout).into_owned();
        hits = parse_hits(&stdout);
    }

    if hits.is_empty() {
        // Nothing ran at all — even after a retry, if `--start-simulator`
        // was given. A "0/N covered" table would dress this up as a real
        // (if terrible) coverage result, when actually the build never
        // executed; show what monkeydo said instead and stop here, same as
        // a `monkeyc` failure, rather than handing it to `report`.
        println!();
        print_captured(&monkeydo_output);
        println!();

        return Err(already_reported());
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
        // Something's wrong: show everything monkeydo printed to help debug
        // — `summary` is part of this, so it isn't also printed on its own.
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

    run_report(Some(&out), &log_path)?;

    if failed {
        // Already shown via `print_captured` above.
        return Err(already_reported());
    }

    Ok(())
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

        return Err(already_reported());
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

fn run_instrument(
    out: Option<&Path>,
    exclude_annotation: &[String],
    jungle: Option<&Path>,
    files: &[PathBuf],
) -> io::Result<PathBuf> {
    let exclude_annotation: Vec<&str> = exclude_annotation.iter().map(String::as_str).collect();
    let root = project_root()?;
    let out = out.map_or_else(|| root.join("bin/coverage"), Path::to_path_buf);
    let jungle = jungle.map_or_else(|| root.join("monkey.jungle"), Path::to_path_buf);

    // A previous run's files would otherwise be recycled into the next
    // build (and report) even after their sources were renamed or removed.
    if out.exists() {
        fs::remove_dir_all(&out)?;
    }

    let default_files = [root.clone()];
    let files = if files.is_empty() {
        &default_files[..]
    } else {
        files
    };

    // The compiler and Monkey C Optimizer both write build artifacts under
    // `{root}/bin` (our own `--out` default lives there too), so a
    // whole-project walk must never descend into it.
    let files = collect_mc_files(&out, &root.join("bin"), files)?;
    if files.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "no input files",
        ));
    }

    let mut manifest = String::new();
    let mut next_id = 0;

    for file in &files {
        let source = read_source(file)?;
        let dest = mirrored_path(file, &root);
        let name = dest.to_string_lossy().into_owned();
        let result = instrument(&source, &name, next_id, &exclude_annotation).map_err(|e| {
            render_parse_error(&name, &source, &e);
            already_reported()
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

    let jungle_source = read_source(&jungle)?;
    // `manifest.xml` isn't copied into `out` the way `.mc` sources are, so the
    // copied jungle needs a path back to wherever it actually lives.
    let manifest_path = relative_path(&out, &root.join("manifest.xml"));
    let coverage_jungle_source = coverage_jungle(&jungle_source, &manifest_path.to_string_lossy())
        .map_err(|e| {
            render_parse_error(&jungle.display().to_string(), &jungle_source, &e);
            already_reported()
        })?;

    write(&out.join("coverage.jungle"), &coverage_jungle_source)?;

    eprintln!(
        "instrumented {next_id} function bodies across {} files",
        files.len()
    );

    Ok(out)
}

/// The Connect IQ project root: the nearest ancestor holding `manifest.xml`.
/// Everything anchors here rather than the working directory, so the mirrored
/// tree, `--out` and the manifest paths come out the same wherever the tool is
/// invoked from.
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

/// Resolve `paths` into a deterministic, deduplicated list of `.mc` files.
/// Files are accepted as-is regardless of extension (so explicit per-file
/// invocations always work); directories are walked recursively, skipping
/// hidden ones and `exclude_dir` (the compiler and Monkey C Optimizer write
/// build artifacts there, and our own `out` default lives under it). Every
/// path is canonicalized before being collected, so overlapping inputs (an
/// explicit file plus a directory containing it, or `source/A.mc` alongside
/// `./source/A.mc`) dedupe to one entry instead of instrumenting the same
/// file twice under different ids. Copied from `monkey-c-formatter` until it
/// grows a shared home.
fn collect_mc_files(out: &Path, exclude_dir: &Path, paths: &[PathBuf]) -> io::Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    for p in paths {
        // Always ignore any files in our output directory, they're generated files.
        if p == out {
            continue;
        }

        let meta = fs::metadata(p)
            .map_err(|e| io::Error::new(e.kind(), format!("{}: {e}", p.display())))?;

        if meta.is_file() {
            files.push(fs::canonicalize(p)?);
        } else if meta.is_dir() {
            walk_dir(&fs::canonicalize(p)?, exclude_dir, &mut files)?;
        } else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("{}: not a file or directory", p.display()),
            ));
        }
    }

    files.sort();
    files.dedup();

    Ok(files)
}

fn walk_dir(dir: &Path, exclude_dir: &Path, out: &mut Vec<PathBuf>) -> io::Result<()> {
    if dir == exclude_dir {
        return Ok(());
    }

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        let file_type = entry.file_type()?;

        let name = entry.file_name();
        let name = name.to_string_lossy();
        if name.starts_with('.') {
            continue;
        }

        if file_type.is_dir() {
            walk_dir(&path, exclude_dir, out)?;
        } else if file_type.is_file() && path.extension().is_some_and(|e| e == "mc") {
            out.push(fs::canonicalize(&path)?);
        }
    }

    Ok(())
}

fn run_report(dir: Option<&Path>, log: &Path) -> io::Result<()> {
    let dir = match dir {
        Some(dir) => dir.to_path_buf(),
        None => project_root()?.join("bin/coverage"),
    };

    let sites: Vec<FunctionSite> = read_source(&dir.join(MANIFEST_FILE))?
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

    let log = if log == Path::new("-") {
        read_stdin_source()?
    } else {
        read_source(log)?
    };

    let hits: HashSet<usize> = parse_hits(&log);

    let mut by_file: BTreeMap<&str, Vec<&FunctionSite>> = BTreeMap::new();
    for site in &sites {
        by_file.entry(&site.file).or_default().push(site);
    }

    println!("{:<36} {:>9}  missed", "file", "covered");

    let mut total = 0;
    let mut covered = 0;

    for (file, file_sites) in &by_file {
        let hit = file_sites.iter().filter(|s| hits.contains(&s.id)).count();
        total += file_sites.len();
        covered += hit;
        let missed: Vec<&str> = file_sites
            .iter()
            .filter(|s| !hits.contains(&s.id))
            .map(|s| s.name.as_str())
            .collect();

        println!(
            "{:<36} {:>4}/{:<4}  {}",
            file,
            hit,
            file_sites.len(),
            missed.join(" ")
        );
    }

    if total == 0 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "manifest contains no reportable sites",
        ));
    }

    println!(
        "\nTOTAL {covered}/{total} functions executed ({:.0}%)",
        100.0 * covered as f64 / total as f64
    );

    // Zero hits across every site means the instrumented build never ran or its
    // output was not captured — a broken pipeline rather than 0% coverage.
    if covered == 0 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "no coverage hits in the log — did the instrumented build run and was its output captured?",
        ));
    }

    Ok(())
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
