use clap::{Parser as ClapParser, Subcommand};
use monkey_c_coverage::{
    FunctionSite, instrument, manifest_line, parse_hits, parse_manifest_line, runtime_module,
};

use std::collections::{BTreeMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

#[derive(ClapParser)]
#[command(
    name = "monkey-c-coverage",
    about = "Test-coverage instrumentation for Monkey C"
)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Rewrite sources with coverage probes into an output directory,
    /// alongside the generated Cov.mc runtime and the site manifest.
    Instrument {
        /// Directory that receives the instrumented copies.
        #[arg(long)]
        out: PathBuf,
        /// Monkey C source files to instrument.
        files: Vec<PathBuf>,
    },
    /// Join a captured simulator log against the manifest and print
    /// per-file coverage.
    Report {
        /// Manifest written by `instrument` (coverage-manifest.tsv).
        #[arg(long)]
        manifest: PathBuf,
        /// Simulator output containing COVHIT lines (e.g. from monkeydo -t).
        #[arg(long)]
        log: PathBuf,
        /// Exclude files matching this suffix from the report denominator;
        /// pass e.g. "Test.mc" so test code does not count itself.
        #[arg(long)]
        exclude_suffix: Option<String>,
    },
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    match run(&cli.command) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("{error}");
            ExitCode::from(1)
        }
    }
}

fn run(command: &Command) -> Result<(), String> {
    match command {
        Command::Instrument { out, files } => run_instrument(out, files),
        Command::Report {
            manifest,
            log,
            exclude_suffix,
        } => run_report(manifest, log, exclude_suffix.as_deref()),
    }
}

fn run_instrument(out: &Path, files: &[PathBuf]) -> Result<(), String> {
    if files.is_empty() {
        return Err("no input files".to_string());
    }
    fs::create_dir_all(out).map_err(|e| format!("{}: {e}", out.display()))?;

    let mut manifest = String::new();
    let mut next_id = 0;
    for file in files {
        let source = read(file)?;
        let name = file_name(file)?;
        let result = instrument(&source, &name, next_id).map_err(|e| {
            format!(
                "{}: parse error at {}:{}: {}",
                name, e.line, e.col, e.message
            )
        })?;
        next_id += result.sites.len();
        for site in &result.sites {
            manifest.push_str(&manifest_line(site));
            manifest.push('\n');
        }
        write(&out.join(&name), &result.source)?;
    }
    write(&out.join("Cov.mc"), &runtime_module())?;
    write(&out.join("coverage-manifest.tsv"), &manifest)?;
    eprintln!(
        "instrumented {} function bodies across {} files",
        next_id,
        files.len()
    );
    Ok(())
}

fn run_report(manifest: &Path, log: &Path, exclude_suffix: Option<&str>) -> Result<(), String> {
    let sites: Vec<FunctionSite> = read(manifest)?
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| parse_manifest_line(line).ok_or_else(|| format!("bad manifest line: {line}")))
        .collect::<Result<_, _>>()?;
    let hits: HashSet<usize> = parse_hits(&read(log)?);

    let mut by_file: BTreeMap<&str, Vec<&FunctionSite>> = BTreeMap::new();
    for site in &sites {
        if let Some(suffix) = exclude_suffix
            && site.file.ends_with(suffix)
        {
            continue;
        }
        by_file.entry(&site.file).or_default().push(site);
    }

    let mut total = 0;
    let mut covered = 0;
    println!("{:<36} {:>9}  missed", "file", "covered");
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
        return Err("manifest contains no reportable sites".to_string());
    }
    println!(
        "\nTOTAL {covered}/{total} functions executed ({:.0}%)",
        100.0 * covered as f64 / total as f64
    );
    Ok(())
}

fn read(path: &Path) -> Result<String, String> {
    fs::read_to_string(path).map_err(|e| format!("{}: {e}", path.display()))
}

fn write(path: &Path, contents: &str) -> Result<(), String> {
    fs::write(path, contents).map_err(|e| format!("{}: {e}", path.display()))
}

fn file_name(path: &Path) -> Result<String, String> {
    path.file_name()
        .and_then(|name| name.to_str())
        .map(str::to_string)
        .ok_or_else(|| format!("{}: not a valid file name", path.display()))
}
