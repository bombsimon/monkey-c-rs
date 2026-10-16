//! Excludes resolved from a `rafiki.toml` above the working directory, which only a real run can
//! show since tests can't safely change the working directory of their own process.

use std::fs;
use std::path::Path;
use std::process::Command;

const UNFORMATTED: &str = "class A {\nvar  x=1;\n}\n";

/// A project whose `rafiki.toml` excludes `src/Table.mc` from formatting, with one other file
/// that needs formatting next to it.
fn project() -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("temp dir");

    fs::create_dir(dir.path().join("src")).expect("create src");
    fs::write(
        dir.path().join("rafiki.toml"),
        "[format]\nexclude = [\"src/Table.mc\"]\n",
    )
    .expect("write config");
    fs::write(dir.path().join("src/A.mc"), UNFORMATTED).expect("write source");
    fs::write(dir.path().join("src/Table.mc"), UNFORMATTED).expect("write source");

    dir
}

/// The files `rafiki fmt --check` reports, run from `cwd` with `args`.
fn unformatted(cwd: &Path, args: &[&str]) -> String {
    let output = Command::new(env!("CARGO_BIN_EXE_rafiki"))
        .current_dir(cwd)
        .args(["fmt", "--check"])
        .args(args)
        .output()
        .expect("rafiki runs");

    String::from_utf8(output.stderr).expect("utf-8")
}

#[test]
fn a_config_above_the_working_directory_is_discovered() {
    let dir = project();
    let reported = unformatted(&dir.path().join("src"), &[]);

    assert!(reported.contains("A.mc"), "{reported}");
    assert!(!reported.contains("Table.mc"), "{reported}");
}

#[test]
fn a_config_named_through_a_parent_directory_resolves_its_excludes() {
    let dir = project();
    let reported = unformatted(
        &dir.path().join("src"),
        &["--config", "../rafiki.toml", "."],
    );

    assert!(reported.contains("A.mc"), "{reported}");
    assert!(!reported.contains("Table.mc"), "{reported}");
}
