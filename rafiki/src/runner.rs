//! The per-target loop shared by `fmt` and `lint`.
//!
//! Both commands accept the same mix of files, directories and `-`, and both
//! need one failing file not to abort the rest of the run, so that plumbing
//! lives here and the commands only say what to do with one target.

use monkey_c_config::FilesSettings;

use std::io;
use std::path::{Path, PathBuf};

use crate::diagnostics;
use crate::discovery;

/// One thing to process. Stdin is distinct from a file because it can neither be
/// written back in place nor named in a diagnostic.
#[derive(Debug, Clone, Copy)]
pub enum Target<'a> {
    Stdin,
    File(&'a Path),
}

impl Target<'_> {
    /// How this target is named in diagnostics.
    pub fn label(&self) -> String {
        match self {
            Self::Stdin => "<stdin>".to_string(),
            Self::File(path) => path.display().to_string(),
        }
    }
}

/// Apply `action` to every target named by `paths`, returning whether all of
/// them were clean.
///
/// A target that fails outright is reported and marks the run unclean, but does
/// not stop the remaining targets — a single unreadable file should not hide
/// the findings in every other one.
pub fn each_target(
    paths: &[PathBuf],
    files: &FilesSettings,
    exclude_root: &Path,
    mut action: impl FnMut(Target<'_>) -> io::Result<bool>,
) -> io::Result<bool> {
    if paths.iter().any(|path| path == Path::new("-")) {
        if paths.len() != 1 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "cannot mix `-` (stdin) with file or directory paths",
            ));
        }

        return action(Target::Stdin);
    }

    let targets = discovery::collect(paths, files, exclude_root)?;
    let mut all_clean = true;

    for target in &targets {
        match action(Target::File(target)) {
            Ok(true) => {}
            Ok(false) => all_clean = false,
            Err(error) => {
                if !diagnostics::is_already_reported(&error) {
                    eprintln!("{}: {error}", target.display());
                }

                all_clean = false;
            }
        }
    }

    Ok(all_clean)
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::cell::RefCell;
    use std::fs::{self, File};

    #[test]
    fn stdin_cannot_be_mixed_with_paths() {
        let paths = vec![PathBuf::from("-"), PathBuf::from("src")];
        let error = each_target(&paths, &FilesSettings::default(), Path::new("."), |_| {
            Ok(true)
        })
        .expect_err("mixing is rejected");

        assert_eq!(error.kind(), io::ErrorKind::InvalidInput);
    }

    #[test]
    fn a_lone_dash_runs_once_against_stdin() {
        let seen = RefCell::new(Vec::new());
        let clean = each_target(
            &[PathBuf::from("-")],
            &FilesSettings::default(),
            Path::new("."),
            |target| {
                seen.borrow_mut().push(target.label());

                Ok(true)
            },
        )
        .expect("runs");

        assert!(clean);
        assert_eq!(*seen.borrow(), vec!["<stdin>"]);
    }

    #[test]
    fn one_failing_target_does_not_stop_the_others() {
        let dir = tempfile::tempdir().expect("temp dir");
        File::create(dir.path().join("A.mc")).expect("create A");
        File::create(dir.path().join("B.mc")).expect("create B");

        let seen = RefCell::new(Vec::new());
        let clean = each_target(
            &[dir.path().to_path_buf()],
            &FilesSettings::default(),
            dir.path(),
            |target| {
                let label = target.label();
                seen.borrow_mut().push(label.clone());

                if label.ends_with("A.mc") {
                    return Err(io::Error::other("boom"));
                }

                Ok(true)
            },
        )
        .expect("runs");

        assert!(!clean, "a failing target makes the run unclean");
        assert_eq!(seen.borrow().len(), 2, "both targets were visited");
    }

    #[test]
    fn an_unclean_target_makes_the_run_unclean() {
        let dir = tempfile::tempdir().expect("temp dir");
        File::create(dir.path().join("A.mc")).expect("create A");

        let clean = each_target(
            &[dir.path().to_path_buf()],
            &FilesSettings::default(),
            dir.path(),
            |_| Ok(false),
        )
        .expect("runs");

        assert!(!clean);
    }

    #[test]
    fn a_missing_path_aborts_before_any_action() {
        let seen = RefCell::new(0);
        each_target(
            &[PathBuf::from("nope/missing.mc")],
            &FilesSettings::default(),
            Path::new("."),
            |_| {
                *seen.borrow_mut() += 1;

                Ok(true)
            },
        )
        .expect_err("missing path is reported");

        assert_eq!(*seen.borrow(), 0);
    }

    #[test]
    fn a_directory_with_no_sources_is_clean() {
        let dir = tempfile::tempdir().expect("temp dir");
        fs::create_dir(dir.path().join("empty")).expect("create dir");

        let clean = each_target(
            &[dir.path().to_path_buf()],
            &FilesSettings::default(),
            dir.path(),
            |_| Ok(false),
        )
        .expect("runs");

        assert!(clean, "nothing to check means nothing to report");
    }
}
