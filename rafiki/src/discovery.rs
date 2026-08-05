//! Turning the paths on the command line into a list of files to work on.

use ignore::WalkBuilder;
use ignore::overrides::OverrideBuilder;
use monkey_c_config::FilesSettings;

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

/// The extension a directory walk looks for. An explicitly named file is taken
/// as-is, so an unusual extension can always be reached by naming it.
const SOURCE_EXTENSION: &str = "mc";

/// Resolve `paths` into a deterministic, de-duplicated list of source files.
///
/// Named files are accepted whatever their extension; directories are walked
/// recursively for `.mc` files, skipping hidden entries and — unless turned off
/// — anything `.gitignore` excludes.
pub fn collect(
    paths: &[PathBuf],
    settings: &FilesSettings,
    exclude_root: &Path,
) -> io::Result<Vec<PathBuf>> {
    let mut files = Vec::new();

    for path in paths {
        let metadata = fs::metadata(path)
            .map_err(|e| io::Error::new(e.kind(), format!("{}: {e}", path.display())))?;

        if metadata.is_file() {
            files.push(path.clone());
        } else if metadata.is_dir() {
            walk(path, settings, exclude_root, &mut files)?;
        } else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("{}: not a file or directory", path.display()),
            ));
        }
    }

    files.sort();
    files.dedup();

    Ok(files)
}

fn walk(
    dir: &Path,
    settings: &FilesSettings,
    exclude_root: &Path,
    out: &mut Vec<PathBuf>,
) -> io::Result<()> {
    let mut builder = WalkBuilder::new(dir);

    // `standard_filters` bundles the gitignore family together with hidden-file
    // skipping, so re-enable `hidden` afterwards: `--no-respect-gitignore` asks
    // for ignored files, not for `.git/` and friends.
    builder.standard_filters(settings.respect_gitignore);
    builder.hidden(true);

    // Without this, ignore files are honoured only inside a git repository,
    // which would make behaviour depend on whether the project is checked in.
    builder.require_git(false);

    if !settings.exclude.is_empty() {
        builder.overrides(excludes(exclude_root, &settings.exclude)?);
    }

    for entry in builder.build() {
        let entry = entry.map_err(io::Error::other)?;
        let path = entry.path();

        let is_file = entry
            .file_type()
            .is_some_and(|file_type| file_type.is_file());
        if is_file && path.extension().is_some_and(|ext| ext == SOURCE_EXTENSION) {
            out.push(path.to_path_buf());
        }
    }

    Ok(())
}

/// Compile `patterns` into an allowlist-style override where every pattern
/// excludes. `ignore` treats a bare glob as "only match this", so each pattern
/// is negated to mean "skip this".
fn excludes(dir: &Path, patterns: &[String]) -> io::Result<ignore::overrides::Override> {
    let mut builder = OverrideBuilder::new(dir);

    for pattern in patterns {
        builder
            .add(&format!("!{pattern}"))
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, format!("{pattern}: {e}")))?;
    }

    builder.build().map_err(io::Error::other)
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::fs::{self, File};

    /// A directory tree with two source files, one of them ignored by git.
    fn tree() -> tempfile::TempDir {
        let dir = tempfile::tempdir().expect("temp dir");
        let root = dir.path();

        fs::create_dir_all(root.join("src")).expect("create src");
        fs::create_dir_all(root.join("build")).expect("create build");
        fs::create_dir_all(root.join(".hidden")).expect("create hidden dir");

        File::create(root.join("src/Main.mc")).expect("create source");
        File::create(root.join("src/notes.txt")).expect("create text file");
        File::create(root.join("build/Generated.mc")).expect("create generated source");
        File::create(root.join(".hidden/Hidden.mc")).expect("create hidden source");
        fs::write(root.join(".gitignore"), "build/\n").expect("write gitignore");

        dir
    }

    fn names(files: &[PathBuf]) -> Vec<String> {
        files
            .iter()
            .map(|path| {
                path.file_name()
                    .expect("file name")
                    .to_string_lossy()
                    .into_owned()
            })
            .collect()
    }

    #[test]
    fn a_walk_finds_source_files_and_skips_other_extensions() {
        let dir = tree();
        let files = collect(
            &[dir.path().to_path_buf()],
            &FilesSettings {
                respect_gitignore: false,
                ..FilesSettings::default()
            },
            dir.path(),
        )
        .expect("walks");

        assert_eq!(names(&files), vec!["Generated.mc", "Main.mc"]);
    }

    #[test]
    fn gitignored_files_are_skipped_by_default() {
        let dir = tree();
        let files = collect(
            &[dir.path().to_path_buf()],
            &FilesSettings::default(),
            dir.path(),
        )
        .expect("walks");

        assert_eq!(names(&files), vec!["Main.mc"]);
    }

    #[test]
    fn hidden_directories_are_skipped_even_without_gitignore() {
        let dir = tree();
        let files = collect(
            &[dir.path().to_path_buf()],
            &FilesSettings {
                respect_gitignore: false,
                ..FilesSettings::default()
            },
            dir.path(),
        )
        .expect("walks");

        assert!(!names(&files).contains(&"Hidden.mc".to_string()));
    }

    #[test]
    fn exclude_globs_are_applied() {
        let dir = tree();
        let files = collect(
            &[dir.path().to_path_buf()],
            &FilesSettings {
                exclude: vec!["src/**".to_string()],
                respect_gitignore: false,
            },
            dir.path(),
        )
        .expect("walks");

        assert_eq!(names(&files), vec!["Generated.mc"]);
    }

    #[test]
    fn a_named_file_is_used_whatever_its_extension() {
        let dir = tree();
        let path = dir.path().join("src/notes.txt");
        let files = collect(
            std::slice::from_ref(&path),
            &FilesSettings::default(),
            dir.path(),
        )
        .expect("collects");

        assert_eq!(files, vec![path]);
    }

    #[test]
    fn a_named_file_is_used_even_when_gitignored() {
        let dir = tree();
        let path = dir.path().join("build/Generated.mc");
        let files = collect(
            std::slice::from_ref(&path),
            &FilesSettings::default(),
            dir.path(),
        )
        .expect("collects");

        assert_eq!(files, vec![path]);
    }

    #[test]
    fn results_are_sorted_and_deduplicated() {
        let dir = tree();
        let root = dir.path().to_path_buf();
        let files = collect(&[root.clone(), root], &FilesSettings::default(), dir.path())
            .expect("collects");

        assert_eq!(names(&files), vec!["Main.mc"]);
    }

    #[test]
    fn a_missing_path_is_an_error() {
        let error = collect(
            &[PathBuf::from("nope/missing.mc")],
            &FilesSettings::default(),
            Path::new("."),
        )
        .expect_err("missing path is reported");

        assert!(error.to_string().contains("missing.mc"), "{error}");
    }

    #[test]
    fn an_invalid_exclude_glob_is_an_error() {
        let dir = tree();
        collect(
            &[dir.path().to_path_buf()],
            &FilesSettings {
                exclude: vec!["[".to_string()],
                ..FilesSettings::default()
            },
            dir.path(),
        )
        .expect_err("malformed glob is reported");
    }

    #[test]
    fn exclude_globs_are_relative_to_the_config_directory() {
        let dir = tree();
        let files = collect(
            &[dir.path().join("src")],
            &FilesSettings {
                exclude: vec!["src/**".to_string()],
                respect_gitignore: false,
            },
            dir.path(),
        )
        .expect("walks");

        assert!(files.is_empty());
    }
}
