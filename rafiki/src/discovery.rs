//! Turning the paths on the command line into a list of files to work on.

use ignore::WalkBuilder;

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

/// The extension a directory walk looks for. An explicitly named file is taken
/// as-is, so an unusual extension can always be reached by naming it.
const SOURCE_EXTENSION: &str = "mc";

/// Resolve `paths` into a deterministic, de-duplicated list of source files, leaving out the ones
/// `excluded` matches. It's given each path and whether it's a directory, and a walk never enters
/// an excluded directory.
///
/// Named files are accepted whatever their extension; directories are walked
/// recursively for `.mc` files, skipping hidden entries and — unless turned off
/// — anything `.gitignore` excludes. Excludes apply to named files too, since
/// editors and pre-commit hooks name every file they hand over.
pub fn collect<F>(
    paths: &[PathBuf],
    respect_gitignore: bool,
    excluded: F,
) -> io::Result<Vec<PathBuf>>
where
    F: Fn(&Path, bool) -> bool + Clone + Send + Sync + 'static,
{
    let mut files = Vec::new();

    for path in paths {
        let metadata = fs::metadata(path)
            .map_err(|e| io::Error::new(e.kind(), format!("{}: {e}", path.display())))?;

        if metadata.is_file() {
            if !excluded(path, false) {
                files.push(path.clone());
            }
        } else if metadata.is_dir() {
            walk(path, respect_gitignore, excluded.clone(), &mut files)?;
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
    respect_gitignore: bool,
    excluded: impl Fn(&Path, bool) -> bool + Send + Sync + 'static,
    out: &mut Vec<PathBuf>,
) -> io::Result<()> {
    let mut builder = WalkBuilder::new(dir);

    // `standard_filters` bundles the gitignore family together with hidden-file
    // skipping, so re-enable `hidden` afterwards: `--no-respect-gitignore` asks
    // for ignored files, not for `.git/` and friends.
    builder.standard_filters(respect_gitignore);
    builder.hidden(true);

    // Without this, ignore files are honoured only inside a git repository,
    // which would make behaviour depend on whether the project is checked in.
    builder.require_git(false);

    builder.filter_entry(move |entry| {
        let is_dir = entry
            .file_type()
            .is_some_and(|file_type| file_type.is_dir());

        !excluded(entry.path(), is_dir)
    });

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

#[cfg(test)]
mod tests {
    use super::*;
    use monkey_c_config::PathMatcher;

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

    fn nothing_excluded(_: &Path, _: bool) -> bool {
        false
    }

    fn excluding(
        root: &Path,
        glob: &str,
    ) -> impl Fn(&Path, bool) -> bool + Clone + Send + Sync + use<> {
        let matcher =
            PathMatcher::new(root, &[glob.to_string()], "files.exclude", None).expect("globs");

        move |path, is_dir| matcher.is_match(path, is_dir)
    }

    #[test]
    fn a_walk_finds_source_files_and_skips_other_extensions() {
        let dir = tree();
        let files = collect(&[dir.path().to_path_buf()], false, nothing_excluded).expect("walks");

        assert_eq!(names(&files), vec!["Generated.mc", "Main.mc"]);
    }

    #[test]
    fn gitignored_files_are_skipped_by_default() {
        let dir = tree();
        let files = collect(&[dir.path().to_path_buf()], true, nothing_excluded).expect("walks");

        assert_eq!(names(&files), vec!["Main.mc"]);
    }

    #[test]
    fn hidden_directories_are_skipped_even_without_gitignore() {
        let dir = tree();
        let files = collect(&[dir.path().to_path_buf()], false, nothing_excluded).expect("walks");

        assert!(!names(&files).contains(&"Hidden.mc".to_string()));
    }

    #[test]
    fn excluded_files_are_skipped() {
        let dir = tree();
        let files = collect(
            &[dir.path().to_path_buf()],
            false,
            excluding(dir.path(), "src/**"),
        )
        .expect("walks");

        assert_eq!(names(&files), vec!["Generated.mc"]);
    }

    #[test]
    fn excludes_relative_to_a_parent_apply_when_walking_a_subdirectory() {
        let dir = tree();
        let root = dir.path().join("src/..");
        let files = collect(
            &[dir.path().join("src")],
            false,
            excluding(&root, "src/Main.mc"),
        )
        .expect("walks");

        assert!(files.is_empty());
    }

    #[test]
    fn an_excluded_directory_is_not_entered() {
        let dir = tree();
        let nested = dir.path().join("build/nested");
        fs::create_dir(&nested).expect("create dir");

        let entered = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
        let seen = entered.clone();
        let files = collect(
            &[dir.path().to_path_buf()],
            false,
            move |path: &Path, is_dir| {
                seen.lock().expect("lock").push(path.to_path_buf());

                is_dir && path.ends_with("build")
            },
        )
        .expect("walks");

        assert_eq!(names(&files), vec!["Main.mc"]);
        assert!(!entered.lock().expect("lock").contains(&nested));
    }

    #[test]
    fn a_named_file_is_used_whatever_its_extension() {
        let dir = tree();
        let path = dir.path().join("src/notes.txt");
        let files = collect(std::slice::from_ref(&path), true, nothing_excluded).expect("collects");

        assert_eq!(files, vec![path]);
    }

    #[test]
    fn a_named_file_is_used_even_when_gitignored() {
        let dir = tree();
        let path = dir.path().join("build/Generated.mc");
        let files = collect(std::slice::from_ref(&path), true, nothing_excluded).expect("collects");

        assert_eq!(files, vec![path]);
    }

    #[test]
    fn a_named_file_is_skipped_when_excluded() {
        let dir = tree();
        let files = collect(
            &[dir.path().join("build/Generated.mc")],
            true,
            excluding(dir.path(), "build/"),
        )
        .expect("collects");

        assert!(files.is_empty());
    }

    #[test]
    fn results_are_sorted_and_deduplicated() {
        let dir = tree();
        let root = dir.path().to_path_buf();
        let files = collect(&[root.clone(), root], true, nothing_excluded).expect("collects");

        assert_eq!(names(&files), vec!["Main.mc"]);
    }

    #[test]
    fn a_missing_path_is_an_error() {
        let error = collect(&[PathBuf::from("nope/missing.mc")], true, nothing_excluded)
            .expect_err("missing path is reported");

        assert!(error.to_string().contains("missing.mc"), "{error}");
    }
}
