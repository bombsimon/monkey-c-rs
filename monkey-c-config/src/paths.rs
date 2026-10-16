use ignore::gitignore::{Gitignore, GitignoreBuilder};

use std::fs;
use std::path::{self, Component, Path, PathBuf};

use crate::{Error, ResolvedConfig};

/// Globs in `.gitignore` syntax, relative to a directory. Paths are compared as absolute paths, so
/// one given relative to the working directory matches the same way as one an editor sends.
#[derive(Debug, Clone)]
pub struct PathMatcher {
    root: PathBuf,
    canonical_root: PathBuf,
    gitignore: Gitignore,
}

impl Default for PathMatcher {
    fn default() -> Self {
        Self {
            root: PathBuf::new(),
            canonical_root: PathBuf::new(),
            gitignore: Gitignore::empty(),
        }
    }
}

impl PathMatcher {
    /// `setting` and `file` say where `patterns` came from, to name in an error: `file` is the
    /// `rafiki.toml` they're from, or `None` for a flag.
    pub fn new(
        root: &Path,
        patterns: &[String],
        setting: &'static str,
        file: Option<&Path>,
    ) -> Result<Self, Error> {
        let (matcher, errors) = Self::lenient(root, patterns, setting, file);

        match errors.into_iter().next() {
            Some(error) => Err(error),
            None => Ok(matcher),
        }
    }

    /// A matcher of the valid `patterns`, along with an error for each invalid one.
    pub fn lenient(
        root: &Path,
        patterns: &[String],
        setting: &'static str,
        file: Option<&Path>,
    ) -> (Self, Vec<Error>) {
        let normalized = normalize(root);
        let mut builder = GitignoreBuilder::new(&normalized);
        let mut errors = Vec::new();
        let invalid = |pattern: String, source| Error::Glob {
            setting,
            path: file.map(Path::to_path_buf),
            pattern,
            source: Box::new(source),
        };

        for pattern in patterns {
            if let Err(source) = builder.add_line(None, pattern) {
                errors.push(invalid(pattern.clone(), source));
            }
        }

        // Each pattern is already checked as it's added, so this only fails when the set as a
        // whole is too large to compile, which no single pattern is to blame for.
        let gitignore = builder.build().unwrap_or_else(|source| {
            errors.push(invalid(patterns.join(", "), source));

            Gitignore::empty()
        });

        let canonical_root = fs::canonicalize(root).unwrap_or_else(|_| normalized.clone());
        let matcher = Self {
            root: normalized,
            canonical_root,
            gitignore,
        };

        (matcher, errors)
    }

    /// Whether `path`, or a directory it's in, matches. A path outside the root never does.
    pub fn is_match(&self, path: &Path, is_dir: bool) -> bool {
        self.matches(&normalize(path), is_dir)
    }

    /// Like [`Self::is_match`], for a path that's already normalized.
    fn matches(&self, path: &Path, is_dir: bool) -> bool {
        if self.gitignore.is_empty() {
            return false;
        }

        // Symlinks are only resolved for a path outside the root as written, so a walk under the
        // root never pays for it, while an editor sending the other spelling still matches.
        let canonical;
        let relative = match path.strip_prefix(&self.root) {
            Ok(relative) => relative,
            Err(_) => {
                let Ok(resolved) = fs::canonicalize(path) else {
                    return false;
                };

                canonical = resolved;

                let Ok(relative) = canonical.strip_prefix(&self.canonical_root) else {
                    return false;
                };

                relative
            }
        };

        // Like git, a file in an excluded directory can't be brought back with a `!` pattern.
        // Checking the file first would let a named file through that a walk never reaches.
        let in_excluded_directory = relative
            .ancestors()
            .skip(1)
            .filter(|dir| !dir.as_os_str().is_empty())
            .any(|dir| self.gitignore.matched(dir, true).is_ignore());

        in_excluded_directory || self.gitignore.matched(relative, is_dir).is_ignore()
    }
}

/// `path` made absolute with `.` and `..` resolved, without touching the filesystem.
fn normalize(path: &Path) -> PathBuf {
    let absolute = path::absolute(path).unwrap_or_else(|_| path.to_path_buf());
    let mut normalized = PathBuf::new();

    for component in absolute.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                normalized.pop();
            }
            component => normalized.push(component),
        }
    }

    normalized
}

/// The settings in a `rafiki.toml` that depend on which file is checked, with their globs resolved
/// against the directory of that `rafiki.toml`.
#[derive(Debug, Clone, Default)]
pub struct PathRules {
    files_exclude: PathMatcher,
    format_exclude: PathMatcher,
    lint_exclude: PathMatcher,
    per_file_ignores: Vec<(PathMatcher, Vec<String>)>,
}

/// `files_exclude` is built by the caller, since `--exclude` on the command line replaces the
/// file's `[files]` excludes and resolves against another directory.
impl PathRules {
    pub fn new(resolved: &ResolvedConfig, files_exclude: PathMatcher) -> Result<Self, Error> {
        let (rules, errors) = Self::lenient(resolved, files_exclude);

        match errors.into_iter().next() {
            Some(error) => Err(error),
            None => Ok(rules),
        }
    }

    /// The rules from every valid glob, along with an error for each invalid one. One typo then
    /// only loses its own pattern, so an editor doesn't start formatting generated files over it.
    pub fn lenient(resolved: &ResolvedConfig, files_exclude: PathMatcher) -> (Self, Vec<Error>) {
        let config = &resolved.config;
        let directory = resolved.directory();
        let mut errors = Vec::new();
        let mut matcher = |patterns: &[String], setting| {
            let (matcher, mut invalid) =
                PathMatcher::lenient(&directory, patterns, setting, resolved.path.as_deref());
            errors.append(&mut invalid);

            matcher
        };

        let format_exclude = matcher(
            config.format.exclude.as_deref().unwrap_or_default(),
            "format.exclude",
        );
        let lint_exclude = matcher(
            config.lint.exclude.as_deref().unwrap_or_default(),
            "lint.exclude",
        );

        let per_file_ignores = config
            .lint
            .per_file_ignores
            .iter()
            .flatten()
            .map(|(pattern, rules)| {
                let matcher = matcher(std::slice::from_ref(pattern), "lint.per-file-ignores");

                (matcher, rules.clone())
            })
            .collect();

        let rules = Self {
            files_exclude,
            format_exclude,
            lint_exclude,
            per_file_ignores,
        };

        (rules, errors)
    }

    pub fn formats(&self, path: &Path, is_dir: bool) -> bool {
        let path = normalize(path);

        !self.files_exclude.matches(&path, is_dir) && !self.format_exclude.matches(&path, is_dir)
    }

    pub fn lints(&self, path: &Path, is_dir: bool) -> bool {
        let path = normalize(path);

        !self.files_exclude.matches(&path, is_dir) && !self.lint_exclude.matches(&path, is_dir)
    }

    /// The rules `[lint.per-file-ignores]` turns off for the file at `path`.
    pub fn ignored_rules(&self, path: &Path) -> Vec<&str> {
        let path = normalize(path);

        self.per_file_ignores
            .iter()
            .filter(|(matcher, _)| matcher.matches(&path, false))
            .flat_map(|(_, rules)| rules.iter().map(String::as_str))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::FilesSettings;

    #[cfg(unix)]
    use std::os::unix;

    fn patterns(globs: &[&str]) -> Vec<String> {
        globs.iter().map(|glob| glob.to_string()).collect()
    }

    fn matcher(root: &Path, globs: &[&str]) -> PathMatcher {
        PathMatcher::new(root, &patterns(globs), "files.exclude", None).expect("globs")
    }

    /// `text` as if read from `/project/rafiki.toml`.
    fn resolved(text: &str) -> ResolvedConfig {
        ResolvedConfig {
            config: toml::from_str(text).expect("config parses"),
            path: Some(PathBuf::from("/project/rafiki.toml")),
        }
    }

    fn file_matches(matcher: &PathMatcher, path: &str) -> bool {
        matcher.is_match(Path::new(path), false)
    }

    #[test]
    fn matches_relative_to_the_root() {
        let matcher = matcher(Path::new("/project"), &["gen/**"]);

        assert!(file_matches(&matcher, "/project/gen/A.mc"));
        assert!(!file_matches(&matcher, "/project/src/gen/A.mc"));
        assert!(!file_matches(&matcher, "/elsewhere/gen/A.mc"));
    }

    #[test]
    fn a_directory_pattern_matches_the_files_in_it() {
        let matcher = matcher(Path::new("/project"), &["vendor/"]);

        assert!(matcher.is_match(Path::new("/project/vendor"), true));
        assert!(!matcher.is_match(Path::new("/project/vendor"), false));
        assert!(file_matches(&matcher, "/project/vendor/lib/A.mc"));
    }

    #[test]
    fn a_negated_pattern_keeps_a_file() {
        let matcher = matcher(Path::new("/project"), &["gen/*", "!gen/Keep.mc"]);

        assert!(file_matches(&matcher, "/project/gen/A.mc"));
        assert!(!file_matches(&matcher, "/project/gen/Keep.mc"));
    }

    #[test]
    fn relative_paths_resolve_against_the_working_directory() {
        let matcher = matcher(Path::new("."), &["gen/**"]);

        assert!(file_matches(&matcher, "gen/A.mc"));
        assert!(file_matches(&matcher, "./gen/A.mc"));
    }

    #[test]
    fn parent_directories_in_either_path_are_resolved() {
        let matcher = matcher(Path::new("/project/src/.."), &["gen/**"]);

        assert!(file_matches(&matcher, "/project/gen/A.mc"));
        assert!(file_matches(&matcher, "/project/src/../gen/A.mc"));
    }

    #[cfg(unix)]
    #[test]
    fn a_root_behind_a_symlink_matches_either_spelling() {
        let dir = tempfile::tempdir().expect("temp dir");
        let real = dir.path().join("real");
        let link = dir.path().join("link");
        fs::create_dir(&real).expect("create dir");
        unix::fs::symlink(&real, &link).expect("symlink");

        let real = fs::canonicalize(&real).expect("canonical");
        fs::create_dir(real.join("gen")).expect("create gen");
        fs::write(real.join("gen/A.mc"), "").expect("write source");

        for root in [&link, &real] {
            let matcher = matcher(root, &["gen/**"]);

            assert!(matcher.is_match(&link.join("gen/A.mc"), false), "{root:?}");
            assert!(matcher.is_match(&real.join("gen/A.mc"), false), "{root:?}");
        }
    }

    #[test]
    fn a_file_in_an_excluded_directory_cannot_be_brought_back() {
        let matcher = matcher(Path::new("/project"), &["gen/", "!gen/Keep.mc"]);

        assert!(file_matches(&matcher, "/project/gen/Keep.mc"));
    }

    #[test]
    fn an_empty_matcher_matches_nothing() {
        assert!(!file_matches(&PathMatcher::default(), "A.mc"));
    }

    #[test]
    fn an_invalid_glob_is_an_error() {
        let error = PathMatcher::new(
            Path::new("."),
            &patterns(&["a{b"]),
            "format.exclude",
            Some(Path::new("/project/rafiki.toml")),
        )
        .expect_err("rejected");

        assert!(
            error
                .to_string()
                .starts_with("/project/rafiki.toml: invalid glob `a{b` in `format.exclude`: "),
            "{error}"
        );
    }

    #[test]
    fn a_lenient_matcher_keeps_the_valid_globs() {
        let (matcher, errors) = PathMatcher::lenient(
            Path::new("/project"),
            &patterns(&["a{b", "gen/**"]),
            "--exclude",
            None,
        );

        assert_eq!(errors.len(), 1);
        assert!(errors[0].to_string().contains("`a{b`"), "{}", errors[0]);
        assert!(file_matches(&matcher, "/project/gen/A.mc"));
    }

    /// The rules in `text` for a `rafiki.toml` in `/project`, with `[files]` excludes resolved the
    /// way a caller without `--exclude` does.
    fn rules(text: &str) -> PathRules {
        let resolved = resolved(text);
        let files = FilesSettings::default().with_overrides(&resolved.config.files);
        let files_exclude =
            PathMatcher::new(Path::new("/project"), &files.exclude, "files.exclude", None)
                .expect("globs");

        PathRules::new(&resolved, files_exclude).expect("resolves")
    }

    #[test]
    fn excludes_are_per_command() {
        let rules = rules(
            r#"
[files]
exclude = ["bin/**"]

[format]
exclude = ["gen/**"]

[lint]
exclude = ["tests/**"]
"#,
        );

        let formats = |path: &str| rules.formats(Path::new(path), false);
        let lints = |path: &str| rules.lints(Path::new(path), false);

        assert!(!formats("/project/bin/A.mc") && !lints("/project/bin/A.mc"));
        assert!(!formats("/project/gen/A.mc") && lints("/project/gen/A.mc"));
        assert!(formats("/project/tests/A.mc") && !lints("/project/tests/A.mc"));
        assert!(formats("/project/src/A.mc") && lints("/project/src/A.mc"));
    }

    #[test]
    fn per_file_ignores_turn_rules_off_for_matching_files() {
        let rules = rules(
            r#"
[lint.per-file-ignores]
"tests/**" = ["naming-convention"]
"tests/Strings.mc" = ["one-class-per-file"]
"#,
        );

        assert_eq!(
            rules.ignored_rules(Path::new("/project/tests/Strings.mc")),
            vec!["naming-convention", "one-class-per-file"]
        );
        assert!(
            rules
                .ignored_rules(Path::new("/project/src/A.mc"))
                .is_empty()
        );
    }

    #[test]
    fn a_lenient_rule_set_only_loses_the_invalid_glob() {
        let (rules, errors) = PathRules::lenient(
            &resolved(
                r#"
[format]
exclude = ["gen/**"]

[lint.per-file-ignores]
"a{b" = ["naming-convention"]
"#,
            ),
            PathMatcher::default(),
        );

        assert_eq!(errors.len(), 1);
        assert!(
            errors[0]
                .to_string()
                .contains("rafiki.toml: invalid glob `a{b` in `lint.per-file-ignores`"),
            "{}",
            errors[0]
        );
        assert!(!rules.formats(Path::new("/project/gen/A.mc"), false));
    }
}
