//! The settings page opens with every setting at its default, which goes stale as soon as a
//! default changes or a key is added. This keeps it honest.

use monkey_c_config::{
    Config, FilesConfig, FilesSettings, FormatConfig, FormatSettings, LintConfig, LintSettings,
    NumbersConfig,
};

use std::fs;
use std::path::Path;

/// The first `toml` block on the settings page.
fn documented_defaults() -> Config {
    let path =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("../docs/src/configuration/settings/README.md");
    let page = fs::read_to_string(&path).expect("settings page exists");

    let (_, rest) = page
        .split_once("```toml\n")
        .expect("the page has a toml block");
    let (block, _) = rest.split_once("```").expect("the toml block is closed");

    toml::from_str(block).expect("the documented defaults parse")
}

#[test]
fn every_setting_is_documented_at_its_default() {
    let Config {
        format,
        lint,
        files,
    } = documented_defaults();

    // Destructured without `..`, so a new key fails to compile here until it's documented.
    let FormatConfig {
        line_width,
        alignment,
        short_array_element_width,
        union_style,
        nullable,
        annotation_separator,
        resource_ref,
        modifier_order,
        parens,
        sort_imports,
        exclude: format_exclude,
        numbers,
    } = format.clone();
    let NumbersConfig {
        hex_prefix_case,
        hex_digit_case,
        exponent_case,
        exponent_padding,
        float_suffix_case,
        double_suffix_case,
        long_suffix_case,
        leading_zero,
        trailing_zeros,
    } = numbers;
    let LintConfig {
        enable,
        disable,
        exclude: lint_exclude,
        per_file_ignores,
    } = lint.clone();
    let FilesConfig {
        exclude: files_exclude,
        respect_gitignore,
    } = files.clone();

    assert!(line_width.is_some() && alignment.is_some() && short_array_element_width.is_some());
    assert!(union_style.is_some() && nullable.is_some() && annotation_separator.is_some());
    assert!(resource_ref.is_some() && modifier_order.is_some() && parens.is_some());
    assert!(sort_imports.is_some());
    assert!(hex_prefix_case.is_some() && hex_digit_case.is_some() && exponent_case.is_some());
    assert!(exponent_padding.is_some() && float_suffix_case.is_some());
    assert!(double_suffix_case.is_some() && long_suffix_case.is_some());
    assert!(leading_zero.is_some() && trailing_zeros.is_some());
    assert!(enable.is_some() && disable.is_some() && respect_gitignore.is_some());
    assert_eq!(format_exclude, Some(Vec::new()));
    assert_eq!(lint_exclude, Some(Vec::new()));
    assert_eq!(files_exclude, Some(Vec::new()));
    assert_eq!(per_file_ignores, Some(Default::default()));

    assert_eq!(
        FormatSettings::default().with_overrides(&format),
        FormatSettings::default()
    );
    assert_eq!(
        LintSettings::default().with_overrides(&lint),
        LintSettings::default()
    );
    assert_eq!(
        FilesSettings::default().with_overrides(&files),
        FilesSettings::default()
    );
}
