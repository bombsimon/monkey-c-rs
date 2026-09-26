//! Keeps the rule documentation in sync with the rules the linter has, so a rule
//! can't be added, renamed or removed without the docs following.
//!
//! Run with `UPDATE_DOCS=1` to regenerate the rules table instead of failing.

use monkey_c_linter::rules::{self, FixAvailability};

use std::collections::BTreeSet;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

const BEGIN: &str = "<!-- begin rules -->";
const END: &str = "<!-- end rules -->";

fn docs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../docs/src")
}

fn rules_dir() -> PathBuf {
    docs_dir().join("linter/rules")
}

fn fix_label(fix: FixAvailability) -> &'static str {
    match fix {
        FixAvailability::Always => "Yes",
        FixAvailability::Sometimes => "Mostly",
        FixAvailability::Never => "No",
    }
}

/// A Markdown table with every column padded to its widest cell.
fn markdown_table(header: [&str; 3], rows: &[[String; 3]]) -> String {
    let widths: Vec<usize> = (0..3)
        .map(|column| {
            rows.iter()
                .map(|row| row[column].chars().count())
                .chain([header[column].chars().count()])
                .max()
                .unwrap_or(0)
        })
        .collect();

    let line = |cells: [&str; 3]| {
        let padded: Vec<String> = cells
            .iter()
            .zip(&widths)
            .map(|(cell, width)| format!("{cell:width$}"))
            .collect();

        format!("| {} |\n", padded.join(" | "))
    };

    let separator: Vec<String> = widths.iter().map(|width| "-".repeat(*width)).collect();
    let mut table = line(header);
    table.push_str(&format!("| {} |\n", separator.join(" | ")));

    for row in rows {
        table.push_str(&line([&row[0], &row[1], &row[2]]));
    }

    table
}

fn rules_table() -> String {
    let rows: Vec<[String; 3]> = rules::ALL
        .iter()
        .map(|rule| {
            [
                format!("[`{name}`](./{name})", name = rule.name),
                rule.summary.to_string(),
                fix_label(rule.fix).to_string(),
            ]
        })
        .collect();

    markdown_table(["Rule", "Flags", "Fix"], &rows)
}

#[test]
fn rules_table_is_up_to_date() {
    let path = rules_dir().join("README.md");
    let page = fs::read_to_string(&path).expect("read the rules page");

    let start = page.find(BEGIN).expect("rules page has a begin marker") + BEGIN.len();
    let end = page.find(END).expect("rules page has an end marker");
    let expected = format!("\n{}", rules_table());

    if page[start..end] == expected {
        return;
    }

    if env::var_os("UPDATE_DOCS").is_some() {
        let updated = format!("{}{expected}{}", &page[..start], &page[end..]);
        fs::write(&path, updated).expect("write the rules page");

        return;
    }

    panic!(
        "the rules table in {} is out of date, run `UPDATE_DOCS=1 cargo test -p \
         monkey-c-linter --test rule_docs` to regenerate it\n\nexpected:\n{expected}",
        path.display()
    );
}

#[test]
fn every_rule_has_exactly_one_page() {
    let rules: BTreeSet<&str> = rules::ALL.iter().map(|rule| rule.name).collect();
    let pages: BTreeSet<String> = fs::read_dir(rules_dir())
        .expect("read the rules directory")
        .filter_map(Result::ok)
        .filter(|entry| entry.path().join("README.md").is_file())
        .map(|entry| entry.file_name().to_string_lossy().into_owned())
        .collect();
    let pages: BTreeSet<&str> = pages.iter().map(String::as_str).collect();

    let missing: Vec<_> = rules.difference(&pages).collect();
    let unknown: Vec<_> = pages.difference(&rules).collect();

    assert!(missing.is_empty(), "rules without a page: {missing:?}");
    assert!(
        unknown.is_empty(),
        "pages for rules that don't exist: {unknown:?}"
    );
}

#[test]
fn every_rule_is_in_the_summary() {
    let summary = fs::read_to_string(docs_dir().join("SUMMARY.md")).expect("read SUMMARY.md");

    let missing: Vec<&str> = rules::ALL
        .iter()
        .map(|rule| rule.name)
        .filter(|name| !summary.contains(&format!("(linter/rules/{name}/README.md)")))
        .collect();

    assert!(
        missing.is_empty(),
        "rules missing from SUMMARY.md: {missing:?}"
    );
}
