//! Individual lint rules. Each rule walks the AST and pushes any findings
//! into a `Vec<Diagnostic>` provided by the top-level [`crate::lint`] driver.
pub mod bool_comparison;
pub mod collapsible_if;
pub mod compound_assignment;
pub mod ifs_same_cond;
pub mod invalid_directive;
pub mod naming_convention;
pub mod one_class_per_file;
pub mod super_initializer_call;
pub mod unused_ignore;

/// A lint rule as presented to users, in `--list-rules` and in the rule documentation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Rule {
    pub name: &'static str,
    pub summary: &'static str,
    pub fix: FixAvailability,
}

/// Whether a rule's findings come with a fix `--fix` can apply.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FixAvailability {
    Always,
    Sometimes,
    Never,
}

/// Every rule, in alphabetical order. The single source of truth for the
/// `--enable`/`--disable` flags, their help listing and the rules table in the docs.
pub const ALL: &[Rule] = &[
    Rule {
        name: bool_comparison::RULE,
        summary: "Comparing with `true` or `false`",
        fix: FixAvailability::Always,
    },
    Rule {
        name: collapsible_if::RULE_ELSE_IF,
        summary: "An `else` block that only contains an `if`",
        fix: FixAvailability::Always,
    },
    Rule {
        name: collapsible_if::RULE_IF,
        summary: "An `if` that only contains another `if`",
        fix: FixAvailability::Always,
    },
    Rule {
        name: compound_assignment::RULE,
        summary: "`x = x + n` instead of `x += n`",
        fix: FixAvailability::Always,
    },
    Rule {
        name: ifs_same_cond::RULE,
        summary: "Two branches of an `if` chain with the same condition",
        fix: FixAvailability::Never,
    },
    Rule {
        name: invalid_directive::RULE,
        summary: "A `rafiki:` comment that isn't a valid directive",
        fix: FixAvailability::Never,
    },
    Rule {
        name: naming_convention::RULE,
        summary: "Names that don't follow Garmin's coding conventions",
        fix: FixAvailability::Never,
    },
    Rule {
        name: one_class_per_file::RULE,
        summary: "More than one class in a file",
        fix: FixAvailability::Never,
    },
    Rule {
        name: super_initializer_call::RULE,
        summary: "An `initialize` that doesn't call the parent's",
        fix: FixAvailability::Never,
    },
    Rule {
        name: unused_ignore::RULE,
        summary: "A rule in a `rafiki: ignore` comment with nothing to silence",
        fix: FixAvailability::Always,
    },
];

/// Whether `name` is one of the rules in [`ALL`].
pub fn exists(name: &str) -> bool {
    ALL.iter().any(|rule| rule.name == name)
}
