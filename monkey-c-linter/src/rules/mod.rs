//! Individual lint rules. Each rule walks the AST and pushes any findings
//! into a `Vec<Diagnostic>` provided by the top-level [`crate::lint`] driver.
pub mod bool_comparison;
pub mod collapsible_if;
pub mod compound_assignment;
pub mod ifs_same_cond;
pub mod import_order;
pub mod naming_convention;
pub mod one_class_per_file;
pub mod redundant_resource_ref;
pub mod super_initializer_call;
pub mod unneeded_parens;

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
        name: import_order::RULE,
        summary: "Imports that aren't sorted and grouped",
        fix: FixAvailability::Sometimes,
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
        name: redundant_resource_ref::RULE,
        summary: "The legacy `@` before a resource reference",
        fix: FixAvailability::Always,
    },
    Rule {
        name: super_initializer_call::RULE,
        summary: "An `initialize` that doesn't call the parent's",
        fix: FixAvailability::Never,
    },
    Rule {
        name: unneeded_parens::RULE,
        summary: "Parentheses that can't change how the code is read",
        fix: FixAvailability::Always,
    },
];

/// Whether `name` is one of the rules in [`ALL`].
pub fn exists(name: &str) -> bool {
    ALL.iter().any(|rule| rule.name == name)
}
