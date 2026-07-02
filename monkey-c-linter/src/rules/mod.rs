//! Individual lint rules. Each rule walks the AST and pushes any findings
//! into a `Vec<Diagnostic>` provided by the top-level [`crate::lint`] driver.
pub mod collapsible_if;
pub mod compound_assignment;
pub mod ifs_same_cond;
pub mod import_order;
pub mod naming_convention;
pub mod one_class_per_file;
pub mod redundant_resource_ref;
pub mod super_initializer_call;
pub mod unneeded_parens;

/// Every rule's stable identifier, in alphabetical order. The single source of
/// truth for the `--enable`/`--disable` CLI flags and their help listing.
pub const ALL: &[&str] = &[
    collapsible_if::RULE_ELSE_IF,
    collapsible_if::RULE_IF,
    compound_assignment::RULE,
    ifs_same_cond::RULE,
    import_order::RULE,
    naming_convention::RULE,
    one_class_per_file::RULE,
    redundant_resource_ref::RULE,
    super_initializer_call::RULE,
    unneeded_parens::RULE,
];
