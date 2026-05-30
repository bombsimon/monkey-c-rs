//! Individual lint rules. Each rule walks the AST and pushes any findings
//! into a `Vec<Diagnostic>` provided by the top-level [`crate::lint`] driver.
pub mod compound_assignment;
pub mod import_order;
pub mod unneeded_parens;
