//! Individual lint rules. Each rule walks the AST and pushes any findings
//! into a `Vec<Diagnostic>` provided by the top-level [`crate::lint`] driver.
pub mod compound_assignment;
pub mod ifs_same_cond;
pub mod import_order;
pub mod naming_convention;
pub mod one_class_per_file;
pub mod redundant_resource_ref;
pub mod super_initializer_call;
pub mod unneeded_parens;
