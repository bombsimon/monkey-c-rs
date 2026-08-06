//! A parser and printer for [Connect IQ jungle files][jungle-reference], the build language that
//! tells `monkeyc` which sources, resources, barrels and annotations to use per device.
//!
//! ```
//! use monkey_c_jungle::ast::{JungleFile, Value};
//!
//! let mut jungle = JungleFile::parse("# Sources\nbase.sourcePath = source\n").expect("parses");
//!
//! jungle.set(
//!     "fenix5.resourcePath",
//!     vec![Value::reference("fenix5.resourcePath"), Value::text("fenix-resources")],
//! );
//!
//! assert_eq!(
//!     jungle.to_string(),
//!     "# Sources\nbase.sourcePath = source\nfenix5.resourcePath = $(fenix5.resourcePath);fenix-resources\n",
//! );
//! ```
//!
//! Building one from nothing is the same API without the parse — [`ast::JungleFile`] is `Default`,
//! and `to_string()` gives you the bytes to write:
//!
//! ```
//! use monkey_c_jungle::ast::{JungleFile, Value};
//!
//! let mut jungle = JungleFile::default();
//!
//! jungle.push_comment("Only the shared code");
//! jungle.set("base.sourcePath", [Value::text("source")]);
//!
//! assert_eq!(jungle.to_string(), "# Only the shared code\nbase.sourcePath = source\n");
//! ```
//!
//! # Comments end lines, they don't sit on them
//!
//! One rule shapes most of this crate. A jungle comment runs to the end of its line *and takes the
//! line break with it*, and `monkeyc` treats the lot as whitespace. So a comment can never end a
//! build instruction — the instruction carries on below it:
//!
//! ```jungle
//! project.manifest = # Foo
//!     manifest.xml
//! ```
//!
//! That is one instruction. The same rule is what lets a value list annotate its entries, since
//! the comment absorbs the break that would otherwise end things:
//!
//! ```jungle
//! base.sourcePath = source; # shared by everything
//!   wearable-source # everything else
//! ```
//!
//! So a comment belongs to the value it was written against ([`ast::Value::comments`]), and one
//! with no value to attach to is an [`ast::Entry::Comment`] of its own. It also means a comment on
//! the *last* value eats the instruction's terminator, so the printer always follows one with a
//! blank line — otherwise the next line would run into it, which `monkeyc` rejects.
//!
//! [jungle-reference]: https://developer.garmin.com/connect-iq/reference-guides/jungle-reference/

pub mod ast;
mod display;
pub mod lexer;
pub mod parser;
pub mod token;
