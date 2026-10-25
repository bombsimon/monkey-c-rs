//! Parser for Monkey C, the language of Garmin's Connect IQ apps.
//!
//! [`parser::Parser`] turns source text into an [`ast::Ast`] plus a table of the comments in it.
//! Comments are kept out of the tree so tools that don't care about them never see them, while
//! the formatter and linter attach them by position.
//!
//! ```
//! use monkey_c_parser::parser::Parser;
//!
//! let output = Parser::new("var x = 1; // one").parse().unwrap();
//! assert_eq!(output.comments.comments.len(), 1);
//! ```
//!
//! <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/>

pub mod ast;
pub mod comments;
pub mod lexer;
pub mod line_index;
pub mod parser;
pub mod token;
