//! Snapshot tests over realistic jungle files.
//!
//! The snapshot body is the file as `Display` writes it back, so the diff between a fixture in
//! `tests/inputs/` and its snapshot *is* the assertion about what parsing and printing changed —
//! nothing for a file already written the way `monkeyc` projects write them, the rewrite for one
//! that isn't. Byte-identity on specific constructs is pinned in `display.rs` instead.
//!
//! See <https://insta.rs/> for the snapshot tooling.
use monkey_c_jungle::ast::JungleFile;

fn render(source: &str) -> String {
    JungleFile::parse(source).expect("should parse").to_string()
}

#[test]
fn render_inputs() {
    insta::glob!("inputs/*.jungle", |path| {
        let source = std::fs::read_to_string(path).expect("read input file");

        insta::with_settings!({ omit_expression => true }, {
            insta::assert_snapshot!(render(&source));
        });
    });
}

/// Whatever a fixture's snapshot shows is final: re-parsing it and printing again is a no-op, so a
/// file can't drift a little further each time a tool touches it.
#[test]
fn rendering_reaches_a_fixed_point() {
    insta::glob!("inputs/*.jungle", |path| {
        let source = std::fs::read_to_string(path).expect("read input file");
        let once = render(&source);

        assert_eq!(render(&once), once, "{} keeps changing", path.display());
    });
}
