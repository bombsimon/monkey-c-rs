use monkey_c_jungle::ast::{JungleFile, Value};

fn render(source: &str) -> String {
    JungleFile::parse(source).expect("should parse").to_string()
}

/// Source already written the way `monkeyc` projects write it, which must come back byte for byte.
const UNCHANGED: &[(&str, &str)] = &[
    ("assignment", "project.manifest = manifest.xml\n"),
    (
        "value list",
        "base.resourcePath = resources/drawables;resources/menus\n",
    ),
    (
        "dereference",
        "fenix5.resourcePath = $(fenix5.resourcePath);fenix-resources\n",
    ),
    (
        "group",
        "base.barrelPath = barrels/Math.barrel;[icons/round.jungle;icons/rect.jungle]\n",
    ),
    (
        "windows separators and globs",
        "base.sourcePath = .\\**.mc\n",
    ),
    (
        "quoting kept even where unneeded",
        "d2bravo.personality = ../resources;\"../devices/d2bravo\"\n",
    ),
    (
        "comments and blank lines",
        "# Set the base source path\nbase.sourcePath = source\n\n# Wearables\nround.sourcePath = $(round.sourcePath);wearable-source\n",
    ),
    ("leading blank line", "\na = b\n"),
    // A comment is content, so it breaks a run of blank lines and both sides survive.
    (
        "blank lines split by a comment",
        "a = b\n\n# note\n\nc = d\n",
    ),
    // Parsed comments keep the author's spacing, so banners and tight `#note` survive.
    ("comment spacing", "#not spaced\n#### banner ####\na = b\n"),
    ("comment after a value", "base.sourcePath = source # note\n"),
    // A comment on the last value eats the terminator, so the printer owes a blank line.
    (
        "annotated instruction, then entry",
        "a = b # note\n\nc = d\n",
    ),
    (
        "annotated instruction, then comment",
        "a = b # note\n\n# standalone\nc = d\n",
    ),
    ("comment on a middle value", "a = b; # note\n  c\nd = e\n"),
    (
        "annotated list",
        "base.sourcePath = $(round.sourcePath); # Round devices\n  wearable-source # Everything else\n",
    ),
    (
        "several comments on one value",
        "a = b; # one\n  # two\n  c\n",
    ),
    // A group lays its contents out the same way. Dropping these silently was a bug.
    ("comment inside a group", "a = [b; # note\n  c]\n"),
    // A comment on the group's last value ate the break, so `]` has to start a new line.
    ("comment on a group's last value", "a = [b;c # note\n]\n"),
    // Only a comment absorbs the break after a `;`, so a value without one stays on its line.
    // Breaking after every value once any was annotated emitted output `monkeyc` rejected.
    ("unannotated value, last annotated", "a = b;c # note\n"),
    ("unannotated value, then a break", "a = b;c; # note\n  d\n"),
    (
        "break, then two unannotated",
        "a = b; # note\n  c;d # other\n",
    ),
];

/// Input the printer deliberately rewrites, and what it rewrites to.
const NORMALISED: &[(&str, &str, &str)] = &[
    ("spacing inside a line", "a.b=c ;  d", "a.b = c;d\n"),
    ("missing final newline", "a = b", "a = b\n"),
    ("carriage returns", "a = b\r\nc = d\r\n", "a = b\nc = d\n"),
    // The AST doesn't model continuations, so a split instruction folds back onto one line.
    ("continuation", "a = b;\\\n  c\n", "a = b;c\n"),
    (
        "value list starting below the `=`",
        "base.sourcePath =\n  source\n",
        "base.sourcePath = source\n",
    ),
    // A comment belongs to its value and follows it, even when the instruction folds up.
    (
        "comment before its value",
        "base.sourcePath = # note\n  source\n",
        "base.sourcePath = source # note\n",
    ),
    // One blank line separates; a run says nothing more.
    (
        "run of blank lines",
        "a = b\n\n\n\nc = d\n",
        "a = b\n\nc = d\n",
    ),
    ("trailing run", "a = b\n\n\n", "a = b\n\n"),
    ("leading run", "\n\n\na = b\n", "\na = b\n"),
];

#[test]
fn canonical_source_round_trips() {
    for (name, source) in UNCHANGED {
        assert_eq!(render(source).as_str(), *source, "{name}");
    }
}

#[test]
fn other_source_is_normalised() {
    for (name, source, expected) in NORMALISED {
        assert_eq!(render(source).as_str(), *expected, "{name}");
    }
}

/// Whatever the printer rewrote has reached its final shape, so a file can't drift each time a
/// tool touches it.
#[test]
fn rendering_reaches_a_fixed_point() {
    for (name, source, _) in NORMALISED {
        let once = render(source);

        assert_eq!(render(&once), once, "{name}");
    }
}

/// The whole construction API in one file: comments, blank lines, literals, dereferences, groups,
/// per-value notes, and text that has to be quoted.
#[test]
fn a_file_can_be_built_from_nothing() {
    let mut jungle = JungleFile::default();

    jungle.push_comment("Sources");
    jungle.set("base.sourcePath", [Value::text("my sources/app.mc")]);
    jungle.push_blank_line();

    jungle.set(
        "base.barrelPath",
        [
            Value::reference("base.barrelPath"),
            Value::group([Value::text("a.jungle"), Value::text("b.jungle")]),
        ],
    );
    jungle.set(
        "round.resourcePath",
        [
            Value::reference("round.resourcePath").with_comment("Round devices"),
            Value::text("resources-round"),
        ],
    );

    assert_eq!(
        jungle.to_string(),
        concat!(
            "# Sources\n",
            "base.sourcePath = \"my sources/app.mc\"\n",
            "\n",
            "base.barrelPath = $(base.barrelPath);[a.jungle;b.jungle]\n",
            "round.resourcePath = $(round.resourcePath); # Round devices\n",
            "  resources-round\n",
        ),
    );
}

/// `Comment::new` owns the spacing after the `#`, so callers pass content rather than formatting.
/// Text that already carries its own spacing is left as written.
#[test]
fn constructed_comments_space_themselves() {
    let mut jungle = JungleFile::default();

    jungle.push_comment("Sources");
    jungle.push_comment("  wider on purpose");
    jungle.push_comment("");

    assert_eq!(jungle.to_string(), "# Sources\n#  wider on purpose\n#\n");
}

#[test]
fn an_empty_file_renders_as_nothing() {
    assert_eq!(JungleFile::default().to_string(), "");
}
