use monkey_c_jungle::ast::{Entry, JungleFile, LiteralPart, Value, ValueKind};
use monkey_c_jungle::parser::ParserError;

fn parse(source: &str) -> JungleFile {
    JungleFile::parse(source).expect("should parse")
}

fn parse_err(source: &str) -> ParserError {
    JungleFile::parse(source).expect_err("should fail")
}

/// Render an assignment's values, which is enough to assert on structure without spelling out
/// spans in every case.
fn values(source: &str, target: &str) -> Vec<String> {
    parse(source)
        .get(target)
        .expect("target not found")
        .values
        .iter()
        .map(Value::to_string)
        .collect()
}

fn comments(source: &str, target: &str) -> Vec<Vec<String>> {
    parse(source)
        .get(target)
        .expect("target not found")
        .values
        .iter()
        .map(|value| value.comments.iter().map(|c| c.text.clone()).collect())
        .collect()
}

/// One realistic file exercising every shape the grammar has, rather than a case each.
const SAMPLE: &str = "\
# Build instructions for a fictional app.
project.manifest = manifest.xml

base.sourcePath = source
base.resourcePath = resources/drawables;resources/menus
base.barrelPath = barrels/Math.barrel;[icons/round.jungle;icons/rect.jungle]

# Fenix devices share a resource folder.
fenix5.resourcePath = $(fenix5.resourcePath);fenix-resources
fenix5.lang.eng = resources-eng
fenix5.sourcePath = \"my projects/file.mc\";source

round-360x360 = $(round)
base.personality = $(base.personality)/extra
";

#[test]
fn parses_a_whole_file() {
    let jungle = parse(SAMPLE);

    assert_eq!(jungle.assignments().count(), 9);
    assert_eq!(jungle.to_string(), SAMPLE, "sample should round-trip");

    assert!(matches!(jungle.entries[0], Entry::Comment(_)));
    assert!(matches!(jungle.entries[2], Entry::BlankLine));
}

#[test]
fn names_split_into_qualifier_and_property() {
    let jungle = parse(SAMPLE);

    for (target, qualifier, property) in [
        ("project.manifest", "project", &["manifest"][..]),
        ("fenix5.lang.eng", "fenix5", &["lang", "eng"]),
        ("round-360x360", "round-360x360", &[]),
    ] {
        let name = &jungle.get(target).expect("target not found").target;

        assert_eq!(name.qualifier(), qualifier, "{target}");
        assert_eq!(name.property(), property, "{target}");
    }
}

#[test]
fn value_lists_split_on_semicolons() {
    for (target, expected) in [
        ("base.sourcePath", &["source"][..]),
        (
            "base.resourcePath",
            &["resources/drawables", "resources/menus"],
        ),
        (
            "fenix5.resourcePath",
            &["$(fenix5.resourcePath)", "fenix-resources"],
        ),
        ("fenix5.sourcePath", &["\"my projects/file.mc\"", "source"]),
        (
            "base.barrelPath",
            &[
                "barrels/Math.barrel",
                "[icons/round.jungle;icons/rect.jungle]",
            ],
        ),
        // Only a `;` starts a new value, so a dereference and the text glued to it are one.
        ("base.personality", &["$(base.personality)/extra"]),
    ] {
        assert_eq!(values(SAMPLE, target), expected, "{target}");
    }
}

#[test]
fn value_kinds() {
    let jungle = parse(SAMPLE);

    let ValueKind::Literal(parts) = &jungle.get("base.personality").unwrap().values[0].kind else {
        panic!("expected a literal value");
    };
    assert!(matches!(parts[0], LiteralPart::Reference(_)));
    assert!(matches!(parts[1], LiteralPart::Text(_)));

    let ValueKind::Group(group) = &jungle.get("base.barrelPath").unwrap().values[1].kind else {
        panic!("expected a group value");
    };
    assert_eq!(group.len(), 2);
}

#[test]
fn spans_point_back_at_the_source() {
    let source = "a = b\n";
    let jungle = parse(source);
    let assignment = jungle.get("a").expect("target not found");

    assert_eq!(&source[assignment.span.start..assignment.span.end], "a = b");
}

#[test]
fn empty_source_has_no_entries() {
    assert!(parse("").entries.is_empty());
}

#[test]
fn parses_through_str_parse() {
    let jungle: JungleFile = "a = b\n".parse().expect("should parse");

    assert_eq!(jungle.assignments().count(), 1);
}

/// A comment reaches to the end of its line and takes the break with it, so it can sit inside an
/// instruction. It belongs to the value it was written against; one with no value to attach to
/// stays a line of its own.
#[test]
fn comments_attach_to_their_value() {
    let source =
        "base.sourcePath = $(round.sourcePath); # Round devices\n  wearable # Everything else\n";

    assert_eq!(
        values(source, "base.sourcePath"),
        vec!["$(round.sourcePath)", "wearable"],
    );
    assert_eq!(
        comments(source, "base.sourcePath"),
        vec![vec![" Round devices"], vec![" Everything else"]],
    );

    // Written before the value, it still belongs to it, and is the only entry.
    let inside = "project.manifest = # Foo\n    manifest.xml\n";
    assert_eq!(values(inside, "project.manifest"), vec!["manifest.xml"]);
    assert_eq!(comments(inside, "project.manifest"), vec![vec![" Foo"]]);
    assert_eq!(parse(inside).entries.len(), 1);

    // With nothing to attach to, it becomes an entry.
    assert!(matches!(
        parse("# Sources\nbase.sourcePath = source\n").entries[0],
        Entry::Comment(_),
    ));
}

/// Three things hold a line open, and nothing else does. Checked against `monkeyc`.
#[test]
fn the_three_ways_to_split_a_line() {
    for source in [
        "a =\n  b\n",        // a break after the `=`
        "a =\n\n  b\n",      // and it may be several
        "a = b; #\n  c\n",   // a comment after a `;`, eating its own break
        "a = b;\\\n  c\n",   // a `\` after a `;`
        "a = [b;\\\n  c]\n", // including inside a group
    ] {
        assert!(
            JungleFile::parse(source).is_ok(),
            "should parse: {source:?}"
        );
    }

    assert_eq!(values("a = b;\\\n  c\n", "a"), vec!["b", "c"]);

    for source in [
        "a = b;\n  c\n",     // bare break after a `;`
        "a = b\n  c\n",      // break between values with no `;`
        "a = [b;\n  c]\n",   // break inside a group
        "a = b; #\n\n  c\n", // the comment ate one break, the blank line ends it
        // `monkeyc` accepts a break before a `;`, but that form silently discards the instruction
        // after it, so rejecting turns an invisible mistake into an error.
        "a = b\n  ;c\n",
        // A comment after the values eats the terminator, so the line below runs into this one.
        "a = b # note\nc = d\n",
    ] {
        assert!(
            JungleFile::parse(source).is_err(),
            "should reject: {source:?}"
        );
    }
}

/// A `\` is an ordinary value character unless it starts a token, so `b\` is a value and the break
/// after it still terminates — matching `monkeyc`.
#[test]
fn a_backslash_belongs_to_the_value_it_touches() {
    assert_eq!(values("a = b\\\n", "a"), vec!["b\\"]);
    assert!(JungleFile::parse("a = b\\\n  c\n").is_err());
}

#[test]
fn editing_a_parsed_file() {
    let mut jungle = parse("base.sourcePath = source\na = 1\nb = 2\na = 3\n");

    // The later assignment of a target is the one that wins, so it is the one `get` and `set` see.
    assert_eq!(
        jungle.get("a").expect("target not found").values[0].to_string(),
        "3"
    );

    jungle.set("base.sourcePath", [Value::text("src")]);
    jungle.set("fenix5.excludeAnnotations", [Value::text("experimental")]);

    assert_eq!(jungle.remove("a"), 2);
    assert_eq!(jungle.remove("nothing"), 0);

    assert_eq!(
        jungle.to_string(),
        "base.sourcePath = src\nb = 2\nfenix5.excludeAnnotations = experimental\n",
    );
}

#[test]
fn errors() {
    for (source, fragment) in [
        ("base.sourcePath source", "expected `=`"),
        ("= source", "expected a qualifier name"),
        ("base..sourcePath = source", "empty segment"),
        ("a = $()", "empty segment"),
        ("a = \"oops\n", "closing `\"`"),
        ("a = $(b.c\n", "closing `)`"),
        ("a = [b;c\n", "expected `]`"),
        // `monkeyc` requires a value in every list position.
        ("a = b;;c", "expected a value"),
        ("a = b;", "expected a value"),
        ("a =", "expected a value"),
        ("a =\n", "expected a value"),
        ("a = []", "expected a value"),
    ] {
        let err = parse_err(source);

        assert!(
            err.message.contains(fragment),
            "{source:?} should mention {fragment:?}, got: {}",
            err.message,
        );
    }
}

#[test]
fn errors_point_at_the_offending_line() {
    let err = parse_err("a = b\nc = d\n= e\n");

    assert_eq!((err.line, err.col), (3, 1));
}
