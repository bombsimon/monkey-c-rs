# Jungle

The `monkey-c-jungle` crate parses [jungle files][jungle-reference], the build
language that tells `monkeyc` which sources, resources, barrels and annotations
to use for each device.

Jungle is a small, line-oriented language, so the crate is small too: a lexer, a
recursive descent parser and an AST that knows how to write itself back out.

```rust,ignore
use monkey_c_jungle::ast::{JungleFile, Value};

let mut jungle = JungleFile::parse("base.sourcePath = source\n")?;

jungle.set(
    "fenix5.resourcePath",
    vec![
        Value::reference("fenix5.resourcePath"),
        Value::text("fenix-resources"),
    ],
);

print!("{jungle}");
```

## Building a file from scratch

There is no separate builder — a `JungleFile` is `Default`, and the same methods
that edit a parsed file fill an empty one. `to_string()` gives you the bytes.

```rust,ignore
use monkey_c_jungle::ast::{JungleFile, Value};

let mut jungle = JungleFile::default();

jungle.set("project.manifest", [Value::text("manifest.xml")]);
jungle.push_blank_line();

jungle.push_comment("Only the shared sources");
jungle.set("base.sourcePath", [Value::text("source")]);
jungle.push_blank_line();

jungle.push_comment("Older devices lack the newer API");
for device in ["fenix3", "fr230"] {
    jungle.set(
        &format!("{device}.excludeAnnotations"),
        [Value::text("experimental")],
    );
}
jungle.push_blank_line();

// Extend a qualifier rather than replace it, and note why.
jungle.set(
    "round.resourcePath",
    [
        Value::reference("round.resourcePath"),
        Value::text("resources-round").with_comment("shared by every round device"),
    ],
);

std::fs::write("monkey.jungle", jungle.to_string())?;
```

```jungle
project.manifest = manifest.xml

# Only the shared sources
base.sourcePath = source

# Older devices lack the newer API
fenix3.excludeAnnotations = experimental
fr230.excludeAnnotations = experimental

round.resourcePath = $(round.resourcePath);resources-round # shared by every round device
```

The pieces: `Value::text` for a literal, `Value::reference` for a `$(…)`,
`Value::group` for a `[…]`, and `with_comment` to note one. `push_comment` and
`push_blank_line` add the lines between instructions — `push_comment` spaces the
text off the `#` itself, so pass content rather than formatting.

`set` replaces the last instruction assigning a target, or appends one if there
is none, so the same call works whether you are building or editing. Reach for
`remove` to drop a target and `get` to read one back.

## The AST

A `JungleFile` is a flat list of `Entry` in source order: an instruction, a
comment, or a blank line. Keeping the last two is what lets a file be edited and
written back without losing the notes around it.

An instruction is a `QualifiedName` target and a list of `Value`:

| Source                       | AST                                         |
| ---------------------------- | ------------------------------------------- |
| `base`                       | one qualifier segment, no property          |
| `fenix5.lang.eng`            | qualifier `fenix5`, property `lang.eng`     |
| `a;b`                        | two values                                  |
| `$(base.sourcePath)/shared`  | one value, a reference part and a text part |
| `[round.jungle;rect.jungle]` | one `ValueKind::Group` of two values        |
| `"my sources/app.mc"`        | one quoted text value                       |

Only a `;` starts a new value. That is why appending to a path
(`$(fenix5.resourcePath);fenix-resources`) and extending one
(`$(base.sourcePath)/extra`) mean different things. Every position in a list
must hold a value — `monkeyc` rejects a bare `qualifier =`, a trailing `;`, a
gap between two `;` and an empty `[]` alike.

## Line breaks

An instruction ends at a line break. Only three things hold one open, all
verified against `monkeyc`:

```jungle
foo =              # a break after the `=`, before the first value
  bar

foo = bar; #       # a comment after a `;` — it eats its own line break
  baz

foo = bar;\        # a `\` after a `;`
  baz
```

Everything else terminates, including the near-misses: a bare break after a
`;`, a break between two values with no `;`, and any break inside a `[…]`
group. The `\` is picky — it only works directly after a `;`, so `foo = bar \`
and `foo =\` both fail, and `source\` at the end of a line is just the value
`source\`.

Two deliberate differences from `monkeyc`. It allows a break _before_ a `;`
(`source` on one line, `;xx` on the next); this crate rejects that, because the
form is a trap — `monkeyc` accepts it and then silently discards the
instruction that follows, with no error at all. It also allows a break between
a name and its `=`; rejecting that keeps the error for a forgotten `=` pointing
at the line that forgot it, rather than at the line after.

In the other direction the parser is a little laxer than `monkeyc` about where
a `\` may appear. Those forms are all nonsense that no one writes, and being
lax there costs nothing — a file that relies on it wouldn't build anyway.

## Comments

A comment runs from its `#` to the end of the line **and takes the line break
with it** — `monkeyc` treats the lot as whitespace. So a comment can't end an
instruction; the instruction carries on below it:

```jungle
project.manifest = # Foo
    manifest.xml
```

That is one instruction, `project.manifest = manifest.xml`. The same rule is
what lets a list annotate its entries, the comment absorbing each break:

```jungle
base.sourcePath = source; # shared by everything
  $(round.sourcePath); # round devices
  wearable-source # everything else
```

A comment therefore belongs to the value it was written against and prints after
it. One with no value to attach to — the usual kind, alone on its line — is an
`Entry::Comment`.

The catch: a comment on the _last_ value eats the instruction's terminator, so
the line below runs into it. `monkeyc` rejects that, and so does this parser;
the printer avoids emitting it by always leaving a blank line after one.

A `#` can't appear in a value at all. Quoting doesn't help — the comment starts
inside the quotes and leaves the string unclosed.

## Writing files back

`Display` emits the line structure the grammar requires and normalises the rest:
one space around `=`, none around `;`, one instruction per line, trailing
newline. Paths, globs and redundant quotes are kept verbatim.

A blank line separates, so one survives, but a run of them collapses to a single
line — the second says nothing the first didn't. `monkey-c-formatter` treats
blank runs in Monkey C the same way.

A break inside an instruction survives only when something forces it. A comment
does; a `\` doesn't, so a continued instruction folds back onto one line.

The printer doesn't remember where the author broke a line. If wrapping long
value lists is ever wanted, the way to add it is a width rule the printer
applies itself, the way `monkey-c-formatter` wraps Monkey C — not a record of the
input's breaks.

That gives two guarantees, and fixtures cover both. A file written the way
`monkeyc` projects write them comes back byte for byte, annotated lists
included. A file leaning on the other break forms can't — folding a `\` is a
rewrite. There, what holds is that the rewrite settles in one pass: printing the
output again is a no-op, so a file doesn't drift each time a tool touches it.

[jungle-reference]: https://developer.garmin.com/connect-iq/reference-guides/jungle-reference/
