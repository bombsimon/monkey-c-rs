# Jungle library

`monkey-c-jungle` reads and writes [jungle files][jungle-reference], which tell
`monkeyc` which sources, resources and annotations to use for each device. It
keeps comments and blank lines, so a file can be edited and written back
without losing anything.

```rust,ignore
use monkey_c_jungle::ast::{JungleFile, Value};

let mut jungle = JungleFile::parse("base.sourcePath = source\n")?;

jungle.set(
    "fenix5.resourcePath",
    [
        Value::reference("fenix5.resourcePath"),
        Value::text("fenix-resources"),
    ],
);

print!("{jungle}");
```

## Building a file

An empty `JungleFile` is built with the same methods:

```rust,ignore
let mut jungle = JungleFile::default();

jungle.set("project.manifest", [Value::text("manifest.xml")]);
jungle.push_blank_line();

jungle.push_comment("Shared sources");
jungle.set("base.sourcePath", [Value::text("source")]);
jungle.push_blank_line();

jungle.set(
    "round.resourcePath",
    [
        Value::reference("round.resourcePath"),
        Value::text("resources-round").with_comment("every round device"),
    ],
);

std::fs::write("monkey.jungle", jungle.to_string())?;
```

```jungle
project.manifest = manifest.xml

# Shared sources
base.sourcePath = source

round.resourcePath = $(round.resourcePath);resources-round # every round device
```

`Value::text` is a plain value, `Value::reference` a `$(…)` and `Value::group` a
`[…]`. `set` replaces the last assignment to a name, or adds one if there is
none. `get` reads a value back and `remove` deletes it.

## The syntax tree

A `JungleFile` is a list of entries in file order: an assignment, a comment or a
blank line. An assignment has a name and a list of values:

| Source                       | Parsed as                                    |
| ---------------------------- | -------------------------------------------- |
| `fenix5.lang.eng`            | qualifier `fenix5`, property `lang.eng`      |
| `a;b`                        | two values                                   |
| `$(base.sourcePath)/shared`  | one value, a reference followed by text      |
| `[round.jungle;rect.jungle]` | one group of two values                      |
| `"my sources/app.mc"`        | one quoted value                             |

Only `;` separates values, so `$(fenix5.resourcePath);extra` adds a path while
`$(base.sourcePath)/extra` extends one.

## Line breaks and comments

An assignment ends at the end of the line, except in three places, the same as
in `monkeyc`: right after the `=`, after a `;` followed by a comment, and after
a `;` followed by `\`.

A comment runs to the end of the line and belongs to the value before it. A
comment after the last value continues the assignment onto the next line, so
the printer always puts a blank line after one. A `#` can't appear inside a
value, even in quotes.

## Writing files

Printing normalizes the spacing to one space around `=`, no space around `;`
and one assignment per line. Several blank lines in a row become one, and a
`\` line continuation is joined onto one line. Printing a file that was already
printed gives the same result, so tools that edit a file don't make it drift.

[jungle-reference]: https://developer.garmin.com/connect-iq/reference-guides/jungle-reference/
