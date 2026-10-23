# Settings

Each setting lists its default and how to set it outside of `rafiki.toml`. Only
the formatting settings can be sent by an editor. The rest are read from the
file.

Every setting at its default:

```toml
[format]
line-width = 111
alignment = true
short-array-element-width = 8
union-style = "or"
nullable = "shorthand"
annotation-separator = "space"
resource-ref = "bare"
modifier-order = "visibility-first"
parens = "normalize"
sort-imports = true
exclude = []

[format.numbers]
hex-prefix-case = "lower"
hex-digit-case = "upper"
exponent-case = "lower"
exponent-padding = "trim"
float-suffix-case = "lower"
double-suffix-case = "lower"
long-suffix-case = "upper"
leading-zero = "add"
trailing-zeros = "preserve"

[lint]
enable = []
disable = []
exclude = []

[lint.per-file-ignores]
# "tests/**" = ["naming-convention"]

[files]
exclude = []
respect-gitignore = true
```

## `[format]`

### `line-width`

The width the formatter tries to keep lines within. See
[Wrapping long lines](../../formatter#wrapping-long-lines).

- **Default:** `111`
- **Flag:** `--line-width <COLUMNS>`, or `-l <COLUMNS>`
- **Editor option:** `lineWidth`

### `alignment`

Whether to line up `=>` in dictionaries, `=` in enums and trailing comments in
columns. See [Column alignment](../../formatter#column-alignment).

- **Default:** `true`
- **Flag:** `--alignment` or `--no-alignment`
- **Editor option:** `alignment`

### `short-array-element-width`

The widest an array item can be for the array to fill its lines instead of
putting each item on its own line. Every item has to fit, and `0` turns filling
off. See [Filling short arrays](../../formatter#filling-short-arrays).

- **Default:** `8`
- **Flag:** `--short-array-element-width <COLUMNS>`
- **Editor option:** `shortArrayElementWidth`

The settings below choose how the formatter spells code, not just how it lays
it out. Each one rewrites the code into a form that means exactly the same, and
`"preserve"` keeps it as written. They're only read from `rafiki.toml`, since
the spelling is a choice for the whole project. See
[Rewrites](../../formatter#rewrites).

### `union-style`

The separator between the members of a union type: `"or"` for
`Number or Float`, `"pipe"` for `Number | Float`, or `"preserve"`.

`or` is what Garmin's documentation and nearly all Monkey C code use, and it
can't be mistaken for a bitwise or. A cast only reads `|` as a union when a type
name, `{` or `[` follows it, so `"pipe"` keeps `or` in front of a parenthesized
member there.

- **Default:** `"or"`

### `nullable`

How a type that may be `null` is written: `"shorthand"` for `String?`,
`"union"` for `String or Null`, or `"preserve"`. With `"union"`, a union with
several `?` members gets a single `Null`.

Only a union of one type with `Null` has a shorthand. `String or Number or Null`
is kept, and so is `Null or Method() as Boolean`, where the `?` would go to the
return type.

- **Default:** `"shorthand"`

### `annotation-separator`

The separator between the entries of an annotation: `"space"` for
`(:debug :background)`, `"comma"` for `(:debug, :background)`, or `"preserve"`.
Space is the only form Garmin documents.

- **Default:** `"space"`

### `resource-ref`

Whether to drop the legacy `@` in front of a resource reference, which compiles
the same without it: `"bare"` writes `Rez.Strings.name`, `"preserve"` keeps
`@Rez.Strings.name`.

- **Default:** `"bare"`

### `modifier-order`

The order of the visibility and `static` keywords: `"visibility-first"` for
`private static`, or `"preserve"`. The compiler accepts both, and visibility
first matches Java and C#. Keywords with a comment between them are kept as
written.

- **Default:** `"visibility-first"`

### `parens`

Whether the formatter decides where parentheses go: `"normalize"` or
`"preserve"`. Normalizing drops the ones that can't change how the code is read
and adds the ones that make it easier to read.

They're dropped around a whole expression where something else already ends
it: the value of an assignment, initializer or `return`, a call argument, array
element, dictionary key or value, index, condition, and a ternary's condition or
branches. They're also dropped around an operand whose operator binds tighter than the one
around it, or as tight on the left, around the same `&&`, `||`, `&`, `|` or `^`
on the right, so `a || (b || c)` becomes `a || b || c`, around a name, literal,
call or other single operand, and around a `Method(…)` type that nothing follows.

A pair with a comment inside is kept, and so are pairs whose removal would change
how the code reads: around a cast that something follows, a ternary inside a
ternary's condition or `then` branch, and `-(-x)`.

They're added, or kept, where operators mix in a way that's easy to misread: `*`,
`/` and `%` with each other, as Prettier does, and every mix where Monkey C groups
differently from C. Monkey C puts `<<`, `>>`, `&`, `instanceof` and `has` on the
same level as `*`, and `|` and `^` with `+`, so `a + b << c` means `a + (b << c)`,
and that's how it's written.

```monkey-c
var thirds = width / 3 * 2;      // var thirds = (width / 3) * 2;
var sumShift = a + b << c;       // var sumShift = a + (b << c);
var maskEquals = x & mask == 0;  // var maskEquals = (x & mask) == 0;
```

- **Default:** `"normalize"`

### `sort-imports`

Whether to sort each run of `using` and `import` declarations into four groups
split by blank lines: `using Toybox.*`, `import Toybox.*`, the other `using`s,
then the other `import`s, each sorted by name. A run with a comment between its
imports, at the end of the last one or right above the first is kept as written,
since the comment could be about any one of them. A comment with a blank line
below it is taken to be about the whole block, so the run is still sorted. A run
right after an annotation is kept too, since the annotation belongs to the
import below it. A `rafiki: fmt off` region splits a run in two.

- **Default:** `true`

### `exclude`

Paths to never format, on top of [`[files]` `exclude`](#exclude-2), like a file
of hand-aligned tables that's still linted. Uses the same globs, relative to
`rafiki.toml`.

- **Default:** `[]`

## `[format.numbers]`

How number literals are spelled, one setting per part of the literal. Like the
settings above they're only read from `rafiki.toml`, and none of them changes a
literal's type or value: whether it has a `.`, an exponent or a suffix stays as
written, and so do the digits of a decimal integer.

The casing settings take `"lower"`, `"upper"` or `"preserve"`.

### `hex-prefix-case`

The casing of the `0x` in front of a hex literal: `0XFF` to `0xFF`.

- **Default:** `"lower"`

### `hex-digit-case`

The casing of the digits of a hex literal: `0xff` to `0xFF`. Uppercase is what
Garmin's documentation uses, and it sets the digits apart from the lowercase
`x`.

- **Default:** `"upper"`

### `exponent-case`

The casing of the `e` in scientific notation: `1E5` to `1e5`.

- **Default:** `"lower"`

### `exponent-padding`

Whether to drop the `+` and leading zeros of an exponent: `"trim"` writes
`2.5e+07` as `2.5e7`, `"preserve"` keeps it.

- **Default:** `"trim"`

### `float-suffix-case`

The casing of the `f` that makes a literal a `Float`: `1.0F` to `1.0f`.

- **Default:** `"lower"`

### `double-suffix-case`

The casing of the `d` that makes a literal a `Double`: `1.0D` to `1.0d`.

- **Default:** `"lower"`

### `long-suffix-case`

The casing of the `l` that makes a literal a `Long`, in decimal and hex alike:
`5l` to `5L` and `0xFFl` to `0xFFL`. Uppercase is the default since `10l` is
easy to misread as `101`.

- **Default:** `"upper"`

### `leading-zero`

Whether to write the `0` in front of a fraction: `"add"` writes `.5` as `0.5`
and `.5d` as `0.5d`, `"preserve"` keeps it.

- **Default:** `"add"`

### `trailing-zeros`

Whether to drop zeros at the end of a fraction: `"trim"` writes `1.50` as `1.5`,
`"preserve"` keeps it. One digit always stays after the `.`, so `1.0` is never
written as `1`, which would be a `Number`. Kept by default, since trailing zeros
are often there on purpose, like lining up a table of values by precision.

- **Default:** `"preserve"`

## `[lint]`

### `enable`

Rules to run. When the list is empty every rule runs, otherwise only the ones
listed. `rafiki lint --list-rules` prints the names, and each rule is described
under [Rules](../../linter/rules).

- **Default:** `[]`
- **Flag:** `--enable <RULE>`, comma separated or repeated

### `disable`

Rules to turn off. A rule listed in both `enable` and `disable` is turned off.

- **Default:** `[]`
- **Flag:** `--disable <RULE>`, comma separated or repeated

A rule name that doesn't exist is an error. Otherwise a typo in `enable` would
turn every rule off and look like a clean run.

### `exclude`

Paths to never lint, on top of [`[files]` `exclude`](#exclude-2), like test
fixtures that are still formatted. Uses the same globs, relative to
`rafiki.toml`.

- **Default:** `[]`

### `per-file-ignores`

Rules to turn off for some paths only, as a table from a glob to a list of
rules. Uses the same globs as `exclude`, relative to `rafiki.toml`.

```toml
[lint.per-file-ignores]
"tests/**" = ["naming-convention"]
"source/Strings.mc" = ["one-class-per-file", "naming-convention"]
```

- **Default:** `{}`

## `[files]`

### `exclude`

Paths to neither format nor lint, as globs in `.gitignore` syntax. In
`rafiki.toml` they're relative to the file itself, and on the command line
they're relative to the current directory.

As in `.gitignore`, a `!` pattern brings back a path an earlier pattern
excluded, but not a file inside an excluded directory. Use `gen/*` with
`!gen/Keep.mc` rather than `gen/`.

Excluded files are skipped even when you name them on the command line, so a
pre-commit hook or editor integration that passes every changed file still
leaves them alone. The language server skips them too. Use `--no-config` to
reach one anyway.

- **Default:** `[]`
- **Flag:** `--exclude <GLOB>`, repeated for more than one

### `respect-gitignore`

Whether to skip files that `.gitignore` excludes. Hidden directories like `.git`
are skipped either way.

- **Default:** `true`
- **Flag:** `--respect-gitignore` or `--no-respect-gitignore`
