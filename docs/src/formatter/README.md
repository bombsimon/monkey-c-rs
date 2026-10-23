# Formatter

`rafiki fmt` formats Monkey C code, with close to nothing to configure. Run it
with `--check` or `--diff` to see what would change without writing anything.
If you're coming from [Prettier][prettier], see
[Differences from Prettier](prettier).

> [!NOTE]
> Feedback on the formatter is appreciated, whether it's a bug, an
> inconsistency or a thought on how code should be formatted. Please open an
> [issue] on GitHub.

## Never changes meaning

The formatter changes whitespace and line breaks, and the [rewrites](#rewrites)
below that spell the same code in one consistent way. It never changes what the
code does. Changes that could, or that change the shape of the code like merging
two `if`s, are left to the [linter][linter], where they can be reviewed on their
own.

Two more changes are always made:

- `new Foo` gets an empty argument list and becomes `new Foo()`.
- Extra semicolons are dropped, so `var x = 1;;` becomes `var x = 1;`.

## Rewrites

Monkey C often has several ways to write the same thing. The formatter picks one
of them, and each choice is a [setting][settings] that can also keep the code as
written with `"preserve"`.

```monkey-c
// Before
using Toybox.WatchUi;
import Toybox.Lang;
using Toybox.Graphics;

(:debug, :background)
static private var mLabel as String or Null;

function width(values as Array<Number | Float>) as Number {
    return (values.size() * 2);
}

function title() {
    return @Rez.Strings.Title;
}

// After
using Toybox.Graphics;
using Toybox.WatchUi;

import Toybox.Lang;

(:debug :background)
private static var mLabel as String?;

function width(values as Array<Number or Float>) as Number {
    return values.size() * 2;
}

function title() {
    return Rez.Strings.Title;
}
```

| Setting                                        | Default              | Rewrites                                          |
| ---------------------------------------------- | -------------------- | ------------------------------------------------- |
| [`union-style`][union-style]                   | `"or"`               | `Number \| Float` to `Number or Float`            |
| [`nullable`][nullable]                         | `"shorthand"`        | `String or Null` to `String?`                     |
| [`annotation-separator`][annotation-separator] | `"space"`            | `(:debug, :background)` to `(:debug :background)` |
| [`resource-ref`][resource-ref]                 | `"bare"`             | `@Rez.Strings.Title` to `Rez.Strings.Title`       |
| [`modifier-order`][modifier-order]             | `"visibility-first"` | `static private` to `private static`              |
| [`parens`][parens]                             | `"normalize"`        | `return (a + b);` to `return a + b;`              |
| [`sort-imports`][sort-imports]                 | `true`               | Imports sorted into groups                        |
| [`[format.numbers]`][numbers]                  | See settings         | `0Xff` to `0xFF`, `.5` to `0.5`, `1E+05` to `1e5` |

A rewrite is skipped wherever it would read differently, or where a comment
inside the code it changes could end up next to the wrong part. To keep one spot
as written, use [`rafiki: fmt off`](#skipping-formatting).

## Wrapping long lines

The formatter uses the [Wadler]-[Lindig] algorithm to wrap lines at a default
width of 111 columns. 111 is chosen because 80 is too little and 222 is too
much. The width can be changed with the [`line-width`][line-width] setting.

Anything that can wrap, such as an argument list, an array or a condition, stays
on one line if it fits together with whatever follows it on that line.
Otherwise it's broken with each part on its own line, starting from the
outermost construct, and anything still too long is broken the same way.

## The magic trailing comma

The formatter uses the same magic trailing comma as [ruff] to determine
if multiple items should be wrapped over multiple lines even when they would
fit on a single line. The rule applies to arrays, dictionaries, function
declaration parameters, and function / method call arguments.

The formatter never adds a trailing comma itself.

A list without a trailing comma folds onto one line when it fits:

```monkey-c
// Before
var values = [
    1,
    2
];

// After
var values = [1, 2];
```

A trailing comma keeps it broken, one item per line, even when it would fit:

```monkey-c
// Before
var values = [1, 2,];

// After
var values = [
    1,
    2,
];
```

The same goes for parameters and call arguments:

```monkey-c
// Before
function f(first, second,) {
    call(first, second,);
}

// After
function f(
    first,
    second,
) {
    call(
        first,
        second,
    );
}
```

## Column alignment

The formatter pads names so that `=>` in dictionaries and `=` in enums line up
in a column, and does the same for trailing comments on consecutive lines. The
intent is purely visual, to make related entries easier to scan. It can be
turned off with the [`alignment`][alignment] setting.

Alignment only applies to entries that are already on separate lines, so a
dictionary needs a [magic trailing comma](#the-magic-trailing-comma) or has to
be too long for one line. In enums only runs of consecutive variants with an
explicit value are aligned.

```monkey-c
// Before
var dict = {
    :keyOne => 1,
    :keyNumberTwo => 2,
};

enum Color {
    COLOR_RED = 1,
    COLOR_GREEN = 2, // default
    COLOR_BLUE_DARK = 3, // unused
}

// After
var dict = {
    :keyOne       => 1,
    :keyNumberTwo => 2,
};

enum Color {
    COLOR_RED       = 1,
    COLOR_GREEN     = 2, // default
    COLOR_BLUE_DARK = 3, // unused
}
```

## Filling short arrays

An array that has to wrap usually gets one item per line. When every item is
short, it fills each line instead and lines the items up in columns, so byte
arrays and lookup tables don't run for pages. Numbers line up by their last
digit and everything else by its first character. Here at a line width of 80
and 30:

```monkey-c
var parameters = [
    0x56, 0x34, 0x12, 0x63, 0x96, 0x47, 0x71, 0x73, 0x4f, 0xbd, 0x76, 0xe3,
    0xb4, 0x05, 0x19, 0xd1, 0xd9, 0x4a, 0x48
]b;

var offsets = [
       0, 12, 250, -3, 1024,
    -512, 16,  88,  7,   42
];
```

An item counts as short when it's at most 8 columns wide, which can be changed
with the [`short-array-element-width`][short-array-element-width] setting. A
[magic trailing comma](#the-magic-trailing-comma) still breaks the array, and
the items are filled inside it. An array with a comment or a blank line between
its items keeps one item per line, since those usually group the items by hand.

## Comments

Comments are kept where they are and are never dropped. When the usual layout
would move a comment, the formatter leaves that code as written instead, so a
comment stays next to the code it describes. If the formatter can't find a place
for one, it warns rather than losing it.

The text of a comment is left as written apart from trailing whitespace, so a
long comment isn't rewrapped and can go past the line width. A `//` comment at
the end of a line doesn't count toward the width either, so it never forces the
code before it to wrap.

## Skipping formatting

To keep some code exactly as written, like a hand-aligned table, put it between
`rafiki: fmt off` and `rafiki: fmt on` comments:

```monkey-c
// rafiki: fmt off
const LOOKUP = [
    1,   2,   3,
    10,  20,  30,
];
// rafiki: fmt on
```

Both comments go on their own lines between statements or declarations, like
[ruff]'s `fmt: off`. Without a `rafiki: fmt on`, the rest of the block is kept as
written. The lines keep their layout relative to each other, but the code as a
whole is indented to match the code around it. A line indented less than the
`rafiki: fmt off` comment is moved in to line up with it.

[Lindig]: https://lindig.github.io/papers/strictly-pretty-2000.pdf
[alignment]: ../configuration/settings#alignment
[annotation-separator]: ../configuration/settings#annotation-separator
[line-width]: ../configuration/settings#line-width
[modifier-order]: ../configuration/settings#modifier-order
[nullable]: ../configuration/settings#nullable
[parens]: ../configuration/settings#parens
[resource-ref]: ../configuration/settings#resource-ref
[settings]: ../configuration/settings
[short-array-element-width]: ../configuration/settings#short-array-element-width
[sort-imports]: ../configuration/settings#sort-imports
[numbers]: ../configuration/settings#formatnumbers
[union-style]: ../configuration/settings#union-style
[Wadler]: https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
[issue]: https://github.com/bombsimon/monkey-c-rs/issues/new
[linter]: ../linter
[prettier]: https://github.com/markw65/prettier-plugin-monkeyc
[ruff]: https://github.com/astral-sh/ruff
