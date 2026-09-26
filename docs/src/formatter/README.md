# Formatter

`rafiki fmt` formats Monkey C code, with close to nothing to configure. Run it
with `--check` or `--diff` to see what would change without writing anything.
If you're coming from [Prettier][prettier], see
[Differences from Prettier](prettier).

> [!NOTE]
> Feedback on the formatter is appreciated, whether it's a bug, an
> inconsistency or a thought on how code should be formatted. Please open an
> [issue] on GitHub.

## Layout only

The formatter only changes whitespace and line breaks, never what the code
says. Parentheses, number literals, types and the legacy `@` prefix are kept
exactly as written. Changes like removing redundant parentheses are left to the
[linter][linter], where they can be reviewed on their own.

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

## Comments

Comments are kept where they are and are never dropped. If the formatter can't
find a place for one, it warns rather than losing it.

The text of a comment is left as written apart from trailing whitespace, so a
long comment isn't rewrapped and can go past the line width. A `//` comment at
the end of a line doesn't count toward the width either, so it never forces the
code before it to wrap.

[Lindig]: https://lindig.github.io/papers/strictly-pretty-2000.pdf
[alignment]: ../configuration/settings#alignment
[line-width]: ../configuration/settings#line-width
[Wadler]: https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
[issue]: https://github.com/bombsimon/monkey-c-rs/issues/new
[linter]: ../linter
[prettier]: https://github.com/markw65/prettier-plugin-monkeyc
[ruff]: https://github.com/astral-sh/ruff
