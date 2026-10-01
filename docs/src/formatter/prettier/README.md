# Differences from Prettier

[Prettier with its Monkey C plugin][prettier-plugin-monkeyc] is the other
formatter in use for Monkey C, so it's a natural point of comparison. `rafiki`
doesn't aim to reproduce it line for line, and it has no flags to emulate it.
This page lists the differences currently known and why they exist. It isn't
complete, and entries may change as either formatter evolves.

The examples compare the output of both at a line width of 80 with alignment
disabled.

## Line width

`rafiki`'s default line width is 111 to match modern formatters and monitors,
while Prettier defaults to 80. Both can be reconfigured, with `--line-width` or
`line-width` in `rafiki.toml`.

## Alignment

`rafiki` [aligns][column-alignment] dictionaries and trailing comments into
columns for readability, which Prettier doesn't. It can be disabled with
`--no-alignment`, or with `alignment = false` in `rafiki.toml`.

## Trailing commas

Prettier adds a trailing comma to every list it breaks over multiple lines.
`rafiki` only keeps a trailing comma that's already in the source, where it
works as a [magic trailing comma][magic-trailing-comma] and keeps the list
broken. Leaving it out means a list folds back onto one line by itself if it
shrinks, e.g. after renaming a variable.

```monkey-c
// rafiki
var settings = {
    :background => Graphics.COLOR_BLACK,
    :foreground => Graphics.COLOR_WHITE
};

// Prettier
var settings = {
    :background => Graphics.COLOR_BLACK,
    :foreground => Graphics.COLOR_WHITE,
};
```

## Parentheses

Prettier rewrites parentheses: it drops the ones precedence makes redundant and
adds some for clarity. `rafiki` never changes what the code says, so it keeps
parentheses exactly as written.

```monkey-c
// rafiki
if ((value != null) && (value.count != 0)) {
var width = (percent / 100f * total).toLong();
var scaled = 10 / 360.toFloat();

// Prettier
if (value != null && value.count != 0) {
var width = ((percent / 100f) * total).toLong();
var scaled = 10 / (360).toFloat();
```

> [!TIP]
> The linter's [`unneeded-parens`][unneeded-parens] rule removes redundant
> parentheses with `--fix`. It only covers positions where parentheses never
> matter, such as the right-hand side of an assignment or a `return` value, so
> the first example above isn't rewritten by either tool.

## Legacy `@` resource references

Prettier also drops the legacy `@` prefix from resource references. That's
another change to what the code says rather than to its layout, so `rafiki`
keeps it.

```monkey-c
// rafiki
label.setText(@Rez.Strings.Title);

// Prettier
label.setText(Rez.Strings.Title);
```

> [!TIP]
> The linter's [`redundant-resource-ref`][redundant-resource-ref] rule removes
> the prefix with `--fix`.

## Number literals

Prettier rewrites number literals into one form: lowercase hex digits and
exponents, a leading zero before the decimal point and no trailing zeros after
it. `rafiki` keeps literals as written.

```monkey-c
// rafiki
var color = 0xFFAA00;
var half = .5;
var ratio = 0.80;
var tiny = 3.5E-7f;

// Prettier
var color = 0xffaa00;
var half = 0.5;
var ratio = 0.8;
var tiny = 3.5e-7f;
```

## Nullable types

Prettier shortens `or Null` and `| Null` in a type to `?`. `rafiki` keeps the
type as written.

```monkey-c
// rafiki
function f(value as Number or Null) as String | Null {

// Prettier
function f(value as Number?) as String? {
```

> [!TIP]
> The linter's [`null-union`][null-union] rule shortens it to `?` with `--fix`.

## Union types

Prettier writes every `|` in a union type as `or`. `rafiki` keeps whichever
was written.

```monkey-c
// rafiki
typedef Numeric as Number | Float;

// Prettier
typedef Numeric as Number or Float;
```

> [!TIP]
> The linter's [`pipe-union`][pipe-union] rule rewrites `|` to `or` with `--fix`.

## Annotations

Prettier adds a comma between annotations separated only by a space, without a
space after it. `rafiki` keeps the group as written.

```monkey-c
// rafiki
(:excludeFromLowMemoryDevices :excludeFromDataField)

// Prettier
(:excludeFromLowMemoryDevices,:excludeFromDataField)
```

## Nested generic types

Prettier puts a space between the closing `>` of nested generic types. `rafiki`
writes them together.

```monkey-c
// rafiki
var rows = [] as Array<Array<Number>>;

// Prettier
var rows = [] as Array<Array<Number> >;
```

## Binary operators

When an expression has to break, `rafiki` breaks before an operator and starts
the continuation line with it. Prettier breaks after the operator and leaves it
at the end of the line. This is one of the most common differences between the
two, and applies to every binary operator, most often `+` when building strings
and `&&` or `||` in conditions.

```monkey-c
// rafiki
string += posnTime.hour.format("%2d")
    + ":"
    + posnTime.min.format("%02d")
    + ":"
    + posnTime.sec.format("%02d");

// Prettier
string +=
    posnTime.hour.format("%2d") +
    ":" +
    posnTime.min.format("%02d") +
    ":" +
    posnTime.sec.format("%02d");
```

A parenthesised expression keeps its `(` next to the first operand and its `)`
after the last one. Prettier moves the expression inside the parentheses, as it
does for [conditions](#conditions).

```monkey-c
// rafiki
return (coordinates[0] > buttonCoordinates[0][0]
    && coordinates[0] < buttonCoordinates[0][1]
    && coordinates[1] > buttonCoordinates[1][0]);

// Prettier
return (
    coordinates[0] > buttonCoordinates[0][0] &&
    coordinates[0] < buttonCoordinates[0][1] &&
    coordinates[1] > buttonCoordinates[1][0]
);
```

## Breaking after `=`

Prettier first tries breaking after `=` and keeping the whole right-hand side
together on the next line. `rafiki` keeps the start of the expression next to
the `=` and breaks at its operators instead.

```monkey-c
// rafiki
isAvailable = SomeModule.SOME_LONG_CONSTANT_NAME
    == otherObject.someLongPropertyName;

// Prettier
isAvailable =
    SomeModule.SOME_LONG_CONSTANT_NAME == otherObject.someLongPropertyName;
```

## Conditions

A condition that doesn't fit breaks at its operators, with the closing `)` right
after the last operand and `{` on its own line. Prettier moves the whole
condition inside the parentheses instead, which leaves `if (` and `) {` on lines
of their own.

```monkey-c
// rafiki
if (Toybox has :SomeModule
    && Toybox.SomeModule has :someVeryLongFunctionName)
{

// Prettier
if (
    Toybox has :SomeModule &&
    Toybox.SomeModule has :someVeryLongFunctionName
) {
```

## Call arguments

When a call doesn't fit, `rafiki` puts every argument on its own line. Prettier
first tries to break only the last argument when it's a non-empty array or
dictionary and the argument before it isn't of the same kind.

```monkey-c
// rafiki
var text = Lang.format(
    "$1$ of $2$",
    [currentValue.format("%d"), maximumValue.format("%d")]
);

// Prettier
var text = Lang.format("$1$ of $2$", [
    currentValue.format("%d"),
    maximumValue.format("%d"),
]);
```

Prettier tries three layouts in order: everything on one line, only the last
argument broken, and every argument on its own line. `rafiki` only keeps
brackets next to the parentheses when the array or dictionary is the
only argument.

> [!NOTE]
> Whether to expand the last argument like Prettier is still open. It saves a
> level of indentation for calls like `Lang.format`, but the other arguments
> end up easy to miss at the end of a long first line.

## Comments

`rafiki` keeps [comments where they are][comments]. Prettier moves some of them,
most visibly a comment after an opening `{`, which it puts on the next line
above the first statement.

```monkey-c
// rafiki
if (isReady) { // wait for the first reading
    update();
}

// Prettier
if (isReady) {
    // wait for the first reading
    update();
}
```

## Multi-line block comments

When a `/* … */` comment is re-indented, `rafiki` moves every line of it by the
same amount, so the layout inside the comment stays intact. Prettier only does
that when every line starts with `*`, like a `/** … */` doc comment. For any
other comment it moves just the first line and leaves the rest where they were,
which separates the comment from its opener. This matches how `rustfmt` and
`gofmt` handle block comments.

```monkey-c
// Source
function f() {
  if (x) {
/* disabled
foo();
    bar();
*/
                 /*
                 bar
                 */
  }
}

// rafiki
function f() {
    if (x) {
        /* disabled
        foo();
            bar();
        */
        /*
        bar
        */
    }
}

// Prettier
function f() {
    if (x) {
        /* disabled
foo();
    bar();
*/
        /*
                 bar
                 */
    }
}
```

Comments that are already indented correctly come out unchanged in both.

## Dictionary spacing

`rafiki` writes one-line dictionaries without spaces inside the braces, the same
way as arrays and as most of the Connect IQ SDK samples. Prettier pads them.

```monkey-c
// rafiki
var params = {"key" => "value"};

// Prettier
var params = { "key" => "value" };
```

## Blank lines between declarations

`rafiki` always puts a blank line between functions and classes. Prettier keeps
whatever blank lines the source has.

```monkey-c
// rafiki
class C {
    function a() {}

    function b() {}
}

// Prettier
class C {
    function a() {}
    function b() {}
}
```

## Empty blocks

`rafiki` writes every empty block as `{}`. Prettier splits some of them over two
lines, such as the body of an `if` or a class, while keeping others like
`function f() {}` on one.

```monkey-c
// rafiki
if (x) {}
class Foo {}

// Prettier
if (x) {
}
class Foo {
}
```

## Long class headers

Prettier breaks a class header that doesn't fit before `extends`, with `{` on
its own line. `rafiki` keeps the header on one line even when it overflows.

```monkey-c
// rafiki
class ExampleBackgroundServiceDelegate extends Toybox.Background.ServiceDelegate {

// Prettier
class ExampleBackgroundServiceDelegate
    extends Toybox.Background.ServiceDelegate
{
```

## Index expressions

Prettier breaks inside `[...]` when an index expression doesn't fit. `rafiki`
keeps index expressions on one line, even when that overflows.

```monkey-c
// rafiki
var entries = {
    :first => lookupTable[indexTable[SomeModule.SOME_VERY_LONG_CONSTANT_NAME_HERE]],
    :second => 2
};

// Prettier
var entries = {
    :first => lookupTable[
        indexTable[SomeModule.SOME_VERY_LONG_CONSTANT_NAME_HERE]
    ],
    :second => 2,
};
```

## Missing semicolons

Prettier adds a semicolon the source is missing. `rafiki` reports a parse
error instead, since the Monkey C compiler rejects the code as well.

```monkey-c
// Source
var a = 1
var b = 2;

// Prettier
var a = 1;
var b = 2;
```

[column-alignment]: ..#column-alignment
[comments]: ..#comments
[magic-trailing-comma]: ..#the-magic-trailing-comma
[null-union]: ../../linter/rules/null-union
[pipe-union]: ../../linter/rules/pipe-union
[prettier-plugin-monkeyc]: https://github.com/markw65/prettier-plugin-monkeyc
[redundant-resource-ref]: ../../linter/rules/redundant-resource-ref
[unneeded-parens]: ../../linter/rules/unneeded-parens
