# Formatter

The Monkey C formatter aims to be a zero-config one-size-fits-all solution to
ensure consistent formatting of your Monkey C code. More opinionated suggestions
for the code is implemented in the [`monkey-c-linter`][linter].

> [!NOTE]
> I'd love any input and testing on the formatter. Both help finding bugs and
> inconsistencies but also input on the formatting algorithm. Please create an
> [issue] for any bug or feature request.

## Wrapping long lines

The formatter is using the [Wadler]-[Lindig] algorithm to wrap lines at a
default width of 111 columns. 111 is chosen because 80 is too little and 222 is
too much.

## The magic trailing comma

`monkey-c-formatter` uses the same magic trailing comma as [ruff] to determine
if multiple items should be wrapped over multiple lines even when they would
fit on a single line. The rule applies to arrays, dictionaries, function
declaration parameters, and function / method call arguments.

<table style="width: 980px">
<tr>
<th>Original</th>
<th>Formatted</th>
</tr>

<tr>
<td>

    function SomeFunction(arg1 as String, arg2 as Number,) as Void {
        var arr = [
            1,
            2,
            3
        ];

        var arr2 = [1, 2, 3,];

        var dict = {
            :keyOne => 1,
            :keyNumberTwo => 2
        };

        var dict2 = {:keyOne=>1, :keyNumberTwo=>2,};

        AnotherFunction(arr, arr2, dict, dict2,)
    }

</td>
<td>

    function SomeFunction(
        arg1 as String,
        arg2 as Number,
    ) as Void {
        var arr = [1, 2, 3];

        var arr2 = [
            1,
            2,
            3,
        ];

        var dict = {:keyOne => 1, :keyNumberTwo => 2};

        var dict2 = {
            :keyOne       => 1,
            :keyNumberTwo => 2,
        };

        AnotherFunction(
            arr,
            arr2,
            dict,
            dict2,
        )
    }

</td>
</tr>

</table>

## Column alignment

When alignment is enabled the formatter pads names so that the separator
operators (`=>` in dictionaries, `=` in enum variants) line up in a vertical
column. The intent is purely visual — to make related entries easier to scan.

Alignment only kicks in when an entry is already rendered multi-line. For
dictionaries that follows the magic trailing comma rule above. For enums the
formatter looks for runs of two or more consecutive variants that all have an
explicit value, and pads the names within each run.

<table style="width: 980px">
<tr>
<th>Original</th>
<th>Formatted</th>
</tr>

<tr>
<td>

    var dict = {
        :keyOne => 1,
        :keyNumberTwo => 2
        :a => 3
    };

    enum Foo {
        FIRST = 1,
        SECOND = 2,
        THIRD_ONE = 3,
    }

    enum Mixed {
        A,
        B = 5,
        C = 6,
        D,
    }

</td>
<td>

    var dict = {
        :keyOne       => 1,
        :keyNumberTwo => 2
        :a            => 3
    };

    enum Foo {
        FIRST     = 1,
        SECOND    = 2,
        THIRD_ONE = 3,
    }

    enum Mixed {
        A,
        B = 5,
        C = 6,
        D,
    }

</td>
</tr>

</table>

In the `Mixed` example the run `[B = 5, C = 6]` is already aligned with itself
and no padding is needed. The bare variants `A` and `D` break the run and stay
as-is.

[Lindig]: https://lindig.github.io/papers/strictly-pretty-2000.pdf
[Wadler]: https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
[issue]: https://github.com/bombsimon/monkey-c-rs/issues/new
[linter]: ../linter
[ruff]: https://github.com/astral-sh/ruff
