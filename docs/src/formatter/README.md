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

## The magic comma

`monkey-c-formatter` uses the same trailing magic comma to determine if multiple
arguments should be wrapped over multiple lines even if they would fit on a
single line, just like [ruff] does.

<table style="width: 980px">
<tr>
<th>Original</th>
<th>Formatted</th>
</tr>

<tr>
<td>

    var no_trailing = [
        1,
        2,
        3
    ];

    var trailing = [1, 2, 3,];

</td>
<td>

    var no_trailing = [1, 2, 3];

    var trailing = [
        1,
        2,
        3,
    ];

</td>
</tr>

</table>

## Aligning fat-commas

The formatter will by default align fat-commas (`=>`) when creating dictionaries
to increase readability of values. Dictionaries have the same rule regarding the
magic trailing comma.

<table style="width: 980px">
<tr>
<th>Original</th>
<th>Formatted</th>
</tr>

<tr>
<td>

    var no_trailing = {
        :keyOne => 1,
        :keyNumberTwo => 2
    };

    var trailing = {:keyOne=>1, :keyNumberTwo=>2,};

</td>
<td>

    var no_trailing = {:keyOne => 1, :keyNumberTwo => 2};

    var trailing = {
        :keyOne       => 1,
        :keyNumberTwo => 2,
    };

</td>
</tr>

</table>

[Lindig]: https://lindig.github.io/papers/strictly-pretty-2000.pdf
[Wadler]: https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
[issue]: https://github.com/bombsimon/monkey-c-rs/issues/new
[linter]: ../linter
[ruff]: https://github.com/astral-sh/ruff
