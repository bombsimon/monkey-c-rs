# Linter

Linter for Monkey C code to find both stylistic and other issues in the code.
When possible the linter supports automatically fixing the issues by using the
`--fix` flag.

> [!NOTE]
> The fixer doesn't format the code to normalize after changes so the user is
> expected to run the [`monkey-c-formatter`][formatter] after applying fixes.

```sh
› monkey-c-linter monkey-c-linter/example/Example.mc
[import-order] Warning: imports should be sorted and grouped
   ╭─[ monkey-c-linter/example/Example.mc:1:1 ]
   │
 1 │ ╭─▶ using Toybox.Lang;
   ┆ ┆
 5 │ ├─▶ using Toybox.Graphics as Gfx;
   │ │
   │ ╰─────────────────────────────────── imports should be sorted and grouped
   │
   │     Note: fix: replace with `using Toybox.Graphics as Gfx;
   │           using Toybox.Lang;
   │
   │           import Toybox.System;
   │
   │           using MyModule.First;
   │           using MyModule.Second;`
───╯
[unneeded-parens] Warning: unneeded parentheses around expression
    ╭─[ monkey-c-linter/example/Example.mc:10:17 ]
    │
 10 │     var value = (M + C + 180 + 101.11d);
    │                 ───────────┬───────────
    │                            ╰───────────── unneeded parentheses around expression
    │
    │ Note: fix: replace with `M + C + 180 + 101.11d`
────╯
[unneeded-parens] Warning: unneeded parentheses around expression
    ╭─[ monkey-c-linter/example/Example.mc:13:20 ]
    │
 13 │         var isPm = (hour >= 12);
    │                    ──────┬─────
    │                          ╰─────── unneeded parentheses around expression
    │
    │ Note: fix: replace with `hour >= 12`
────╯
[unneeded-parens] Warning: unneeded parentheses around expression
    ╭─[ monkey-c-linter/example/Example.mc:19:17 ]
    │
 19 │         value = (valueEnabled ? "V on" : "V off");
    │                 ────────────────┬────────────────
    │                                 ╰────────────────── unneeded parentheses around expression
    │
    │ Note: fix: replace with `valueEnabled ? "V on" : "V off"`
────╯

```

[formatter]: ../formatter
