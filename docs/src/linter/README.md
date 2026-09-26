# Linter

The linter finds code that is likely a mistake or harder to read than it needs
to be. Each finding names the rule behind it, and every rule has a page under
[Rules](rules) explaining what it looks for and why.

```sh
rafiki lint
```

```text
[unneeded-parens] Warning: unneeded parentheses around expression
   ╭─[ source/Clock.mc:4:21 ]
   │
 4 │     var afternoon = (hour >= 12);
   │                     ──────┬─────
   │                           ╰─────── unneeded parentheses around expression
   │
   │ Note: fix: replace with `hour >= 12`
───╯
```

## Fixing findings

Many rules come with a fix, which `--fix` applies:

```sh
rafiki lint --fix
rafiki fmt
```

A fix only changes the code the finding is about and leaves the rest as it is,
so run the formatter afterwards to tidy up the layout. Findings without a fix
are still reported, and the command exits with `1` while any remain.

## Choosing rules

Every rule runs by default. To turn some off, or to run only a few, use
[`enable` and `disable`](../configuration/settings#lint) in `rafiki.toml` or
the matching flags:

```sh
rafiki lint --disable one-class-per-file
rafiki lint --list-rules
```
