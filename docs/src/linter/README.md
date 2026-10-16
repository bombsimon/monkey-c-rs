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

## Suppressing findings

To keep a rule on but silence it in one place, add a `rafiki: ignore` comment
with the rules to skip. Anything after `]` is an optional reason.

```monkey-c
// rafiki: ignore[naming-convention] name comes from the SDK callback
function onUpdate_legacy(dc as Dc) as Void {
    var ok = (a == true); // rafiki: ignore[bool-comparison, unneeded-parens]
}
```

A comment on its own line covers the whole declaration or statement below it,
like the function above, including its annotations. A comment at the end of a
line covers that line and the statement it ends, so it still applies after the
formatter splits a long statement over several lines.

To silence a rule for a whole file, use `rafiki: file-ignore` before any code:

```monkey-c
// rafiki: file-ignore[one-class-per-file] the view and its delegate belong together
```

Every comment has to name its rules; there's no way to silence all of them at
once.

To silence rules for whole paths, like every test file, use
[`per-file-ignores`](../configuration/settings#per-file-ignores) in
`rafiki.toml` instead. To skip linting some paths entirely, use
[`exclude`](../configuration/settings#exclude-1).

Suppressions are checked too. [`unused-ignore`](rules/unused-ignore) flags a
rule with nothing left to silence, and `--fix` removes it.
[`invalid-directive`](rules/invalid-directive) flags a `rafiki:` comment that
can't be read, like one naming a rule that doesn't exist.
