# Rules

Each lint rule walks the parsed AST looking for a specific pattern. When a rule
fires it produces a [`Diagnostic`][diagnostic] — the source range, a message,
and (when applicable) a machine-applicable [`Fix`][fix] that replaces a byte
range with new text.

Fixes are byte-level text replacements, not AST rewrites. That means a fix only
touches the affected source range and leaves the surrounding formatting alone.
Once `--fix` has been applied the user is expected to run the
[`monkey-c-formatter`][formatter] if they want whitespace normalised.

## Categories

| Rule                                               | Auto-fix | Notes                                                           |
| -------------------------------------------------- | -------- | --------------------------------------------------------------- |
| [`collapsible-else-if`][collapsible-else-if]       | ✅       | Rewrites `else { if … }` as `else if …`                         |
| [`collapsible-if`][collapsible-if]                 | ✅       | Merges a nested `if` with `&&`; skipped when comments intervene |
| [`compound-assignment`][compound-assignment]       | ✅       | Rewrites `x = x + n` as `x += n`                                |
| [`ifs-same-cond`][ifs-same-cond]                   | ❌       | Checks for consecutive ifs with the same condition              |
| [`import-order`][import-order]                     | ⚠️       | Suppressed when comments interleave                             |
| [`naming-convention`][naming-convention]           | ❌       | Naming convention according to [Coding Conventions]             |
| [`one-class-per-file`][one-class-per-file]         | ❌       | Only allow one class per file according to [Coding Conventions] |
| [`redundant-resource-ref`][redundant-resource-ref] | ✅       | Drops legacy `@` on `Rez.*` refs                                |
| [`super-initializer-call`][super-initializer-call] | ❌       | Flags missing `Base.initialize(…)` [Coding Conventions]         |
| [`unneeded-parens`][unneeded-parens]               | ✅       | Removes redundant parentheses                                   |

[Coding Conventions]: https://developer.garmin.com/connect-iq/monkey-c/coding-conventions/
[collapsible-else-if]: ./collapsible-else-if
[collapsible-if]: ./collapsible-if
[compound-assignment]: ./compound-assignment
[diagnostic]: https://github.com/bombsimon/monkey-c-rs/blob/main/monkey-c-linter/src/lib.rs
[fix]: https://github.com/bombsimon/monkey-c-rs/blob/main/monkey-c-linter/src/lib.rs
[formatter]: ../../formatter
[ifs-same-cond]: ./ifs-same-cond
[import-order]: ./import-order
[naming-convention]: ./naming-convention
[one-class-per-file]: ./one-class-per-file
[redundant-resource-ref]: ./redundant-resource-ref
[super-initializer-call]: ./super-initializer-call
[unneeded-parens]: ./unneeded-parens
