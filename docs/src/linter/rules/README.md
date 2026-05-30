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

| Rule                                         | Auto-fix | Notes                               |
| -------------------------------------------- | -------- | ----------------------------------- |
| [`compound-assignment`][compound-assignment] | ✅       | Rewrites `x = x + n` as `x += n`    |
| [`import-order`][import-order]               | ⚠️       | Suppressed when comments interleave |
| [`unneeded-parens`][unneeded-parens]         | ✅       | Removes redundant parentheses       |

[compound-assignment]: ./compound-assignment
[diagnostic]: https://github.com/bombsimon/monkey-c-rs/blob/main/monkey-c-linter/src/lib.rs
[fix]: https://github.com/bombsimon/monkey-c-rs/blob/main/monkey-c-linter/src/lib.rs
[formatter]: ../../formatter
[import-order]: ./import-order
[unneeded-parens]: ./unneeded-parens
