# Rules

Each rule has a page explaining what it looks for, what it leaves alone and why
it exists. The examples show the code after `rafiki lint --fix` followed by
`rafiki fmt`.

<!-- begin rules -->
| Rule                                                 | Flags                                                        | Fix |
| ---------------------------------------------------- | ------------------------------------------------------------ | --- |
| [`bool-comparison`](./bool-comparison)               | Comparing with `true` or `false`                             | Yes |
| [`collapsible-else-if`](./collapsible-else-if)       | An `else` block that only contains an `if`                   | Yes |
| [`collapsible-if`](./collapsible-if)                 | An `if` that only contains another `if`                      | Yes |
| [`compound-assignment`](./compound-assignment)       | `x = x + n` instead of `x += n`                              | Yes |
| [`ifs-same-cond`](./ifs-same-cond)                   | Two branches of an `if` chain with the same condition        | No  |
| [`invalid-directive`](./invalid-directive)           | A `rafiki:` comment that isn't a valid directive             | No  |
| [`naming-convention`](./naming-convention)           | Names that don't follow Garmin's coding conventions          | No  |
| [`one-class-per-file`](./one-class-per-file)         | More than one class in a file                                | No  |
| [`super-initializer-call`](./super-initializer-call) | An `initialize` that doesn't call the parent's               | No  |
| [`unused-ignore`](./unused-ignore)                   | A rule in a `rafiki: ignore` comment with nothing to silence | Yes |
<!-- end rules -->
