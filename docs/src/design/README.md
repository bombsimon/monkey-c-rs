# Design principles

These are the ideas behind how the tools behave. When a choice isn't obvious,
it usually comes back to one of them.

## Close to zero configuration

A formatter is most useful when nobody argues about its output. `rafiki` has a
few settings, like the line width, and nothing that changes the style itself.
Options that only exist to match another formatter's output were tried and
removed, since each one doubles the ways code can look.

## The formatter only changes layout

Formatting never changes what the code says. Parentheses and number literals,
for example, are kept exactly as written, so formatting is always safe to run
and easy to review. Changes to the code itself belong to the linter, where they
show up as findings you can inspect before applying.

## Fixes are small edits

A lint fix replaces the few characters it's about and leaves everything else
alone, including the formatting around it. The linter and formatter stay
independent that way, and running `rafiki fmt` after `rafiki lint --fix` tidies
up any layout a fix leaves behind.

## Same result everywhere

The command line and the language server read the same `rafiki.toml` and share
the same formatter and linter. A file comes out identical whether it's formatted
in the terminal or in your editor.

## Fill the gaps in Garmin's tooling

Garmin's own editor extension already handles code navigation, like going to a
definition or finding references. The language server doesn't compete with it.
It adds the missing pieces, formatting and linting, and is meant to run
alongside Garmin's.

## Inspired by ruff

The parser and the tools on top of it are modeled on [ruff]. The syntax tree
keeps enough detail to write code back out, and the formatter uses the same
[Wadler]-[Lindig] algorithm to decide where lines break.

[Lindig]: https://lindig.github.io/papers/strictly-pretty-2000.pdf
[Wadler]: https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
[ruff]: https://github.com/astral-sh/ruff
