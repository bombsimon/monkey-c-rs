# Test coverage

The Connect IQ SDK can run unit tests but can't tell you what they cover.
`rafiki coverage` adds that by recording which functions run during your tests.

> [!NOTE]
> Coverage is per function. A function counts as covered as soon as it runs,
> however much of it runs.

## Run it

With the simulator running and `monkeyc` and `monkeydo` from the SDK on your
`PATH`, run this from anywhere in the project:

```sh
rafiki coverage test -d fr965 -y developer_key.der
```

It builds the tests, runs them in the simulator and prints how many functions
in each file ran, along with the ones that didn't:

```text
╭─────────────────────┬─────────┬──────────────────╮
│ FILE                ┆ COVERED ┆ MISSED           │
╞═════════════════════╪═════════╪══════════════════╡
│ source/ClockView.mc ┆ 2/3     ┆ ClockView.onHide │
├╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┤
│ source/Format.mc    ┆ 1/2     ┆ formatDate       │
╰─────────────────────┴─────────┴──────────────────╯

TOTAL 3/5 functions executed (60%)
```

The command fails when the build fails or a test fails. A failing test still
prints the table, since the code did run.

A few flags are useful here:

- `--start-simulator` starts the simulator if it isn't running.
- `--out-format lcov --out coverage.info` writes an LCOV report for tools like
  `genhtml`, Codecov or the VS Code Coverage Gutters extension.
- `--dry-run` prints the `monkeyc` and `monkeydo` commands instead of running
  them.
- Anything after `--` is passed on to `monkeyc`, like `-- -O 3`.

Run `rafiki coverage test --help` for the full list.

## What's left out

Functions annotated with `(:test)` or `(:release)` aren't counted, since test
code shouldn't cover itself and tests don't run release builds. Skip more
annotations with `--exclude-annotation`.

## Run each step yourself

`test` is a shortcut for three steps, which you can also run one at a time, for
example in CI:

```sh
rafiki coverage instrument
monkeyc -f bin/coverage/coverage.jungle -d fr965 -o bin/coverage/cov.prg -y developer_key.der --unit-test
monkeydo bin/coverage/cov.prg fr965 -t | rafiki coverage report -
```

[How it works](how-it-works) explains what each step does.
