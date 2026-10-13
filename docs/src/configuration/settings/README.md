# Settings

Each setting lists its default and how to set it outside of `rafiki.toml`. Only
the formatting settings can be sent by an editor. The rest are read from the
file.

## `[format]`

### `line-width`

The width the formatter tries to keep lines within. See
[Wrapping long lines](../../formatter#wrapping-long-lines).

- **Default:** `111`
- **Flag:** `--line-width <COLUMNS>`, or `-l <COLUMNS>`
- **Editor option:** `lineWidth`

### `alignment`

Whether to line up `=>` in dictionaries, `=` in enums and trailing comments in
columns. See [Column alignment](../../formatter#column-alignment).

- **Default:** `true`
- **Flag:** `--alignment` or `--no-alignment`
- **Editor option:** `alignment`

### `short-array-element-width`

The widest an array item can be for the array to fill its lines instead of
putting each item on its own line. Every item has to fit, and `0` turns filling
off. See [Filling short arrays](../../formatter#filling-short-arrays).

- **Default:** `8`
- **Flag:** `--short-array-element-width <COLUMNS>`
- **Editor option:** `shortArrayElementWidth`

## `[lint]`

### `enable`

Rules to run. When the list is empty every rule runs, otherwise only the ones
listed. `rafiki lint --list-rules` prints the names, and each rule is described
under [Rules](../../linter/rules).

- **Default:** `[]`
- **Flag:** `--enable <RULE>`, comma separated or repeated

### `disable`

Rules to turn off. A rule listed in both `enable` and `disable` is turned off.

- **Default:** `[]`
- **Flag:** `--disable <RULE>`, comma separated or repeated

A rule name that doesn't exist is an error. Otherwise a typo in `enable` would
turn every rule off and look like a clean run.

## `[files]`

### `exclude`

Paths to skip, as globs in `.gitignore` syntax. In `rafiki.toml` they're
relative to the file itself, and on the command line they're relative to the
current directory.

- **Default:** `[]`
- **Flag:** `--exclude <GLOB>`, repeated for more than one

### `respect-gitignore`

Whether to skip files that `.gitignore` excludes. Hidden directories like `.git`
are skipped either way.

- **Default:** `true`
- **Flag:** `--respect-gitignore` or `--no-respect-gitignore`
