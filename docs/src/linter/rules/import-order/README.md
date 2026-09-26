# `import-order`

Flags `using` and `import` declarations that aren't sorted and grouped.

## Why

A fixed order makes the imports easy to scan and avoids merge conflicts from
everyone adding lines in different places. Keeping `Toybox` separate makes it
clear what comes from the SDK and what comes from the project.

## What it flags

Imports that don't follow this order, with each group sorted alphabetically
and separated by a blank line:

1. `using Toybox.*`
2. `import Toybox.*`
3. Other `using`
4. Other `import`

Each run of imports is checked on its own, so other code between two runs keeps
them apart, the same way [ruff] sorts imports.

## What it leaves alone

When comments are mixed in with the imports, only the order is checked, and
there's no fix since moving the lines would separate them from their comments.

## Example

```monkey-c
// Before
import ModuleC;
using ModuleA;
import Toybox.Time;
using Toybox.Graphics;
import Toybox.Lang;
using Toybox.WatchUi as Ui;

// After
using Toybox.Graphics;
using Toybox.WatchUi as Ui;

import Toybox.Lang;
import Toybox.Time;

using ModuleA;

import ModuleC;
```

[ruff]: https://github.com/astral-sh/ruff
