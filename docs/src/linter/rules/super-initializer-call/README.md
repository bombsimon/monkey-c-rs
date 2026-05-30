# `super-initializer-call`

Flags a derived class whose `initialize` doesn't call the parent's
`initialize`.

## Rationale

Monkey C doesn't automatically chain to the superclass constructor when a
subclass overrides `initialize`. A derived `initialize` that omits the
`Parent.initialize(...)` call leaves the base object in an uninitialised
state — a common, hard-to-debug bug that surfaces only when base-class
fields are read.

## What triggers

The rule fires when all three hold:

1. The class declares `extends Base`.
2. The class body defines its own `initialize` function.
3. No call to `Base.initialize(...)` appears anywhere in that function
   body, including inside `if` / `else` branches and nested blocks.

For a dotted parent (`extends WatchUi.View`), the rule accepts a call to
the last segment (`View.initialize(...)`) or any qualified form
(`WatchUi.View.initialize(...)`).

## What does not trigger

- A class with no `extends` clause.
- A derived class that doesn't override `initialize` — Monkey C's implicit
  no-op constructor still chains.

## Example

Before:

```monkey-c
class MyView extends WatchUi.View {
    function initialize() {
        _state = 0;
    }
}
```

After fixing manually:

```monkey-c
class MyView extends WatchUi.View {
    function initialize() {
        View.initialize();
        _state = 0;
    }
}
```

No auto-fix — the rule can't know which arguments to pass to the parent
initializer.
