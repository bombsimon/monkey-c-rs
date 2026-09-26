# `super-initializer-call`

Flags a class whose `initialize` doesn't call the `initialize` of the class it
extends.

## Why

Monkey C doesn't call the parent's `initialize` for you when a class defines its
own. Forgetting it leaves the parent half set up, which usually only shows when
something reads one of its fields.

## What it flags

A class that extends another class, defines `initialize` and never calls the
parent's `initialize` anywhere in it. For `extends WatchUi.View`, both
`View.initialize()` and `WatchUi.View.initialize()` count.

## What it leaves alone

- Classes that don't extend another class.
- Classes that don't define `initialize`, since the parent's is then used.

## Example

```monkey-c
// Before
class MyView extends WatchUi.View {
    function initialize() {
        _state = 0;
    }
}

// After fixing it by hand
class MyView extends WatchUi.View {
    function initialize() {
        View.initialize();
        _state = 0;
    }
}
```

There's no fix, since the rule can't know what to pass to the parent.
