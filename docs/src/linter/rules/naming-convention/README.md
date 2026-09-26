# `naming-convention`

Flags names that don't follow Garmin's
[Monkey C coding conventions][conventions].

## Why

Consistent names tell you what something is at a glance, like whether
`myThing` is a class or a function. The rules below follow Garmin's conventions.

## What it flags

| Kind                                  | Convention                        | Example                   |
| ------------------------------------- | --------------------------------- | ------------------------- |
| Modules and classes                   | `PascalCase`                      | `MyClass`                 |
| Functions and parameters              | `camelCase`                       | `myFunction(myArg)`       |
| Public class members                  | `camelCase`                       | `var myValue;`            |
| Private, protected and hidden members | `_camelCase`                      | `private var _value;`     |
| Variables                             | `camelCase`                       | `var myTotal = 0;`        |
| Enum values                           | `SCREAMING_SNAKE_CASE`, one prefix | `COLOR_RED`, `COLOR_BLUE` |

## What it leaves alone

Constants. Garmin's conventions don't say how to name them, so `const` names
aren't checked. The SDK itself mostly uses `SCREAMING_SNAKE_CASE` for them.

## Example

```monkey-c
class example {
    var Value = 1;
    private var mCounter as Number = 0;

    function MyFn() {}
}
```

Each finding suggests a name that follows the convention, like `Example` for
`example` and `_mCounter` for `mCounter`. There's no fix, since renaming
something means changing every place it's used.

[conventions]: https://developer.garmin.com/connect-iq/monkey-c/coding-conventions/
