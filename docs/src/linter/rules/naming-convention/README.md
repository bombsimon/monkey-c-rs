# `naming-convention`

Flags identifiers that don't follow Garmin's [Monkey C coding
conventions][conventions].

## Rules

| Category                               | Pattern                                      | Example                   |
| -------------------------------------- | -------------------------------------------- | ------------------------- |
| Modules & Classes                      | `PascalCase`                                 | `MyClass`                 |
| Functions & parameters                 | `camelCase`                                  | `myFunction(myArg)`       |
| Public class members                   | `camelCase`                                  | `var value;`              |
| Private/protected/hidden class members | `_camelCase`                                 | `private var _value;`     |
| Module-scope variables                 | `camelCase`                                  | `var counter = 0;`        |
| Enum variants                          | `SCREAMING_SNAKE_CASE` sharing a `<PREFIX>_` | `COLOR_RED`, `COLOR_BLUE` |

## Rationale

Naming conventions buy consistency at zero ongoing cost — readers don't
have to wonder whether `myThing` is a class or a function. The rules
above mirror Garmin's official conventions verbatim.

Two intentional gaps:

- **`const` declarations** are not checked. Constants conventionally use
  `SCREAMING_SNAKE_CASE` in Monkey C SDK code and an idiom like
  `example.SOME_CONST` reads better than `example.someConst`. The rule
  leaves both module-scope and class-scope `const` alone regardless of
  visibility.
- **Local variables** inside function bodies are not checked yet. Adding
  that requires walking `Stmt::Var` and is on the to-do list.

## Example

Before:

```monkey-c
class example {
    var Value = 1;

    private var mCounter as Number = 0;

    function MyFn() {}
}

enum {
    RED,
    BLUE,
}
```

The rule flags each violation and suggests a conformant name via
[`convert_case`][convert_case]:

```text
class `example` should be PascalCase, e.g. `Example`
public class member `Value` should be camelCase, e.g. `value`
private class member `mCounter` should be `_camelCase`, e.g. `_mCounter`
function `MyFn` should be camelCase, e.g. `myFn`
enum variants should share a common `<PREFIX>_` prefix
```

There is no auto-fix — renaming an identifier has to ripple through every
call site, which the linter can't do safely with byte-level replacements
alone.

[conventions]: https://developer.garmin.com/connect-iq/monkey-c/coding-conventions/
[convert_case]: https://crates.io/crates/convert_case
