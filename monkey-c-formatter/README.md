# `monkey-c-formatter`

A formatter for [Monkey C] that will write a consistent formatted output of the
source code. It supports wrapping and unwrapping lines as they fit in a
reasonable width but can, similar to [ruff], allow automatic multiline wrapping
of arrays and dictionaries by specifying a trailing newline.

```sh
› echo 'var x=[1,    2,3,];' | cargo run --example format
var x = [
    1,
    2,
    3,
];
```

```sh
› echo 'var x=[1,    2,3];' | cargo run --example format
var x = [1, 2, 3];
```

In addition to line breaks it currently has support to align the fat arrow
between key-value pairs in dictionaries.

```sh
› echo 'static const x = {:foo=>"bar", :not_foo=>"baz",};' | cargo run --example format
 static const x = {
    :foo:    => "bar",
    :not_foo => "baz",
};
```

It's backed by the [`monkey-c-parser`][parser] to parse the code.

[Monkey C]: https://developer.garmin.com/connect-iq/monkey-c/
[parser]: ../monkey-c-parser
[ruff]: https://docs.astral.sh/ruff/
