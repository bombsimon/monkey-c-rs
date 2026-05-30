// Parens around a Method type let the `?` apply to the whole callable
// rather than to the return type.
class Foo {
    private var _callback as (Method(value as String) as Void)?;
}

typedef OptionalCb as (Method(x as Number) as Void)?;

// `(Method(…) as Return)` without a trailing `?` is accepted (and the
// parens are redundant — the linter flags this).
function f() as (Method() as Boolean) {
    return method(:g);
}
