class Foo {
    private var _callback as (Method(value as String) as Void)?;
}

typedef OptionalCb as (Method(x as Number) as Void)?;

function f() as (Method() as Boolean) {
    return method(:g);
}
