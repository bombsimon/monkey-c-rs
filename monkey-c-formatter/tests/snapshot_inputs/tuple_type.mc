function getInitialView() as [Views] or [Views, InputDelegates] {
    return [new StartView(), new StartDelegate()];
}

function foo(x as [Number, Number, Number]) as [Number, Number, Number] {
    return x;
}

function nullable(p as [Number, String?]) as [Number, String?]? {
    return p;
}

typedef Pair as [Number, String];

typedef Nested as [Array<Number>, [Number, Number]];

class C {
    function tag() as [Number or Null, String] {
        return [null, "x"];
    }
}
