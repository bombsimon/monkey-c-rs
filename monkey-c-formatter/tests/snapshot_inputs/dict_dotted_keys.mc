function f() {
    var m = {
        Module.CONST => 1,
        Module.CONST_2 => {
            Module.CONST_3 => 1,
        },
        :sym => 2,
        "str" => 3,
        1 => "one",
    };
    var calls = { foo() => bar(1, 2) };
    var numbers = { 1 => 1, 2 => 2 };
}
