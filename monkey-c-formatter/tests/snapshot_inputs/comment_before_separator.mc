var dict = {
    1 /* comment */ => "foo",
    2 // comment
        => "bar",
    3 => // comment
        "baz",
};

var array = [firstEntryInTheArray /* comment */, secondEntryInTheArray, /* comment */ thirdEntryInTheArray];

function f() {
    foo(firstArgument /* comment */, secondArgument /* comment */, thirdArgument, fourthArgument, fifthArgument);
    foo(firstArgument, secondArgument, /* comment */ thirdArgument, fourthArgument, fifthArgument, sixth);
    foo(first // comment
        , second);
    foo = 1 /* comment */;
    foo = 1 // comment
    ;
    for (var i = 0 /* comment */; i < 10 /* comment */; i++) {
    }
}
