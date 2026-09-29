typedef Pair as {
    :first as Number, // comment
    :last as Number // comment
};

typedef Flat as {:first as Number /* comment */, :last as Number};

enum {FOO, BAR} // comment

function f() {
    var a = 1, // comment
        b = 2;
    var c = 1, /* comment */ d = 2;
}
