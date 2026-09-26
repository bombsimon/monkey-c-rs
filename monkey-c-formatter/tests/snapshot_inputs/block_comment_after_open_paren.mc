function f() {
    foo(/* c */ 1);
    foo( /* a */ /* b */ 1, 2);
    foo(/* c */);
    foo(/* c */ // d
        1);
    if (/* c */ x) {}
    var a = new /* ui */MenuItem("Cancel", null, 1, null);
    var b = new MenuItem(/* c */ 1);
    var c = new /* c */ Foo;
    var d = new /* c */ Foo([1, 2]);
    var e = new MenuItem /* c */ (1);
    var f = new MenuItem /* c */ ();
    var g = new Foo /* c */ ([1, 2]);
    var h = new // c
        Foo(1);
}
