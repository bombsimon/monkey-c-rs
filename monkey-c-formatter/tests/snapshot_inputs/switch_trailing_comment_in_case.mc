function f() {
    switch (x) { // comment after open brace
        case 1:
            System.println("x"); // trailing comment
        case 2:
            foo();
    }
}
