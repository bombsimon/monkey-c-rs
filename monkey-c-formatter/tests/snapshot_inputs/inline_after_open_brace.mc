function fn() {
    if (foo = false) { // same line as `{`
        foo = true;
    }

    while (x) { /* block comment */
        do_thing();
    }

    if (cond) {
        // own line above first child
        body();
    }
}
