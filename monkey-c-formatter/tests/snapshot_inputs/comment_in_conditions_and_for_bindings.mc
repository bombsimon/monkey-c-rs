function f() {
    do {
    } while /* c */ (x);
    do {
    } /* a */ while (x /* b */) /* c */;
    do {
    } while (x // c
    );
    if (x
        // c
    ) {
    }
    if (x // c
    ) {
    }
    for (var a = 1 /* c */, b = 2; a < b; a++) {
    }
    for (var a = 1, /* c */ b = 2; a < b; a++) {
    }
}
