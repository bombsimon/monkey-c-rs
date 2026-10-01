function f() {
    for (;;) {}
    for (var i = 0; ; i++) {}
    for (var i = 0; i < 1;) {}
    for (; i < 1;) {}
    for (; i < 1; i++) {}
    for (/* c */;;) {}
    for (;; /* c */) {}
}
