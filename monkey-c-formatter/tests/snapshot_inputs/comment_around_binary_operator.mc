function f() {
    foo = (first) // comment
        | (second);
    if (first ||
        // comment
        second) {
    }
    if (first
        // comment
        || second) {
    }
    if (first || // comment
        second) {
    }
    if (first /* comment */ || second) {
    }
}
