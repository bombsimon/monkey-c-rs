enum Foo {
    FIRST = 1,
    SECOND = 2,
    THIRD_ONE = 3,
}

enum {
    SHORT = 1,
    LONGER_NAME = 22,
    LONGEST_NAME_HERE = 333,
}

// No alignment when not every variant in the run has a value: the
// run [B = 5, C = 6] is 2 entries — they get aligned with each other
// but the no-value variants A and D stay as-is.
enum Mixed {
    A,
    B = 5,
    C = 6,
    D,
}

// A single variant with a value isn't padded — alignment requires
// at least two consecutive value-bearing entries.
enum One {
    A,
    B = 5,
    C,
}
