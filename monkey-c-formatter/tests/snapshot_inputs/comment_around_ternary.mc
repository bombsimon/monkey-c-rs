function f() {
    foo = condition ? firstValue /* comment */ : secondValueThatIsLongEnoughToBreakTheLine(argument);
    foo = condition
        ? firstValue // comment
        : secondValue;
    foo = condition // comment
        ? firstValue
        : secondValue;
    foo = condition ? /* comment */ firstValue : secondValue;
}
