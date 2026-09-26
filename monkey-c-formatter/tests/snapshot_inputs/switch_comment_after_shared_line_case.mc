function f(c) {
    switch (c) {
        case 1: case 2: // one or two
        case 3:         // three
            return;
        case 4: /* four */ case 5: // five
            break;
        case 6: case 7: /* seven */
        default: // default
            break;
    }
}
