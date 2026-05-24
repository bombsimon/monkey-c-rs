function fn() {
    switch (x) {
        case 1: /* block tag */
            do_thing();
        case 2: // line tag
            do_thing();
        default: /* tag */
            fallback();
    }
}
