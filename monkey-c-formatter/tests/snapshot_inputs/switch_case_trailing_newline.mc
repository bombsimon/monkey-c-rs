function fn() {
    switch (x) {
        case 1:
            do_thing();
        case 2:
            do_thing();

        case 3:
            do_thing();









        case 4:
            do_thing();
            // Comment
            do_thing();
            // Comment
        case 5:
            do_thing();
            // Comment

        case 6:
            do_thing();


            // Comment
        default:
            fallback();
    }
}


function f(n) {
  switch (n) {
    case 1:
      // Comment

    case 2:
      doThing();

    // Something about case 3
    case 3:
      doThing();

    // Even
    // Multiline
    case 4:
      doThing();
      // But not trailing with whitespace

    case 5:
      // Not even if empty

    case 6:
      doThing();

      // Or if mixed, it will be

    // Something like this
    case 7:
      doThing();
      // Indented

    // Not indented
    case 8:
    default:
  }
}
