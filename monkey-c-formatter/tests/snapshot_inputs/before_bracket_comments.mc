enum /* A */ { TAG_FOO, TAG_BAR }

class Foo /* B */ {
    var x;
}

module M /* C */ {
    var y;
}

function f1() /* D */ {}

function f2() as Number /* E */ {
    return 1;
}

function f3() {
    if (x) /* F */ {
        doX();
    }

    if (x /* INSIDE */ ) {
        keepAsTrailing();
    }

    while (x) /* G */ {
        f();
    }

    do /* H */ {
        f();
    } while (x);

    for (var i = 0; i < 10; i += 1) /* I */ {
        f();
    }

    switch (x) /* J */ {
        case 1:
            f();
            break;
    }

    try /* K */ {
        f();
    }
    catch (e) {
        g();
    }
}
