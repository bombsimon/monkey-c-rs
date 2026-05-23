using Toybox.Lang;

enum /* NUMBERS */ {
    First,
    /* SECOND */ Second,
    Third,
}

class Comments {
    function fn() {
        dc.drawRoundedRectangle(x - /* LINE_WIDTH / 2) */ 1);

        if (LEVEL <= /* BATTERY_LEVEL_CRITICAL */ 10) {
            System.println("x");
        }

        if (LEVEL == 0 /* INFO */ || LEVEL > 10) {
            System.println("x");
        }

        if (LEVEL == 0 /* TRAILING */) {
            System.println("x");
        }

        if (true
            || // Another case
            false)
        {
            System.println("x");
        }

        var lineWidthPlusMargin = (/* BATTERY_LINE_WIDTH */ 2 + BATTERY_MARGIN);

        var arr = [
            /* FIRST */ 1,
            2,
            3,
        ];

        var dict = {
            :foo => /* INFO */ 1,
            /* INFO */ :bar => 2,
            :baz => 3,
        };
    }
}
