/*
Lorem ipsum dolor sit amet
*/

/* c1 */ import /* c2 */ Toybox.System /* c3 */; // c4

/* c1 */ module /* c2 */ Mod /* c3 */ { // c4
    // c
    /* c1 */ class /* c2 */ Cls /* c3 */ { // c4
        /* c1 */ function /* c2 */ Fn( /* c3 */ arg /* c4*/ as /* c5 */ Number /*c6 */) /* c7 */ as /* c8 */ Void /* c9 */ { // c10
            /* c1 */ var /* c2 */ x /* c3 */ as /* c4 */ Number /* c5 */ = /* c6 */ 1 /* c7 */; // c8
            /* c1 */ var y = new /* c2 */ Array<Number>[/* c3 */ 3 /* c4 */] /* c5 */; // c6

            /* c1 */ if /* c2 */ (/* c3 */ arg /* c4 */ > 3/* c5 */ ) /* c6 */ { // c7
                /* c1 */ System.println(/* c2 */ "x" /* c3 */);
            /* c1 */ } // c2

            if (arg /* c1 */ instanceof /* c2 */ Number) {
                System.println("x");
            }

            // c1
            for (/* c1 */ var /* c2 */ i /* c3 */ = /* c4 */ 0/* c5 */ ; /* c6 */ i /* c7 */ < /* c8 */ y.size() /* c9 */; /* c10 */ i++/* c11 */ ) {
                // c1
                y[/* c1 */ i /* c2 */] = i;
                // c1
            }
            // c1
        }

        function Fn2() as Number {
            /* c1 */ return /* c2 */ 1 /* c3 */; // c4
        }
    }
    // c1
}
/*
Lorem ipsum dolor sit amet
*/
