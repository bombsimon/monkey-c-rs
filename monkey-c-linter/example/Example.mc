using Toybox.Lang;
using MyModule.First;
using MyModule.Second;
import Toybox.System;
using Toybox.Graphics as Gfx;

class Example {
    const M = 10;
    const C = 20;
    var value = (M + C + 180 + 101.11d);

    public function initialize() as Void {}

    function myFunction(hours as Number) {
        var isPm = (hour >= 12);
        if ((1 + 2) + (3 + 4)) {
            System.println(isPm);
        }

        var anotherValue as String?;
        value = (valueEnabled ? "V on" : "V off");
    }
}

class Example2 extends Example {
    const X = 10;

    public function initialize(value as Number) as Void {}
}
