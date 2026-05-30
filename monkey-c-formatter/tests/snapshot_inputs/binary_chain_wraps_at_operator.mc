class Example {
    function myFunction(hours as Number) {
        var x = functionOne("foo", "bar") + functionTwo("foo", "bar") + functionThree("foo", "bar") + functionOne("foo", "bar") + functionTwo("foo", "bar") + functionThree("foo", "bar");
    }
}

function mixed() {
    var y = aaaaaa * bbbbbb + cccccc * dddddd + eeeeee * ffffff + gggggg * hhhhhh;
}

function mixedAdditive() {
    var x = functionOne("foo", "bar") + functionTwo("foo", "bar") - functionThree("foo", "bar") + functionOne("foo", "bar") + functionTwo("foo", "bar") - functionThree("foo", "bar");
}

function mixedPrecedence() {
    var x = functionOne("foo", "bar") + functionTwo("foo", "bar") * functionThree("foo", "bar","foo", "bar","foo", "bar","foo", "bar","foo", "bar","foo", "bar","foo", "bar","foo", "bar") + functionOne("foo", "bar") + functionTwo("foo", "bar") - functionThree("foo", "bar");
}

function tightSubChainsStayInline() {
    var x = aaaaaaaa * bbbbbbbb + cccccccc * dddddddd - eeeeeeee * ffffffff + gggggggg * hhhhhhhh + iiiiiiii * jjjjjjjj;
}

function fits() {
    var z = a + b + c;
}
