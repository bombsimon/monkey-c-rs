using Toybox.Lang;

function tc() {
    try {
        /* Debug */ System.println("t");
    } catch (e) {
        /* Debug */ System.println("c");
    }
}
    
function tc2() {
    try {
        System.println("t");
    } catch (e) {
        // Nothing
    }
}
