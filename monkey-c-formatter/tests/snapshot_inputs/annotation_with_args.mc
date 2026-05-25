(:typecheck(false))
class C {}

(:test)
function bare() {}

(:foo(1, 2), :bar(:sym))
class D {}

(:debug(true))
module M {
    (:hidden)
    var x = 1;
}
