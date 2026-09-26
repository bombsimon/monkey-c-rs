public class A {}
public enum B { X }

public module M {
    class W {
        private class C {}
        static hidden enum { Y }
        static private class D extends A {
            protected enum E { Z }
        }

        private enum {}
    }
}

class V {
    static public var a;
    public static var b;
    static /* why */ private function c() {}
    private // trailing
    static const D = 1;
}
