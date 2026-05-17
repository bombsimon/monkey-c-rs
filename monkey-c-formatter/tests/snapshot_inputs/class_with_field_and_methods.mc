class MyClass extends Base {
    private var mValue as Number = 0;

    public function initialize(value as Number) as Void {
        mValue = value;
    }

    function getValue() as Number {
        return mValue;
    }
}
