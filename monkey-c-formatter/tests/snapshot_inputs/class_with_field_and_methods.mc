class MyClass extends Base {
    private var _value as Number = 0;

    public function initialize(value as Number) as Void {
        _value = value;
    }

    function getValue() as Number {
        return _value;
    }
}
