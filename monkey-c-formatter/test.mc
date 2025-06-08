// Test class with comments
class TestClass extends BaseClass {
    // Private field
    private var count as Number;

    // Public method
    public function initialize() as Void {
        count = 0;
    }

    // Method with parameters
    function increment(amount as Number) as Number {
        count = count + amount;
        return count;
    }

    // Method with complex expressions
    function complexMath(x as Number, y as Number) as Number {
        var result = (x * y) + (x / y);
        if (result > 100) {
            return result;
        } else {
            return result * 2;
        }
    }

    // Method with arrays and dictionaries
    function collections() as Void {
        var arr = [1, 2, 3, 4, 5];
        var dict = {"key1": "value1", "key2": "value2"};
        var sum = 0;
        for (var i = 0; i < arr.size(); i++) {
            sum = sum + arr[i];
        }
    }
} 
