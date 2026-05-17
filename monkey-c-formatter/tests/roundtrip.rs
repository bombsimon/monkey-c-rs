mod common;
use common::{format, format_aligned};

use monkey_c_parser::parser::Parser;

#[test]
fn formatted_class_is_reparseable() {
    let src = r#"
        class MyClass extends Base {
            private var mValue as Number = 0;

            public function initialize(value as Number) as Void {
                mValue = value;
            }

            function getValue() as Number {
                return mValue;
            }
        }
    "#;
    let formatted = format(src);

    assert!(
        Parser::new(&formatted).parse().is_ok(),
        "formatted output should be parseable: {formatted}",
    );
}

#[test]
fn aligned_dict_output_is_reparseable() {
    let src = r#"var d = {:foo => "a_longer_value", :not_foo => "another_longer_value"};"#;
    let formatted = format_aligned(src);

    assert!(
        Parser::new(&formatted).parse().is_ok(),
        "aligned output should be parseable: {formatted}",
    );
}
