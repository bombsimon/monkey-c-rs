fn main() -> Result<(), &'static str> {
    let document = r#"
using Toybox.System;
using Toybox.WatchUi as Ui;

using LocalPackage;

module MyMod {
    class MyClass {
        function myFunc(x as Number, y) {
            var a = 1;
            private var b = 2;
        }
    }
}
"#;
    let ast = monkey_c_rs::parse(document)?;
    println!("{:#?}", ast);

    println!("{}", ast.to_string());

    Ok(())
}
