fn main() -> Result<(), &'static str> {
    let document = r#"
using Toybox.System;
using Toybox.WatchUi as Ui;

using LocalPackage;

module MyMod {
    // var x = 100 / 2;

    class MyClass {
        var a = someFn();
        var b = someFn(a, "a", 1, foo(bar(baz(1))));
        var foo = 1;
        var bar = "biz";
    }

    class Another extends MyModule.MyClass {
        var foo = 1;

        function myFunction(arg1, arg2) {
            var bar = 1;
        }
    }
}
"#;
    let ast = monkey_c_rs::parse(document)?;
    println!("{:#?}", ast);

    Ok(())
}
