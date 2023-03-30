fn main() -> Result<(), &'static str> {
    let document = r#"
using Toybox.System;
using Toybox.WatchUi as Ui;

using LocalPackage;

class MyClass {
    var a = someFn();
    var foo = 1;
    var bar = "biz";
}
"#;
    let ast = monkey_c_rs::parse(document)?;
    println!("{:#?}", ast);

    Ok(())
}
