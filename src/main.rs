use monkey_c_fmt::{MonkeyCParser, Rule};
use pest::Parser;

fn main() {
    let cases = [
        (Rule::decl, "public var foo     = \"bar\";"),
        (Rule::list, "[ foo,\"bar\", 1]"),
        (Rule::decl, "var myList = [ foo,\"bar\", 1];"),
        (Rule::decl, r#"public var foo     = "bar and \"baz\"";"#),
    ];

    for (rule, tc) in cases {
        let pairs = MonkeyCParser::parse(rule, tc).unwrap_or_else(|e| panic!("{}", e));
        monkey_c_fmt::parse_and_print(pairs);
    }
}
