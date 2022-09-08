#[macro_use]
extern crate pest_derive;

#[derive(Parser)]
#[grammar = "monkey_c.pest"]
pub struct MonkeyCParser;

pub fn parse_and_print(pairs: pest::iterators::Pairs<Rule>) {
    for pair in pairs {
        // A pair is a combination of the rule which matched and a span of input
        println!("Rule:    {:?}", pair.as_rule());
        println!("Span:    {:?}", pair.as_span());
        println!("Text:    '{}'", pair.as_str());
        println!("\n");

        for inner_pair in pair.into_inner() {
            let name = match inner_pair.as_rule() {
                Rule::ident => "ident".to_string(),
                Rule::var => "var".to_string(),
                Rule::semi => "semi".to_string(),
                Rule::eq => "eq".to_string(),
                Rule::quote => "quote".to_string(),
                Rule::visibility => "visibility".to_string(),
                Rule::string_literal => "string_literal".to_string(),
                //Rule::list => parse_and_print(inner_pair.into_inner()),
                v => format!("{:?}", v),
            };

            print_pair(&name, inner_pair.as_str());
        }

        println!("\n\n");
    }
}

fn print_pair(prefix: &str, value: &str) {
    println!("{:<20} | {}", prefix, value);
}
