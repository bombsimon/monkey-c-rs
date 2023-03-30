/// Monkey C grammar to parse Monkey C source code.
/// Reference: https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/
#[macro_use]
extern crate pest_derive;

use pest::iterators::Pair;
use pest::Parser;

#[derive(Parser)]
#[grammar = "monkey_c.pest"]
pub struct MonkeyCParser;

#[derive(Debug, Clone)]
pub enum Ast {
    Document(Vec<Box<Ast>>),
    Ident(String),
    BasicLiteral(String),
    Using {
        import: Box<Ast>,
        alias: Option<Box<Ast>>,
    },
    Class {
        ident: Box<Ast>,
        statements: Vec<Box<Ast>>,
    },
    Assign {
        ident: Box<Ast>,
        value: Box<Ast>,
    },
    Empty,
}

pub fn parse(document: &str) -> Result<Ast, &'static str> {
    let mut pairs = MonkeyCParser::parse(Rule::document, document).unwrap();
    let ast = parse_value(pairs.next().unwrap());

    Ok(ast)
}

fn parse_value(pair: Pair<Rule>) -> Ast {
    match pair.as_rule() {
        Rule::document => {
            let mut inner = Vec::new();

            for pair in pair.into_inner() {
                inner.push(Box::new(parse_value(pair)));
            }

            Ast::Document(inner)
        }
        Rule::using => {
            let mut inner = pair.into_inner();

            Ast::Using {
                import: Box::new(parse_value(inner.next().unwrap())),
                alias: inner
                    .next()
                    .map(|i| Box::new(parse_value(i.into_inner().next().unwrap()))),
            }
        }
        Rule::class => {
            let mut pairs = pair.into_inner();
            let ident = Box::new(parse_value(pairs.next().unwrap()));

            let mut statements = Vec::new();
            for pair in pairs {
                statements.push(Box::new(parse_value(pair)));
            }

            Ast::Class { ident, statements }
        }
        Rule::assign => {
            let mut pairs = pair.into_inner();
            let ident = Box::new(parse_value(pairs.next().unwrap()));
            let value = Box::new(parse_value(pairs.next().unwrap()));

            Ast::Assign { ident, value }
        }
        Rule::alias => parse_value(pair.into_inner().next().unwrap()),
        Rule::ident => Ast::Ident(pair.as_str().to_owned()),
        Rule::basic_literal => Ast::BasicLiteral(pair.as_str().to_owned()),
        Rule::EOI => Ast::Empty,
        r => panic!("unknown rule: {:?}", r),
    }
}

/*
enum Keyword {
    And,
    As,
    Break,
    Catch,
    Case,
    Class,
    Const,
    Continue,
    Default,
    Do,
    Else,
    Enum,
    Extends,
    False,
    Finally,
    For,
    Function,
    Has,
    Hidden,
    If,
    InstanceOf,
    Me,
    Module,
    NaN,
    Native,
    New,
    Null,
    Or,
    Private,
    Protected,
    Public,
    Return,
    Self_,
    Static,
    Switch,
    Throw,
    True,
    Try,
    Using,
    Var,
    While,
}

enum ArithmeticOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulu,
    Increment,
    Decrement,
}

enum RelationalOperator {
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterOrEqual,
    LessOrEqual,
}

enum LogicalOperator {
    And,
    Or,
    Not,
}

enum BitwiseOperator {
    And,
    Or,
    Xor,
    Not,
}

enum AssignmentOperator {
    Assign,
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    LeftShift,
    RightShift,
    And,
    Or,
    Xor,
}

enum MiscellaneousOperator {
    TernaryIf,
    TernaryElse,
    New,
}
*/
