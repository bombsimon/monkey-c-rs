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
    Empty,
    Comment(String),
    Document(Vec<Box<Ast>>),
    Ident(String),
    BasicLiteral(String),
    Using {
        import: Box<Ast>,
        alias: Option<Box<Ast>>,
    },
    Module {
        ident: Box<Ast>,
        body: Vec<Box<Ast>>,
    },
    Class {
        ident: Box<Ast>,
        extends: Option<Box<Ast>>,
        body: Vec<Box<Ast>>,
    },
    Function {
        ident: Box<Ast>,
        args: Vec<Box<Ast>>,
        body: Vec<Box<Ast>>,
    },
    Assign {
        target: Box<Ast>,
        value: Box<Ast>,
    },
    Call {
        function: Box<Ast>,
        args: Vec<Box<Ast>>,
    },
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
        Rule::function => {
            let mut pairs = pair.into_inner();
            let (ident, args) = match parse_value(pairs.next().unwrap()) {
                Ast::Call { function, args } => (function, args),
                _ => unreachable!(),
            };

            let mut body = Vec::new();
            for pair in pairs {
                body.push(Box::new(parse_value(pair)));
            }

            Ast::Function { ident, args, body }
        }
        Rule::module => {
            let mut pairs = pair.into_inner();
            let ident = Box::new(parse_value(pairs.next().unwrap()));

            let mut body = Vec::new();
            for pair in pairs {
                body.push(Box::new(parse_value(pair)));
            }

            Ast::Module { ident, body }
        }
        Rule::class => {
            let mut pairs = pair.into_inner();
            let ident = Box::new(parse_value(pairs.next().unwrap()));

            let next_pair = pairs.next();
            if next_pair.is_none() {
                return Ast::Class {
                    ident,
                    extends: None,
                    body: Default::default(),
                };
            }

            let (extends, mut statements) = match parse_value(next_pair.unwrap()) {
                ident @ Ast::Ident(_) => (Some(Box::new(ident)), vec![]),
                other => (None, vec![Box::new(other)]),
            };

            for pair in pairs {
                statements.push(Box::new(parse_value(pair)));
            }

            Ast::Class {
                ident,
                extends,
                body: statements,
            }
        }
        Rule::assign => {
            let mut pairs = pair.into_inner();
            let target = Box::new(parse_value(pairs.next().unwrap()));
            let value = Box::new(parse_value(pairs.next().unwrap()));

            Ast::Assign { target, value }
        }
        Rule::call => {
            let mut pairs = pair.into_inner();
            let function = Box::new(parse_value(pairs.next().unwrap()));

            let mut args = Vec::new();
            for pair in pairs {
                args.push(Box::new(parse_value(pair)));
            }

            Ast::Call { function, args }
        }
        Rule::alias => parse_value(pair.into_inner().next().unwrap()),
        Rule::ident => Ast::Ident(pair.as_str().to_owned()),
        Rule::basic_literal => Ast::BasicLiteral(pair.as_str().to_owned()),
        Rule::COMMENT => Ast::Comment(pair.as_str().to_owned()),
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
