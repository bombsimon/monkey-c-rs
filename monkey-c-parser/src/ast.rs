#![allow(dead_code)]
pub type Ident = String;

#[derive(Debug, PartialEq)]
pub enum Visibility {
    Private,
    Protected,
    Public,
}

#[derive(Debug, PartialEq)]
pub struct Type {
    pub ident: Ident,
    pub optional: bool,
}

#[derive(Debug, PartialEq)]
pub struct Variable {
    pub name: Ident,
    pub type_: Option<Type>,
    pub visibility: Option<Visibility>,
}

#[derive(Debug, PartialEq)]
pub enum Ast {
    Document(Vec<Ast>),
    Import {
        name: Ident,
        alias: Option<Ident>,
    },
    BasicLit(String),
    Class {
        name: Ident,
        body: Vec<Ast>,
    },
    Function {
        name: Ident,
        args: Vec<Variable>,
        returns: Option<Type>,
        body: Vec<Ast>,
    },
    Variable(Variable),
    Assign {
        target: Box<Ast>,
        value: Box<Ast>,
    },
    EOF,
}
