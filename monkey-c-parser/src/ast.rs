#![allow(dead_code)]
type Ident = String;

#[derive(Debug)]
pub enum Ast {
    Document(Vec<Ast>),
    Import { name: Ident, alias: Option<Ident> },
}

fn parse(source: &str) -> Ast {
    let mut lex = crate::lexer::Lexer::new(source);

    let mut ast = Vec::new();

    loop {
        match lex.next_token() {
            (_, crate::token::Type::Newline, _) => continue,
            (_, crate::token::Type::Import, _) => {
                let mut name = String::new();
                let mut alias = None;
                let mut is_alias = false;

                loop {
                    let (_, tkn, _) = lex.next_token();
                    match tkn {
                        crate::token::Type::Identifier(i) => {
                            if is_alias {
                                alias = Some(i);
                            } else {
                                name.push_str(&i);
                            }
                        }
                        crate::token::Type::Dot => name.push('.'),
                        crate::token::Type::As => is_alias = true,
                        crate::token::Type::SemiColon => break,
                        t => panic!("unexpected token {t:?}"),
                    }
                }

                let import = Ast::Import { name, alias };
                ast.push(import);
            }
            _ => break,
        }
    }

    Ast::Document(ast)
}

#[cfg(test)]
mod test {
    #[test]
    fn test_parse_to_ast() {
        let input = r#"
            import Toybox.Graphics;
            import Toybox.WatchUi as Ui;
        "#;
        let ast = super::parse(input);

        println!("{ast:#?}");
    }
}
