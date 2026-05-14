use monkey_c_parser::ast::{
    Ast, ClassDecl, ConstDecl, Expr, FunctionDecl, Stmt, VarDecl, Visibility,
};
use monkey_c_parser::parser::{Parser, ParserError};

fn parse(src: &str) -> Ast {
    Parser::new(src).parse().expect("should parse")
}

fn parse_err(src: &str) -> ParserError {
    Parser::new(src).parse().expect_err("should fail")
}

fn document_nodes(src: &str) -> Vec<Ast> {
    match parse(src) {
        Ast::Document(nodes) => nodes,
        other => panic!("expected Document, got {other:?}"),
    }
}

fn first_class(src: &str) -> ClassDecl {
    document_nodes(src)
        .into_iter()
        .find_map(|n| if let Ast::Class(c) = n { Some(c) } else { None })
        .expect("no class found")
}

fn first_function(src: &str) -> FunctionDecl {
    document_nodes(src)
        .into_iter()
        .find_map(|n| {
            if let Ast::Function(f) = n {
                Some(f)
            } else {
                None
            }
        })
        .expect("no function found")
}

fn class_var(src: &str) -> VarDecl {
    first_class(src)
        .body
        .into_iter()
        .find_map(|n| {
            if let Ast::Variable(v) = n {
                Some(v)
            } else {
                None
            }
        })
        .expect("no var in class")
}

fn class_const(src: &str) -> ConstDecl {
    first_class(src)
        .body
        .into_iter()
        .find_map(|n| if let Ast::Const(c) = n { Some(c) } else { None })
        .expect("no const in class")
}

#[test]
fn test_empty_class() {
    let c = first_class("class Foo {}");
    assert_eq!(c.name, "Foo");
    assert!(c.body.is_empty());
}

#[test]
fn test_class_extends() {
    let c = first_class("class Foo extends Bar {}");
    assert_eq!(c.extends.as_deref(), Some("Bar"));
}

#[test]
fn test_var_name_and_type() {
    let v = class_var("class C { var x as Float; }");
    assert_eq!(v.name, "x");
    assert_eq!(v.type_.unwrap().ident, "Float");
}

#[test]
fn test_var_generic_type() {
    let v = class_var("class C { var items as Array<Number>; }");
    let ty = v.type_.unwrap();
    assert_eq!(ty.ident, "Array");
    assert_eq!(ty.generic_params[0].ident, "Number");
}

#[test]
fn test_var_initializer() {
    let v = class_var("class C { var x = 42; }");
    assert!(v.initializer.is_some());
}

#[test]
fn test_const_mandatory_initializer() {
    let c = class_const("class C { const PI = 3; }");
    assert_eq!(c.name, "PI");
    assert!(matches!(c.initializer, Expr::Lit(_)));
}

#[test]
fn test_const_with_type() {
    let c = class_const("class C { const MAX as Number = 100; }");
    assert_eq!(c.type_.unwrap().ident, "Number");
}

#[test]
fn test_static_const() {
    let c = class_const("class C { static const RATE = 1; }");
    assert!(c.is_static);
}

#[test]
fn test_visibility_private() {
    let v = class_var("class C { private var x; }");
    assert_eq!(v.visibility, Some(Visibility::Private));
}

#[test]
fn test_visibility_protected() {
    let v = class_var("class C { protected var x; }");
    assert_eq!(v.visibility, Some(Visibility::Protected));
}

#[test]
fn test_visibility_hidden_is_separate_variant() {
    let v = class_var("class C { hidden var x; }");
    assert_eq!(v.visibility, Some(Visibility::Hidden));
}

#[test]
fn test_visibility_public() {
    let v = class_var("class C { public var x; }");
    assert_eq!(v.visibility, Some(Visibility::Public));
}

#[test]
fn test_static_var() {
    let v = class_var("class C { static var x; }");
    assert!(v.is_static);
}

#[test]
fn test_function_args() {
    let f = first_class("class C { function foo(a as Number, b as String) {} }")
        .body
        .into_iter()
        .find_map(|n| {
            if let Ast::Function(f) = n {
                Some(f)
            } else {
                None
            }
        })
        .unwrap();
    assert_eq!(f.args.len(), 2);
    assert_eq!(f.args[0].name, "a");
    assert_eq!(f.args[0].type_.as_ref().unwrap().ident, "Number");
    assert_eq!(f.args[1].name, "b");
}

#[test]
fn test_function_return_type() {
    let f = first_function("function foo() as Void {}");
    assert_eq!(f.returns.unwrap().ident, "Void");
}

#[test]
fn test_function_visibility() {
    let f = first_class("class C { public function foo() {} }")
        .body
        .into_iter()
        .find_map(|n| {
            if let Ast::Function(f) = n {
                Some(f)
            } else {
                None
            }
        })
        .unwrap();
    assert_eq!(f.visibility, Some(Visibility::Public));
}

#[test]
fn test_static_function() {
    let f = first_class("class C { static function foo() {} }")
        .body
        .into_iter()
        .find_map(|n| {
            if let Ast::Function(f) = n {
                Some(f)
            } else {
                None
            }
        })
        .unwrap();
    assert!(f.is_static);
}

#[test]
fn test_import() {
    let nodes = document_nodes("import Toybox.WatchUi;");
    if let Ast::Import(d) = &nodes[0] {
        assert_eq!(d.name, "Toybox.WatchUi");
        assert!(d.alias.is_none());
    } else {
        panic!("expected import");
    }
}

#[test]
fn test_import_alias() {
    let nodes = document_nodes("import Toybox.WatchUi as WatchUi;");
    if let Ast::Import(d) = &nodes[0] {
        assert_eq!(d.alias.as_deref(), Some("WatchUi"));
    } else {
        panic!("expected import");
    }
}

#[test]
fn test_module() {
    let nodes = document_nodes("module MyModule { var x; }");
    if let Ast::Module(m) = &nodes[0] {
        assert_eq!(m.name, "MyModule");
        assert_eq!(m.body.len(), 1);
    } else {
        panic!("expected module");
    }
}

#[test]
fn test_nested_module() {
    let nodes = document_nodes("module Outer { module Inner { var x; } }");
    if let Ast::Module(outer) = &nodes[0] {
        assert_eq!(outer.name, "Outer");
        if let Ast::Module(inner) = &outer.body[0] {
            assert_eq!(inner.name, "Inner");
        } else {
            panic!("expected inner module");
        }
    }
}

#[test]
fn test_comment_is_separate_node() {
    let nodes = document_nodes("// a comment\nvar x = 1;");
    assert!(matches!(nodes[0], Ast::Comment(_, _)));
    assert!(matches!(nodes[1], Ast::Variable(_)));
}

#[test]
fn test_annotation_is_separate_node() {
    let nodes = document_nodes("(:test)\nfunction foo() {}");
    assert!(matches!(nodes[0], Ast::Annotation(_, _)));
}

#[test]
fn test_if_statement() {
    let f = first_function("function f() { if (x > 0) { return x; } }");
    assert!(matches!(f.body.stmts[0], Stmt::If(_)));
}

#[test]
fn test_if_else() {
    let f = first_function("function f() { if (x > 0) { return x; } else { return 0; } }");
    if let Stmt::If(s) = &f.body.stmts[0] {
        assert!(s.else_branch.is_some());
    }
}

#[test]
fn test_while_loop() {
    let f = first_function("function f() { while (x > 0) { x = x - 1; } }");
    assert!(matches!(f.body.stmts[0], Stmt::While(_)));
}

#[test]
fn test_for_loop() {
    let f = first_function("function f() { for (var i = 0; i < 10; i++) {} }");
    assert!(matches!(f.body.stmts[0], Stmt::For(_)));
}

#[test]
fn test_return_value() {
    let f = first_function("function f() { return 42; }");
    if let Stmt::Return(r) = &f.body.stmts[0] {
        assert!(r.value.is_some());
    }
}

#[test]
fn test_return_void() {
    let f = first_function("function f() { return; }");
    if let Stmt::Return(r) = &f.body.stmts[0] {
        assert!(r.value.is_none());
    }
}

#[test]
fn test_break_continue() {
    let f = first_function("function f() { for (;;) { break; continue; } }");
    if let Stmt::For(s) = &f.body.stmts[0] {
        assert!(matches!(s.body.stmts[0], Stmt::Break(_)));
        assert!(matches!(s.body.stmts[1], Stmt::Continue(_)));
    }
}

#[test]
fn test_var_span_start() {
    let nodes = document_nodes("var x = 1;");
    if let Ast::Variable(v) = &nodes[0] {
        assert_eq!(v.span.start, 0);
        assert!(v.span.end > 0);
    }
}

#[test]
fn test_var_span_offset() {
    let nodes = document_nodes("import Foo;\nvar x = 1;");
    if let Ast::Variable(v) = &nodes[1] {
        assert!(v.span.start > 0, "var on second line should not start at 0");
    }
}

#[test]
fn test_error_has_line_col() {
    let err = parse_err("class Foo { @invalid }");
    assert!(err.line > 0);
    assert!(err.col > 0);
}

#[test]
fn test_error_line_number() {
    let err = parse_err("class Foo {\n    @invalid\n}");
    assert_eq!(err.line, 2, "error should be on line 2");
}

#[test]
fn test_complex_method_chain_in_body() {
    let src = r#"class C { function f() {
        var pace = Lang.format("$1$", [x.format("%d")]);
    }}"#;
    assert!(Parser::new(src).parse().is_ok());
}

#[test]
fn test_type_cast_in_assignment() {
    let src = "class C { function f() { x = [0, 0] as Array<Number>; } }";
    assert!(Parser::new(src).parse().is_ok());
}

#[test]
fn test_new_expression() {
    let src = "function f() { var x = new MyModule.Foo(); }";
    let f = first_function(src);
    if let Stmt::Var(v) = &f.body.stmts[0] {
        assert!(matches!(v.initializer.as_deref(), Some(Expr::New(_))));
    }
}

#[test]
fn test_bling_global_access() {
    let src = "function f() { $.globalFn(); }";
    assert!(Parser::new(src).parse().is_ok());
}

#[test]
fn test_me_keyword() {
    let src = "class C { function f() { me.x = 1; } }";
    assert!(Parser::new(src).parse().is_ok());
}

#[test]
fn test_multiple_declarations_in_file() {
    let src = "import Foo;\nclass A {}\nclass B {}\nfunction f() {}";
    let nodes = document_nodes(src);
    assert_eq!(nodes.len(), 4);
}

#[test]
fn test_for_loop_with_binary_op() {
    let src = r#"
        function f() {
            var arr = [1, 2, 3];
            var sum = 0;
            for (var i = 0; i < arr.size(); i++) {
                sum = sum + arr[i];
            }
        }
    "#;
    assert!(Parser::new(src).parse().is_ok());
}
