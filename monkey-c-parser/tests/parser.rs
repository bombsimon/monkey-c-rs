use monkey_c_parser::ast::{
    Ast, ClassDecl, ConstDecl, ElseBranch, Expr, FunctionDecl, Stmt, VarDecl, Visibility,
};
use monkey_c_parser::parser::{Parser, ParserError};

fn parse(src: &str) -> Ast {
    Parser::new(src).parse().expect("should parse")
}

fn parse_err(src: &str) -> ParserError {
    Parser::new(src).parse().expect_err("should fail")
}

fn document_nodes(src: &str) -> Vec<Ast> {
    let Ast::Document(nodes) = parse(src) else {
        panic!("expected Document");
    };

    nodes
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
fn test_class() {
    let c = first_class("class Foo {}");
    assert_eq!(c.name, "Foo");
    assert!(c.body.is_empty());

    let c = first_class("class Foo extends Bar {}");
    assert_eq!(c.extends.as_deref(), Some("Bar"));
}

#[test]
fn test_var_declarations() {
    let v = class_var("class C { var x as Float; }");
    assert_eq!(v.name, "x");
    assert_eq!(v.type_.unwrap().ident().unwrap(), "Float");

    // `Array<Number or Float>` is one generic param whose type is a union,
    // not two comma-separated params. Confirm the corrected shape.
    let v = class_var("class C { var items as Array<Number or Float>; }");
    let ty = v.type_.unwrap();
    assert_eq!(ty.ident().unwrap(), "Array");
    assert_eq!(ty.generic_params().len(), 1);
    let param = &ty.generic_params()[0];
    assert_eq!(param.ident().unwrap(), "Number");
    assert_eq!(param.alternatives.len(), 1);
    assert_eq!(param.alternatives[0].ident().unwrap(), "Float");

    // `Dictionary<String, Number>` is two distinct generic params, comma-
    // separated. The old code conflated this with the union shape above.
    let v = class_var("class C { var d as Dictionary<String, Number>; }");
    let ty = v.type_.unwrap();
    assert_eq!(ty.generic_params().len(), 2);
    assert_eq!(ty.generic_params()[0].ident().unwrap(), "String");
    assert_eq!(ty.generic_params()[1].ident().unwrap(), "Number");
    assert!(ty.generic_params()[0].alternatives.is_empty());

    let v = class_var("class C { var x = 42; }");
    assert!(v.initializer.is_some());
}

#[test]
fn test_var_visibility() {
    for (src, expected) in [
        ("class C { private var x; }", Some(Visibility::Private)),
        ("class C { protected var x; }", Some(Visibility::Protected)),
        ("class C { hidden var x; }", Some(Visibility::Hidden)),
        ("class C { public var x; }", Some(Visibility::Public)),
        ("class C { var x; }", None),
    ] {
        let v = class_var(src);
        assert_eq!(v.visibility, expected, "visibility in `{src}`");
    }
}

#[test]
fn test_var_static() {
    assert!(class_var("class C { static var x; }").is_static);
    assert!(!class_var("class C { var x; }").is_static);
}

#[test]
fn test_const_declarations() {
    let c = class_const("class C { const PI = 3; }");
    assert_eq!(c.name, "PI");
    assert!(matches!(c.initializer, Expr::Lit(_)));

    let c = class_const("class C { const MAX as Number = 100; }");
    assert_eq!(c.type_.unwrap().ident().unwrap(), "Number");

    assert!(class_const("class C { static const RATE = 1; }").is_static);
}

#[test]
fn test_function_signature() {
    let f = first_function("function foo(a as Number, b as String) {}");
    assert_eq!(f.args.len(), 2);
    assert_eq!(f.args[0].name, "a");
    assert_eq!(f.args[0].type_.as_ref().unwrap().ident().unwrap(), "Number");
    assert_eq!(f.args[1].name, "b");

    let f = first_function("function foo() as Void {}");
    assert_eq!(f.returns.unwrap().ident().unwrap(), "Void");
}

#[test]
fn test_function_modifiers() {
    for (src, vis, is_static) in [
        ("public function foo() {}", Some(Visibility::Public), false),
        (
            "private function foo() {}",
            Some(Visibility::Private),
            false,
        ),
        ("static function foo() {}", None, true),
        (
            "static hidden function foo() {}",
            Some(Visibility::Hidden),
            true,
        ),
    ] {
        let f = first_function(src);
        assert_eq!(f.visibility, vis, "visibility in `{src}`");
        assert_eq!(f.is_static, is_static, "is_static in `{src}`");
    }
}

#[test]
fn test_imports() {
    let nodes = document_nodes("import Toybox.WatchUi;");
    let Ast::Import(d) = &nodes[0] else {
        panic!("expected import");
    };
    assert_eq!(d.name, "Toybox.WatchUi");
}

#[test]
fn test_import_rejects_alias() {
    // `import` doesn't support aliasing — only `using` does.
    let err = Parser::new("import Toybox.WatchUi as WatchUi;")
        .parse()
        .expect_err("should fail");
    assert!(
        err.message.contains("Semicolon") || err.message.to_lowercase().contains("as"),
        "unexpected error: {}",
        err.message
    );
}

#[test]
fn test_try_catch() {
    let f = first_function("function f() { try { foo(); } catch (e) { log(e); } }");
    let Stmt::Try(t) = &f.body.stmts[0] else {
        panic!("expected try");
    };
    assert_eq!(t.catches.len(), 1);
    assert_eq!(t.catches[0].binding, "e");
    assert!(t.catches[0].type_filter.is_none());
    assert!(t.finally.is_none());
}

#[test]
fn test_try_catch_typed_and_finally() {
    let src = "function f() { \
                  try { foo(); } \
                  catch (e instanceof MyException) { log(e); } \
                  catch (e) { other(e); } \
                  finally { cleanup(); } \
              }";
    let f = first_function(src);
    let Stmt::Try(t) = &f.body.stmts[0] else {
        panic!("expected try");
    };
    assert_eq!(t.catches.len(), 2);
    let filter = t.catches[0]
        .type_filter
        .as_ref()
        .expect("first catch should be typed");
    assert_eq!(filter.ident().unwrap(), "MyException");
    assert!(t.catches[1].type_filter.is_none());
    assert!(t.finally.is_some());
}

#[test]
fn test_try_without_catch_or_finally_fails() {
    let err = Parser::new("function f() { try { foo(); } }")
        .parse()
        .expect_err("should fail");
    assert!(
        err.message.contains("catch") || err.message.contains("finally"),
        "unexpected error: {}",
        err.message
    );
}

#[test]
fn test_throw() {
    let f = first_function("function f() { throw new Lang.Exception(); }");
    assert!(matches!(&f.body.stmts[0], Stmt::Throw(_)));
}

#[test]
fn test_ternary() {
    let f = first_function("function f() { var x = true ? 1 : 2; }");
    let Stmt::Var(v) = &f.body.stmts[0] else {
        panic!("expected var");
    };
    let Some(init) = &v.initializer else {
        panic!("expected initializer");
    };
    assert!(matches!(init.as_ref(), Expr::Ternary(_)));
}

#[test]
fn test_ternary_right_associative() {
    let f = first_function("function f() { var x = a ? b : c ? d : e; }");
    let Stmt::Var(v) = &f.body.stmts[0] else {
        panic!("expected var");
    };
    let Some(init) = &v.initializer else {
        panic!("expected initializer");
    };
    let Expr::Ternary(outer) = init.as_ref() else {
        panic!("expected outer ternary");
    };
    // Right-associative: else branch is itself a ternary.
    assert!(matches!(outer.else_expr.as_ref(), Expr::Ternary(_)));
}

#[test]
fn test_typedef() {
    let nodes = document_nodes("typedef Numeric as Number or Float or Long or Double;");
    let Ast::Typedef(d) = &nodes[0] else {
        panic!("expected typedef");
    };
    assert_eq!(d.name, "Numeric");
    assert_eq!(d.type_.ident().unwrap(), "Number");
    assert_eq!(d.type_.alternatives.len(), 3);
}

#[test]
fn test_using() {
    let nodes = document_nodes("using Toybox.Lang;");
    let Ast::Using(d) = &nodes[0] else {
        panic!("expected using");
    };
    assert_eq!(d.name, "Toybox.Lang");
    assert!(d.alias.is_none());

    let nodes = document_nodes("using Toybox.Lang as Lng;");
    let Ast::Using(d) = &nodes[0] else {
        panic!("expected using");
    };
    assert_eq!(d.name, "Toybox.Lang");
    assert_eq!(d.alias.as_deref(), Some("Lng"));
}

#[test]
fn test_module() {
    let nodes = document_nodes("module MyModule { var x; }");
    let Ast::Module(m) = &nodes[0] else {
        panic!("expected module");
    };
    assert_eq!(m.name, "MyModule");
    assert_eq!(m.body.len(), 1);
}

#[test]
fn test_nested_module() {
    let nodes = document_nodes("module Outer { module Inner { var x; } }");
    let Ast::Module(outer) = &nodes[0] else {
        panic!("expected module");
    };
    let Ast::Module(inner) = &outer.body[0] else {
        panic!("expected inner module");
    };
    assert_eq!(inner.name, "Inner");
}

#[test]
fn test_comment_and_annotation_are_separate_nodes() {
    let nodes = document_nodes("// a comment\nvar x = 1;");
    assert!(matches!(nodes[0], Ast::Comment(_, _)));
    assert!(matches!(nodes[1], Ast::Variable(_)));

    let nodes = document_nodes("(:test)\nfunction foo() {}");
    assert!(matches!(nodes[0], Ast::Annotation(_, _)));
}

#[test]
fn test_statement_types() {
    type Check = fn(&Stmt) -> bool;
    let cases: &[(&str, Check)] = &[
        ("if (x > 0) { return x; }", |s| matches!(s, Stmt::If(_))),
        ("while (x > 0) { x = x - 1; }", |s| {
            matches!(s, Stmt::While(_))
        }),
        ("for (var i = 0; i < 10; i++) {}", |s| {
            matches!(s, Stmt::For(_))
        }),
    ];
    for (src, check) in cases {
        let f = first_function(&format!("function f() {{ {src} }}"));
        assert!(check(&f.body.stmts[0]), "statement type for `{src}`");
    }
}

#[test]
fn test_if_else() {
    let f = first_function("function f() { if (x > 0) { return x; } else { return 0; } }");
    let Stmt::If(s) = &f.body.stmts[0] else {
        panic!("expected if");
    };
    assert!(matches!(s.else_branch, Some(ElseBranch::Block(_))));
}

#[test]
fn test_else_if_chain() {
    let f = first_function(
        "function f() { if (a) { return 1; } else if (b) { return 2; } else { return 3; } }",
    );
    let Stmt::If(outer) = &f.body.stmts[0] else {
        panic!("expected if");
    };
    let Some(ElseBranch::If(middle)) = &outer.else_branch else {
        panic!("expected else-if");
    };
    assert!(matches!(middle.else_branch, Some(ElseBranch::Block(_))));
}

#[test]
fn test_if_condition_with_member_access() {
    let f = first_function("function f() { if (obj.flag) { return 1; } }");
    let Stmt::If(s) = &f.body.stmts[0] else {
        panic!("expected if");
    };
    assert!(matches!(s.condition, Expr::Member(_)));
}

#[test]
fn test_dict_entry_trailing_comments_parse() {
    let f = first_function("function f() { var x = {:a => 1, :b => 2, // note\n}; }");
    let Stmt::Var(v) = &f.body.stmts[0] else {
        panic!("expected var");
    };
    let Some(init) = &v.initializer else {
        panic!("expected initializer");
    };
    let Expr::Dict(d) = init.as_ref() else {
        panic!("expected dict");
    };
    assert_eq!(d.entries.len(), 2);
    assert!(d.entries[0].trailing_comments.is_empty());
    assert_eq!(d.entries[1].trailing_comments.len(), 1);
    assert!(d.tail_comments.is_empty());
}

#[test]
fn test_empty_dict_with_tail_comment_parse() {
    let f = first_function("function f() { var x = {/* tail */}; }");
    let Stmt::Var(v) = &f.body.stmts[0] else {
        panic!("expected var");
    };
    let Some(init) = &v.initializer else {
        panic!("expected initializer");
    };
    let Expr::Dict(d) = init.as_ref() else {
        panic!("expected dict");
    };
    assert!(d.entries.is_empty());
    assert_eq!(d.tail_comments.len(), 1);
}

#[test]
fn test_if_else_with_trailing_comments() {
    let src = "function f() {\n\
                  if (a) { return 1; } // first\n\
                  else if (b) { return 2; } // second\n\
                  else { return 3; }\n\
              }";
    let f = first_function(src);
    let Stmt::If(outer) = &f.body.stmts[0] else {
        panic!("expected if");
    };
    assert_eq!(outer.trailing_comments.len(), 1);
    let Some(ElseBranch::If(middle)) = &outer.else_branch else {
        panic!("expected else-if");
    };
    assert_eq!(middle.trailing_comments.len(), 1);
}

#[test]
fn test_return_statements() {
    let f = first_function("function f() { return 42; }");
    let Stmt::Return(r) = &f.body.stmts[0] else {
        panic!("expected return");
    };
    assert!(r.value.is_some());

    let f = first_function("function f() { return; }");
    let Stmt::Return(r) = &f.body.stmts[0] else {
        panic!("expected return");
    };
    assert!(r.value.is_none());
}

#[test]
fn test_break_continue() {
    let f = first_function("function f() { for (;;) { break; continue; } }");
    let Stmt::For(s) = &f.body.stmts[0] else {
        panic!("expected for");
    };
    assert!(matches!(s.body.stmts[0], Stmt::Break(_)));
    assert!(matches!(s.body.stmts[1], Stmt::Continue(_)));
}

#[test]
fn test_span_accuracy() {
    let nodes = document_nodes("var x = 1;");
    let Ast::Variable(v) = &nodes[0] else {
        panic!("expected variable");
    };
    assert_eq!(v.span.start, 0);
    assert!(v.span.end > 0);

    let nodes = document_nodes("import Foo;\nvar x = 1;");
    let Ast::Variable(v) = &nodes[1] else {
        panic!("expected variable");
    };
    assert!(v.span.start > 0, "var on second line should not start at 0");
}

#[test]
fn test_error_location() {
    let err = parse_err("class Foo { @invalid }");
    assert!(err.line > 0);
    assert!(err.col > 0);

    let err = parse_err("class Foo {\n    @invalid\n}");
    assert_eq!(err.line, 2);
}

#[test]
fn test_complex_programs_parse() {
    let program = r#"
import Foo;

class A {}

class B {}

class C {
    function f1() {
        me.x = 1;
    }

    function f2() {
        var pace = Lang.format("$1$", [x.format("%d")]);
    }

    function f3() {
        x = [0, 0] as Array<Number>;
    }
}

function f1() {}

function f2() {
    var x = new MyModule.Foo();
}

function f3() {
    var arr = [1, 2, 3];
    for (var i = 0; i < arr.size(); i++) {
        arr[i] = arr[i] + 1;
    }
}
"#;

    assert!(
        Parser::new(program).parse().is_ok(),
        "failed to parse program"
    );
}
