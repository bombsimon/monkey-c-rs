use monkey_c_parser::ast::{
    AssignOperator, Ast, BinaryExpr, BinaryOperator, Expr, FunctionDecl, LiteralValue, Stmt,
    UnaryExpr, UnaryOperator,
};
use monkey_c_parser::parser::Parser;

/// Parse a standalone expression by wrapping it in a function return statement.
fn parse_expr(src: &str) -> Expr {
    let wrapped = format!("function f() {{ return {src}; }}");
    let ast = Parser::new(&wrapped).parse().expect("should parse");
    if let Ast::Document(nodes) = ast {
        if let Ast::Function(f) = nodes.into_iter().next().unwrap() {
            if let Stmt::Return(r) = f.body.stmts.into_iter().next().unwrap() {
                return r.value.unwrap();
            }
        }
    }
    panic!("could not extract expression");
}

fn binary(src: &str) -> BinaryExpr {
    match parse_expr(src) {
        Expr::Binary(e) => e,
        other => panic!("expected Binary, got {other:?}"),
    }
}

fn unary(src: &str) -> UnaryExpr {
    match parse_expr(src) {
        Expr::Unary(e) => e,
        other => panic!("expected Unary, got {other:?}"),
    }
}

fn parse_function(src: &str) -> FunctionDecl {
    match Parser::new(src).parse().expect("should parse") {
        Ast::Document(nodes) => nodes
            .into_iter()
            .find_map(|n| {
                if let Ast::Function(f) = n {
                    Some(f)
                } else {
                    None
                }
            })
            .expect("no function"),
        _ => panic!("expected document"),
    }
}

#[test]
fn test_integer_literal() {
    match parse_expr("42") {
        Expr::Lit(e) => assert_eq!(e.value, LiteralValue::Long(42)),
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_float_literal() {
    match parse_expr("1.5") {
        Expr::Lit(e) => assert_eq!(e.value, LiteralValue::Double(1.5)),
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_string_literal() {
    match parse_expr(r#""hello""#) {
        Expr::Lit(e) => assert_eq!(e.value, LiteralValue::String("hello".into())),
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_boolean_true() {
    match parse_expr("true") {
        Expr::Lit(e) => assert_eq!(e.value, LiteralValue::Boolean(true)),
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_null() {
    assert!(matches!(
        parse_expr("null"),
        Expr::Lit(ref e) if e.value == LiteralValue::Null
    ));
}

#[test]
fn test_identifier() {
    match parse_expr("myVar") {
        Expr::Ident(e) => assert_eq!(e.name, "myVar"),
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_me() {
    assert!(matches!(parse_expr("me"), Expr::Me(_)));
}

#[test]
fn test_self_keyword() {
    assert!(matches!(parse_expr("self"), Expr::Self_(_)));
}

#[test]
fn test_bling() {
    // $ alone isn't a valid return value, test via member access
    assert!(matches!(parse_expr("$.x"), Expr::Member(_)));
    match parse_expr("$.x") {
        Expr::Member(m) => assert!(matches!(*m.object, Expr::Bling(_))),
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_addition() {
    let e = binary("a + b");
    assert_eq!(e.operator, BinaryOperator::Add);
}

#[test]
fn test_subtraction() {
    let e = binary("a - b");
    assert_eq!(e.operator, BinaryOperator::Sub);
}

#[test]
fn test_multiplication() {
    let e = binary("a * b");
    assert_eq!(e.operator, BinaryOperator::Mul);
}

#[test]
fn test_division() {
    let e = binary("a / b");
    assert_eq!(e.operator, BinaryOperator::Div);
}

#[test]
fn test_modulo() {
    let e = binary("a % b");
    assert_eq!(e.operator, BinaryOperator::Mod);
}

#[test]
fn test_equality() {
    let e = binary("a == b");
    assert_eq!(e.operator, BinaryOperator::Eq);
}

#[test]
fn test_not_equal() {
    let e = binary("a != b");
    assert_eq!(e.operator, BinaryOperator::NotEq);
}

#[test]
fn test_less_than() {
    let e = binary("a < b");
    assert_eq!(e.operator, BinaryOperator::Lt);
}

#[test]
fn test_greater_than() {
    let e = binary("a > b");
    assert_eq!(e.operator, BinaryOperator::Gt);
}

#[test]
fn test_logical_and() {
    let e = binary("a && b");
    assert_eq!(e.operator, BinaryOperator::And);
}

#[test]
fn test_logical_or() {
    let e = binary("a || b");
    assert_eq!(e.operator, BinaryOperator::Or);
}

#[test]
fn test_instanceof() {
    let e = binary("a instanceof Foo");
    assert_eq!(e.operator, BinaryOperator::InstanceOf);
}

#[test]
fn test_unary_negate() {
    let e = unary("-x");
    assert_eq!(e.operator, UnaryOperator::Neg);
}

#[test]
fn test_unary_not() {
    let e = unary("!x");
    assert_eq!(e.operator, UnaryOperator::Not);
}

#[test]
fn test_pre_increment() {
    // pre-increment not valid in return position — use via var statement
    let f = parse_function("function f() { var x = 0; ++x; }");
    assert!(matches!(f.body.stmts[1], Stmt::Expr(Expr::Unary(_))));
}

#[test]
fn test_post_increment() {
    let f = parse_function("function f() { var x = 0; x++; }");
    if let Stmt::Expr(Expr::Unary(u)) = &f.body.stmts[1] {
        assert_eq!(u.operator, UnaryOperator::PostInc);
    } else {
        panic!("expected post-increment");
    }
}

#[test]
fn test_post_decrement() {
    let f = parse_function("function f() { var x = 0; x--; }");
    if let Stmt::Expr(Expr::Unary(u)) = &f.body.stmts[1] {
        assert_eq!(u.operator, UnaryOperator::PostDec);
    } else {
        panic!("expected post-decrement");
    }
}

#[test]
fn test_precedence_mul_over_add() {
    // 1 + 2 * 3  should parse as  1 + (2 * 3)
    let e = binary("1 + 2 * 3");
    assert_eq!(e.operator, BinaryOperator::Add);
    assert!(matches!(*e.right, Expr::Binary(ref r) if r.operator == BinaryOperator::Mul));
}

#[test]
fn test_precedence_parens_override() {
    // (1 + 2) * 3 — top-level should be Mul
    let e = binary("(1 + 2) * 3");
    assert_eq!(e.operator, BinaryOperator::Mul);
}

#[test]
fn test_assignment_via_statement() {
    let f = parse_function("function f() { x = 1; }");
    if let Stmt::Expr(Expr::Assign(a)) = &f.body.stmts[0] {
        assert_eq!(a.operator, AssignOperator::Assign);
    } else {
        panic!("expected assignment statement");
    }
}

#[test]
fn test_add_assign() {
    let f = parse_function("function f() { x += 1; }");
    if let Stmt::Expr(Expr::Assign(a)) = &f.body.stmts[0] {
        assert_eq!(a.operator, AssignOperator::AddAssign);
    } else {
        panic!("expected += statement");
    }
}

#[test]
fn test_member_access() {
    match parse_expr("foo.bar") {
        Expr::Member(e) => assert_eq!(e.property, "bar"),
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_chained_member_access() {
    match parse_expr("a.b.c") {
        Expr::Member(outer) => {
            assert_eq!(outer.property, "c");
            assert!(matches!(*outer.object, Expr::Member(ref m) if m.property == "b"));
        }
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_index_access() {
    assert!(matches!(parse_expr("arr[0]"), Expr::Index(_)));
}

#[test]
fn test_function_call_no_args() {
    match parse_expr("foo()") {
        Expr::Call(e) => assert!(e.args.is_empty()),
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_function_call_with_args() {
    match parse_expr("foo(1, 2, 3)") {
        Expr::Call(e) => assert_eq!(e.args.len(), 3),
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_method_call_callee_is_member() {
    match parse_expr("foo.bar(x)") {
        Expr::Call(e) => assert!(matches!(*e.callee, Expr::Member(_))),
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_bling_member_call() {
    match parse_expr("$.globalFn()") {
        Expr::Call(e) => {
            if let Expr::Member(m) = *e.callee {
                assert!(matches!(*m.object, Expr::Bling(_)));
                assert_eq!(m.property, "globalFn");
            } else {
                panic!("expected member callee");
            }
        }
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_new_expression() {
    match parse_expr("new Foo()") {
        Expr::New(e) => assert_eq!(e.class, "Foo"),
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_new_dotted_class() {
    match parse_expr("new MyModule.Foo()") {
        Expr::New(e) => assert_eq!(e.class, "MyModule.Foo"),
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_type_cast() {
    match parse_expr("x as Number") {
        Expr::TypeCast(e) => assert_eq!(e.target_type.ident, "Number"),
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_array_no_trailing_comma() {
    match parse_expr("[1, 2, 3]") {
        Expr::Array(e) => {
            assert_eq!(e.elements.len(), 3);
            assert!(!e.trailing_comma);
        }
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_array_trailing_comma() {
    match parse_expr("[1, 2, 3,]") {
        Expr::Array(e) => {
            assert_eq!(e.elements.len(), 3);
            assert!(e.trailing_comma);
        }
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_empty_array() {
    match parse_expr("[]") {
        Expr::Array(e) => assert!(e.elements.is_empty()),
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_dict_no_trailing_comma() {
    match parse_expr(r#"{"key": "value"}"#) {
        Expr::Dict(e) => {
            assert_eq!(e.pairs.len(), 1);
            assert!(!e.trailing_comma);
        }
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_dict_trailing_comma() {
    match parse_expr(r#"{"key": "value",}"#) {
        Expr::Dict(e) => assert!(e.trailing_comma),
        other => panic!("{other:?}"),
    }
}

#[test]
fn test_null_check() {
    let e = binary("x == null");
    assert_eq!(e.operator, BinaryOperator::Eq);
    assert!(matches!(*e.right, Expr::Lit(ref l) if l.value == LiteralValue::Null));
}

#[test]
fn test_complex_expression_parses() {
    assert!(Parser::new("function f() { return (x * y) + (x / y); }")
        .parse()
        .is_ok());
}

#[test]
fn test_array_with_method_calls() {
    let src = r#"function f() {
        var x = [arr[0].format("%d"), arr[1].format("%02d")];
    }"#;
    assert!(Parser::new(src).parse().is_ok());
}
