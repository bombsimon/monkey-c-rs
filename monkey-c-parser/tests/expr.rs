use monkey_c_parser::ast::{
    AssignOperator, Ast, BinaryOperator, DoubleLit, Expr, FloatLit, FunctionDecl, LiteralValue,
    Stmt, UnaryOperator,
};
use monkey_c_parser::parser::Parser;

/// Parse a standalone expression by wrapping it in a function return statement.
fn parse_expr(src: &str) -> Expr {
    let wrapped = format!("function f() {{ return {src}; }}");
    let ast = Parser::new(&wrapped).parse().expect("should parse").ast;
    if let Ast::Document(nodes, _) = ast
        && let Ast::Function(f) = nodes.into_iter().next().unwrap()
        && let Stmt::Return(r) = f.body.unwrap().stmts.into_iter().next().unwrap()
    {
        return r.value.unwrap();
    }
    panic!("could not extract expression");
}

fn parse_function(src: &str) -> FunctionDecl {
    let Ast::Document(nodes, _) = Parser::new(src).parse().expect("should parse").ast else {
        panic!("expected document");
    };
    nodes
        .into_iter()
        .find_map(|n| {
            if let Ast::Function(f) = n {
                Some(f)
            } else {
                None
            }
        })
        .expect("no function")
}

#[test]
fn test_literals() {
    for (src, expected) in [
        ("42", LiteralValue::Number(42)),
        ("42l", LiteralValue::Long(42)),
        ("42L", LiteralValue::Long(42)),
        (
            "1.5",
            LiteralValue::Float(FloatLit {
                digits: "1.5".into(),
                has_dot: true,
                leading_dot: false,
                has_suffix: false,
                exponent: None,
            }),
        ),
        (
            "1.5d",
            LiteralValue::Double(DoubleLit {
                digits: "1.5".into(),
                has_dot: true,
                leading_dot: false,
                exponent: None,
            }),
        ),
        ("true", LiteralValue::Boolean(true)),
        ("false", LiteralValue::Boolean(false)),
        ("null", LiteralValue::Null),
        ("NaN", LiteralValue::NaN),
    ] {
        let Expr::Lit(e) = parse_expr(src) else {
            panic!("expected Lit for {src}");
        };
        assert_eq!(e.value, expected, "literal {src}");
    }
}

#[test]
fn test_string_literal() {
    let Expr::Lit(e) = parse_expr(r#""hello""#) else {
        panic!("expected Lit");
    };
    assert_eq!(e.value, LiteralValue::String("hello".into()));
}

#[test]
fn test_identifier() {
    let Expr::Ident(e) = parse_expr("myVar") else {
        panic!("expected Ident");
    };
    assert_eq!(e.name, "myVar");
}

#[test]
fn test_scope_keywords() {
    assert!(matches!(parse_expr("me"), Expr::Me(_)));
    assert!(matches!(parse_expr("self"), Expr::Self_(_)));
}

#[test]
fn test_bling_global_scope() {
    let Expr::Member(m) = parse_expr("$.x") else {
        panic!("expected Member");
    };
    assert!(matches!(*m.object, Expr::Bling(_)));
}

#[test]
fn test_binary_operators() {
    for (src, expected) in [
        ("a + b", BinaryOperator::Add),
        ("a - b", BinaryOperator::Sub),
        ("a * b", BinaryOperator::Mul),
        ("a / b", BinaryOperator::Div),
        ("a % b", BinaryOperator::Mod),
        ("a == b", BinaryOperator::Eq),
        ("a != b", BinaryOperator::NotEq),
        ("a < b", BinaryOperator::Lt),
        ("a <= b", BinaryOperator::LtEq),
        ("a > b", BinaryOperator::Gt),
        ("a >= b", BinaryOperator::GtEq),
        ("a && b", BinaryOperator::And),
        ("a || b", BinaryOperator::Or),
        ("a & b", BinaryOperator::BitAnd),
        ("a | b", BinaryOperator::BitOr),
        ("a ^ b", BinaryOperator::BitXor),
        ("a instanceof Foo", BinaryOperator::InstanceOf),
    ] {
        let Expr::Binary(e) = parse_expr(src) else {
            panic!("expected Binary for `{src}`");
        };
        assert_eq!(e.operator, expected, "binary op in `{src}`");
    }
}

#[test]
fn test_prefix_unary_operators() {
    for (src, expected) in [
        ("+x", UnaryOperator::Pos),
        ("-x", UnaryOperator::Neg),
        ("!x", UnaryOperator::Not),
        ("~x", UnaryOperator::BitNot),
    ] {
        let Expr::Unary(e) = parse_expr(src) else {
            panic!("expected Unary for `{src}`");
        };
        assert_eq!(e.operator, expected, "unary op in `{src}`");
    }
}

#[test]
fn test_resource_ref() {
    // `@Rez.Strings.foo` — legacy resource reference, `@` applies to the
    // whole dotted member-access chain.
    let Expr::Unary(e) = parse_expr("@Rez.Strings.foo") else {
        panic!("expected Unary");
    };
    assert_eq!(e.operator, UnaryOperator::ResourceRef);
    assert!(matches!(e.operand.as_ref(), Expr::Member(_)));
}

#[test]
fn test_explicit_positive_number() {
    let Expr::Unary(e) = parse_expr("+1") else {
        panic!("expected Unary for `+1`");
    };
    assert_eq!(e.operator, UnaryOperator::Pos);
    assert!(matches!(*e.operand, Expr::Lit(_)));
}

#[test]
fn test_postfix_unary_operators() {
    for (src, expected) in [
        ("x++;", UnaryOperator::PostInc),
        ("x--;", UnaryOperator::PostDec),
        ("++x;", UnaryOperator::PreInc),
        ("--x;", UnaryOperator::PreDec),
    ] {
        let f = parse_function(&format!("function f() {{ {src} }}"));
        let Stmt::Expr(Expr::Unary(u)) = &f.body.as_ref().unwrap().stmts[0] else {
            panic!("expected unary statement for `{src}`");
        };
        assert_eq!(u.operator, expected, "unary op in `{src}`");
    }
}

#[test]
fn test_assignment_operators() {
    for (src, expected) in [
        ("x = 1;", AssignOperator::Assign),
        ("x += 1;", AssignOperator::AddAssign),
        ("x -= 1;", AssignOperator::SubAssign),
        ("x *= 1;", AssignOperator::MulAssign),
        ("x /= 1;", AssignOperator::DivAssign),
        ("x %= 1;", AssignOperator::ModAssign),
        ("x &= 1;", AssignOperator::BitAndAssign),
        ("x |= 1;", AssignOperator::BitOrAssign),
        ("x ^= 1;", AssignOperator::BitXorAssign),
    ] {
        let f = parse_function(&format!("function f() {{ {src} }}"));
        let Stmt::Expr(Expr::Assign(a)) = &f.body.as_ref().unwrap().stmts[0] else {
            panic!("expected Assign statement for `{src}`");
        };
        assert_eq!(a.operator, expected, "assign op in `{src}`");
    }
}

#[test]
fn test_operator_precedence() {
    // mul binds tighter than add: 1 + 2 * 3 → Add(1, Mul(2, 3))
    let Expr::Binary(e) = parse_expr("1 + 2 * 3") else {
        panic!("expected Binary");
    };
    assert_eq!(e.operator, BinaryOperator::Add);
    assert!(matches!(*e.right, Expr::Binary(ref r) if r.operator == BinaryOperator::Mul));
}

#[test]
fn test_new_array_untyped() {
    let Expr::NewArray(e) = parse_expr("new [size]") else {
        panic!("expected NewArray");
    };
    assert!(e.element_type.is_none());
}

#[test]
fn test_new_array_typed() {
    let Expr::NewArray(e) = parse_expr("new Array<Number>[size]") else {
        panic!("expected NewArray");
    };
    let ty = e.element_type.expect("typed array");
    assert_eq!(ty.ident().unwrap(), "Array");
    assert_eq!(ty.generic_params().len(), 1);
    assert_eq!(ty.generic_params()[0].ident().unwrap(), "Number");
}

#[test]
fn test_new_array_size_is_expression() {
    let Expr::NewArray(e) = parse_expr("new [20 + 30]") else {
        panic!("expected NewArray");
    };
    assert!(matches!(*e.size, Expr::Binary(_)));
}

#[test]
fn test_has_operator() {
    let Expr::Binary(e) = parse_expr("WatchUI has :WatchFaceDelegate") else {
        panic!("expected Binary");
    };
    assert_eq!(e.operator, BinaryOperator::Has);
}

#[test]
fn test_precedence_parens_override() {
    // (1 + 2) * 3 → Mul((1+2), 3)
    let Expr::Binary(e) = parse_expr("(1 + 2) * 3") else {
        panic!("expected Binary");
    };
    assert_eq!(e.operator, BinaryOperator::Mul);
}

#[test]
fn test_member_access() {
    let Expr::Member(e) = parse_expr("foo.bar") else {
        panic!("expected Member");
    };
    assert_eq!(e.property, "bar");
}

#[test]
fn test_chained_member_access() {
    let Expr::Member(outer) = parse_expr("a.b.c") else {
        panic!("expected Member");
    };
    assert_eq!(outer.property, "c");
    assert!(matches!(*outer.object, Expr::Member(ref m) if m.property == "b"));
}

#[test]
fn test_index_access() {
    assert!(matches!(parse_expr("arr[0]"), Expr::Index(_)));
}

#[test]
fn test_call_expressions() {
    let Expr::Call(e) = parse_expr("foo()") else {
        panic!("expected Call");
    };
    assert!(e.args.is_empty());

    let Expr::Call(e) = parse_expr("foo(1, 2, 3)") else {
        panic!("expected Call");
    };
    assert_eq!(e.args.len(), 3);

    // method call — callee is a Member
    let Expr::Call(e) = parse_expr("foo.bar(x)") else {
        panic!("expected Call");
    };
    assert!(matches!(*e.callee, Expr::Member(_)));
}

#[test]
fn test_bling_member_call() {
    let Expr::Call(e) = parse_expr("$.globalFn()") else {
        panic!("expected Call");
    };
    let Expr::Member(m) = *e.callee else {
        panic!("expected member callee");
    };
    assert!(matches!(*m.object, Expr::Bling(_)));
    assert_eq!(m.property, "globalFn");
}

#[test]
fn test_new_expressions() {
    let Expr::New(e) = parse_expr("new Foo()") else {
        panic!("expected New");
    };
    assert_eq!(e.class, "Foo");

    let Expr::New(e) = parse_expr("new MyModule.Foo()") else {
        panic!("expected New");
    };
    assert_eq!(e.class, "MyModule.Foo");
}

#[test]
fn test_new_expression_without_parens() {
    // `new Foo` (no argument list) is equivalent to `new Foo()`.
    let Expr::New(e) = parse_expr("new MyModule.Foo") else {
        panic!("expected New");
    };
    assert_eq!(e.class, "MyModule.Foo");
    assert!(e.args_open.is_none());
    assert!(e.args.is_empty());
}

#[test]
fn test_new_expression_with_self_reference() {
    // `new self.classDef_()` — instantiating via a `Lang.Class` reference
    // stored on `self`.
    let Expr::New(e) = parse_expr("new self.classDef_()") else {
        panic!("expected New");
    };
    assert_eq!(e.class, "self.classDef_");
}

#[test]
fn test_type_cast() {
    let Expr::TypeCast(e) = parse_expr("x as Number") else {
        panic!("expected TypeCast");
    };
    assert_eq!(e.target_type.ident().unwrap(), "Number");
}

#[test]
fn test_array_literals() {
    let Expr::Array(e) = parse_expr("[1, 2, 3]") else {
        panic!("expected Array");
    };
    assert_eq!(e.entries.len(), 3);
    assert!(!e.trailing_comma, "no trailing comma");

    let Expr::Array(e) = parse_expr("[1, 2, 3,]") else {
        panic!("expected Array");
    };
    assert_eq!(e.entries.len(), 3);
    assert!(e.trailing_comma, "trailing comma");

    let Expr::Array(e) = parse_expr("[]") else {
        panic!("expected Array");
    };
    assert_eq!(e.entries.len(), 0);
}

#[test]
fn test_dict_literals() {
    let Expr::Dict(e) = parse_expr(r#"{"key" => "value"}"#) else {
        panic!("expected Dict");
    };
    assert_eq!(e.entries.len(), 1);
    assert!(!e.trailing_comma, "no trailing comma");

    let Expr::Dict(e) = parse_expr(r#"{"key" => "value",}"#) else {
        panic!("expected Dict");
    };
    assert!(e.trailing_comma, "trailing comma");
}

#[test]
fn test_dict_key_expression() {
    let Expr::Dict(e) =
        parse_expr("{Activity.SPORT_GENERIC * 1000 + Activity.SUB_SPORT_GENERIC => \"generic\"}")
    else {
        panic!("expected Dict");
    };
    let entries: Vec<_> = e.entries.iter().collect();
    assert_eq!(entries.len(), 1);
    assert!(matches!(&entries[0].key, Expr::Binary(b) if b.operator == BinaryOperator::Add));
}

#[test]
fn test_null_check() {
    let Expr::Binary(e) = parse_expr("x == null") else {
        panic!("expected Binary");
    };
    assert_eq!(e.operator, BinaryOperator::Eq);
    assert!(matches!(*e.right, Expr::Lit(ref l) if l.value == LiteralValue::Null));
}

#[test]
fn test_symbol_literal() {
    let Expr::Lit(e) = parse_expr(":mySymbol") else {
        panic!("expected Lit");
    };
    assert_eq!(e.value, LiteralValue::Symbol("mySymbol".into()));
}

#[test]
fn test_symbol_dict_keys() {
    let Expr::Dict(e) = parse_expr(r#"{:title => "George", :name => "Taylor"}"#) else {
        panic!("expected Dict");
    };
    let entries: Vec<_> = e.entries.iter().collect();
    assert_eq!(entries.len(), 2);
    assert!(
        matches!(&entries[0].key, Expr::Lit(l) if l.value == LiteralValue::Symbol("title".into()))
    );
    assert!(
        matches!(&entries[1].key, Expr::Lit(l) if l.value == LiteralValue::Symbol("name".into()))
    );
}

#[test]
fn test_complex_expressions_parse() {
    for src in [
        "function f() { return (x * y) + (x / y); }",
        r#"function f() { var x = [arr[0].format("%d"), arr[1].format("%02d")]; }"#,
    ] {
        assert!(Parser::new(src).parse().is_ok(), "failed to parse: {src}");
    }
}

/// Pin down precedence between adjacent levels: an outer operator with a
/// tighter operator on the RHS should nest the tighter one inside. Together
/// with the existing `1 + 2 * 3` case in `test_operator_precedence`, this
/// covers each adjacent pair in the Java-style chain.
#[test]
fn test_operator_precedence_chain() {
    let cases = [
        // (src, outer op, inner op)
        ("a + b * c", BinaryOperator::Add, BinaryOperator::Mul),
        ("a << b + c", BinaryOperator::LeftShift, BinaryOperator::Add),
        ("a > b << c", BinaryOperator::Gt, BinaryOperator::LeftShift),
        ("a == b > c", BinaryOperator::Eq, BinaryOperator::Gt),
        ("a & b == c", BinaryOperator::BitAnd, BinaryOperator::Eq),
        ("a ^ b & c", BinaryOperator::BitXor, BinaryOperator::BitAnd),
        ("a | b ^ c", BinaryOperator::BitOr, BinaryOperator::BitXor),
        ("a && b | c", BinaryOperator::And, BinaryOperator::BitOr),
        ("a || b && c", BinaryOperator::Or, BinaryOperator::And),
    ];

    for (src, outer_op, inner_op) in cases {
        let expr = parse_expr(src);
        let Expr::Binary(outer) = &expr else {
            panic!("`{src}`: expected outer Binary");
        };
        assert_eq!(outer.operator, outer_op, "outer op for `{src}`");
        let Expr::Binary(rhs) = &*outer.right else {
            panic!("`{src}`: expected RHS Binary");
        };
        assert_eq!(rhs.operator, inner_op, "inner op for `{src}`");
    }
}

/// Left-associative operators should nest the leftmost pair deepest.
#[test]
fn test_left_associativity() {
    for (src, op) in [
        ("a - b - c", BinaryOperator::Sub),
        ("a / b / c", BinaryOperator::Div),
        ("a << b << c", BinaryOperator::LeftShift),
        ("a & b & c", BinaryOperator::BitAnd),
        ("a | b | c", BinaryOperator::BitOr),
    ] {
        let expr = parse_expr(src);
        let Expr::Binary(outer) = &expr else {
            panic!("`{src}`: expected outer Binary");
        };
        assert_eq!(outer.operator, op);
        let Expr::Binary(lhs) = &*outer.left else {
            panic!("`{src}`: expected LHS Binary (right-assoc would put it on the right)");
        };
        assert_eq!(lhs.operator, op);
    }
}

#[test]
fn test_ternary_no_whitespace() {
    for src in ["a?b:c", "a ? b : c"] {
        let Expr::Ternary(e) = parse_expr(src) else {
            panic!("`{src}`: expected Ternary");
        };
        assert!(matches!(*e.cond, Expr::Ident(_)));
        assert!(matches!(*e.then_expr, Expr::Ident(_)));
        assert!(matches!(*e.else_expr, Expr::Ident(_)));
    }
}
