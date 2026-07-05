//! Reader for the ConnectIQ SDK's `api.mir` — the standard library's symbols
//! with their signatures and documentation.
//!
//! `api.mir` is a machine-generated file of pseudo-Monkey-C: real declarations
//! with empty bodies (`function f() as Void {}`), nested in `module`/`class`/
//! `enum` scopes, but carrying SDK-only constructs the real language doesn't
//! have — `//!` doc comments, single-line `[@file = …; …]` attribute blocks,
//! and `<init> {}` slots — plus `type Name as …;` where source uses `typedef`.
//!
//! Rather than teach [`monkey_c_parser`] those SDK-isms, this is a focused
//! line reader. The file is regular enough to make that robust: attribute
//! blocks are always one line, real scopes close on a lone `}` (function
//! bodies are inline `{}`), and declarations end in `;` or `{}`.

/// What a [`Symbol`] declares.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Module,
    Class,
    Enum,
    EnumVariant,
    Function,
    Variable,
    Const,
    Type,
}

/// One standard-library symbol, as read from `api.mir`.
#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    /// Fully-qualified dotted path, e.g. `Toybox.System.println`.
    pub fqn: String,
    /// Final path segment, e.g. `println`.
    pub name: String,
    pub kind: SymbolKind,
    /// The declaration text, trimmed of its body/terminator, e.g.
    /// `public function println(output as $.Toybox.Lang.Object or Null) as Void`.
    pub signature: String,
    /// The `//!` doc block preceding the declaration, markers stripped.
    pub documentation: String,
    /// 1-based line of the declaration within `api.mir`.
    pub line: u32,
}

const MODIFIERS: [&str; 5] = ["public ", "protected ", "private ", "hidden ", "static "];

/// A scope on the stack. `name` is `None` for anonymous scopes (`<init>` slots)
/// that don't contribute to a fully-qualified path.
struct Scope {
    name: Option<String>,
    is_enum: bool,
}

/// Parse `api.mir` text into a flat list of symbols, in source order.
pub fn parse(source: &str) -> Vec<Symbol> {
    let mut symbols = Vec::new();
    let mut scopes: Vec<Scope> = Vec::new();
    let mut doc: Vec<String> = Vec::new();

    for (index, raw) in source.lines().enumerate() {
        let line_no = index as u32 + 1;
        let line = raw.trim();

        if line.is_empty() {
            continue;
        }

        if let Some(rest) = line.strip_prefix("//!") {
            doc.push(rest.strip_prefix(' ').unwrap_or(rest).to_string());
            continue;
        }

        // Attribute blocks sit between the doc and the declaration, so keep the
        // accumulated doc; other non-declarations reset it.
        if line.starts_with("[@") {
            continue;
        }

        if line == "}" {
            scopes.pop();
            doc.clear();
            continue;
        }

        // The lone `static` line precedes a `static <init>` slot.
        if line == "static" {
            continue;
        }

        if line.starts_with("<init>") {
            if opens_scope(line) {
                scopes.push(Scope {
                    name: None,
                    is_enum: false,
                });
            }
            doc.clear();
            continue;
        }

        let decl = strip_modifiers(line);
        let keyword = first_word(decl);

        if matches!(keyword, "module" | "class" | "enum") {
            if let Some(name) = ident_after(decl, keyword) {
                let kind = match keyword {
                    "module" => SymbolKind::Module,
                    "class" => SymbolKind::Class,
                    _ => SymbolKind::Enum,
                };
                symbols.push(symbol(&scopes, &name, kind, line, line_no, &mut doc));

                if opens_scope(line) {
                    scopes.push(Scope {
                        name: Some(name),
                        is_enum: kind == SymbolKind::Enum,
                    });
                }
            }
            continue;
        }

        if matches!(keyword, "function" | "var" | "const" | "type") {
            if let Some(name) = ident_after(decl, keyword) {
                let kind = match keyword {
                    "function" => SymbolKind::Function,
                    "var" => SymbolKind::Variable,
                    "const" => SymbolKind::Const,
                    _ => SymbolKind::Type,
                };
                symbols.push(symbol(&scopes, &name, kind, line, line_no, &mut doc));
            }
            doc.clear();
            continue;
        }

        // Inside an enum, any other identifier line is a variant.
        if scopes.last().is_some_and(|s| s.is_enum)
            && let Some(name) = leading_ident(decl)
        {
            symbols.push(symbol(
                &scopes,
                &name,
                SymbolKind::EnumVariant,
                line,
                line_no,
                &mut doc,
            ));
        }

        doc.clear();
    }

    symbols
}

/// Build a [`Symbol`] and drain the pending doc block into it.
fn symbol(
    scopes: &[Scope],
    name: &str,
    kind: SymbolKind,
    line: &str,
    line_no: u32,
    doc: &mut Vec<String>,
) -> Symbol {
    Symbol {
        fqn: fqn(scopes, name),
        name: name.to_string(),
        kind,
        signature: clean_signature(line),
        documentation: std::mem::take(doc).join("\n"),
        line: line_no,
    }
}

/// Whether a line opens a multi-line scope body (ends in a lone `{`, not `{}`).
fn opens_scope(line: &str) -> bool {
    line.ends_with('{')
}

fn strip_modifiers(mut decl: &str) -> &str {
    loop {
        match MODIFIERS.iter().find_map(|m| decl.strip_prefix(m)) {
            Some(rest) => decl = rest.trim_start(),
            None => return decl,
        }
    }
}

fn first_word(decl: &str) -> &str {
    decl.split(|c: char| !c.is_alphanumeric() && c != '_')
        .next()
        .unwrap_or("")
}

/// The identifier following `keyword` (e.g. `class Foo extends Bar` → `Foo`).
fn ident_after(decl: &str, keyword: &str) -> Option<String> {
    let rest = decl.strip_prefix(keyword)?.trim_start();
    leading_ident(rest)
}

fn leading_ident(text: &str) -> Option<String> {
    let ident: String = text
        .chars()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect();

    (!ident.is_empty()).then_some(ident)
}

fn fqn(scopes: &[Scope], name: &str) -> String {
    let mut path: Vec<&str> = scopes.iter().filter_map(|s| s.name.as_deref()).collect();
    path.push(name);

    path.join(".")
}

/// Trim a declaration line down to its signature: drop the trailing body
/// (`{}` / `{`), statement terminator (`;`), or variant separator (`,`).
fn clean_signature(line: &str) -> String {
    let line = line.trim();
    let trimmed = line
        .strip_suffix("{}")
        .or_else(|| line.strip_suffix('{'))
        .or_else(|| line.strip_suffix(';'))
        .or_else(|| line.strip_suffix(','))
        .unwrap_or(line);

    trimmed.trim().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r#"module Toybox {
    <init> {
    }
    //! The System module.
    //! @since 1.0.0
    [@file = "api/System.mb"; @line = 8; minSdk = "1.0.0"; ]
    module System {
        <init> {
        }
        //! Print to the console with a line terminator.
        //! @param output [Toybox::Lang::Object] The item to print.
        [@file = "api/System.mb"; @line = 788; minSdk = "1.0.0"; ]
        function println(output as $.Toybox.Lang.Object or Null) as Void {}
    }
    module UserProfile {
        <init> {
        }
        //! Sport heart-rate zones.
        public static enum SportHrZone {
            //! @since 1.2.6
            [@file = "api/UserProfile.mb"; @line = 34; ]
            HR_ZONE_SPORT_GENERIC = 0,
            HR_ZONE_SPORT_BIKING = 2,
        }
        class Profile {
            <init> {
            }
            static
            <init> {
            }
            //! The user's weight.
            [@file = "api/UserProfile.mb"; @line = 16; ]
            public var weight as $.Toybox.Lang.Number;
        }
    }
    module Lang {
        <init> {
        }
        type Numeric as $.Toybox.Lang.Number or $.Toybox.Lang.Float;
        class Exception {
            <init> {
            }
        }
        class InvalidValueException extends $.Toybox.Lang.Exception {
            <init> {
            }
            public const CODE as $.Toybox.Lang.Number = 1;
        }
    }
}
"#;

    fn find<'a>(symbols: &'a [Symbol], fqn: &str) -> &'a Symbol {
        symbols
            .iter()
            .find(|s| s.fqn == fqn)
            .unwrap_or_else(|| panic!("symbol {fqn} not found"))
    }

    #[test]
    fn resolves_nested_module_function() {
        let symbols = parse(SAMPLE);
        let println = find(&symbols, "Toybox.System.println");
        assert_eq!(println.kind, SymbolKind::Function);
        assert_eq!(println.name, "println");
        assert_eq!(
            println.signature,
            "function println(output as $.Toybox.Lang.Object or Null) as Void"
        );
        assert!(println.documentation.contains("Print to the console"));
        assert!(println.documentation.contains("@param output"));
    }

    #[test]
    fn modules_and_classes_are_recorded_as_scopes() {
        let symbols = parse(SAMPLE);
        assert_eq!(find(&symbols, "Toybox.System").kind, SymbolKind::Module);
        assert_eq!(
            find(&symbols, "Toybox.UserProfile.Profile").kind,
            SymbolKind::Class
        );
    }

    #[test]
    fn enum_and_variants_get_qualified_names() {
        let symbols = parse(SAMPLE);
        assert_eq!(
            find(&symbols, "Toybox.UserProfile.SportHrZone").kind,
            SymbolKind::Enum
        );
        let variant = find(
            &symbols,
            "Toybox.UserProfile.SportHrZone.HR_ZONE_SPORT_GENERIC",
        );
        assert_eq!(variant.kind, SymbolKind::EnumVariant);
        assert_eq!(variant.signature, "HR_ZONE_SPORT_GENERIC = 0");
    }

    #[test]
    fn init_slots_do_not_leak_into_paths() {
        let symbols = parse(SAMPLE);
        // `weight` is a member of Profile, not of an `<init>` scope.
        let weight = find(&symbols, "Toybox.UserProfile.Profile.weight");
        assert_eq!(weight.kind, SymbolKind::Variable);
        assert_eq!(
            weight.signature,
            "public var weight as $.Toybox.Lang.Number"
        );
        assert!(!symbols.iter().any(|s| s.fqn.contains("init")));
    }

    #[test]
    fn handles_type_const_and_extends() {
        let symbols = parse(SAMPLE);
        assert_eq!(find(&symbols, "Toybox.Lang.Numeric").kind, SymbolKind::Type);
        // A class with an `extends` clause still resolves by its own name.
        assert_eq!(
            find(&symbols, "Toybox.Lang.InvalidValueException").kind,
            SymbolKind::Class
        );
        let code = find(&symbols, "Toybox.Lang.InvalidValueException.CODE");
        assert_eq!(code.kind, SymbolKind::Const);
        assert_eq!(
            code.signature,
            "public const CODE as $.Toybox.Lang.Number = 1"
        );
    }

    #[test]
    fn does_not_record_anonymous_or_attribute_lines() {
        let symbols = parse(SAMPLE);
        assert!(symbols.iter().all(|s| !s.name.is_empty()));
        // The root module is recorded, everything else hangs off it.
        assert_eq!(find(&symbols, "Toybox").kind, SymbolKind::Module);
    }

    /// Smoke test against a real SDK `api.mir`. Opt-in: set `MONKEY_C_API_MIR`
    /// to its path. Run with `cargo test -- --ignored --nocapture`.
    #[test]
    #[ignore = "requires a local ConnectIQ SDK; set MONKEY_C_API_MIR"]
    fn parses_real_api_mir() {
        let path = std::env::var("MONKEY_C_API_MIR").expect("set MONKEY_C_API_MIR");
        let source = std::fs::read_to_string(&path).expect("read api.mir");
        let symbols = parse(&source);

        // Every symbol has a name, an ascii-dotted path, and a real line.
        assert!(symbols.iter().all(|s| !s.name.is_empty() && s.line > 0));
        assert!(
            symbols
                .iter()
                .all(|s| !s.fqn.contains("<") && !s.fqn.contains('['))
        );

        // Spot-check well-known members from the API docs.
        find(&symbols, "Toybox.System.println");
        find(&symbols, "Toybox.Lang.String.compareTo");

        let count = |kind| symbols.iter().filter(|s| s.kind == kind).count();
        eprintln!("total symbols: {}", symbols.len());
        eprintln!("  modules:   {}", count(SymbolKind::Module));
        eprintln!("  classes:   {}", count(SymbolKind::Class));
        eprintln!("  functions: {}", count(SymbolKind::Function));
        eprintln!("  variables: {}", count(SymbolKind::Variable));
        eprintln!("  consts:    {}", count(SymbolKind::Const));
        eprintln!("  enums:     {}", count(SymbolKind::Enum));
        eprintln!("  variants:  {}", count(SymbolKind::EnumVariant));
        eprintln!("  types:     {}", count(SymbolKind::Type));
    }
}
