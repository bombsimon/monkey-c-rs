use crate::ast::{DoubleLit, FloatLit};

/// A token of Monkey C. Literals keep the text as written, and `Display` gives the token back as
/// source text.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference>
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // Keywords
    AndKeyword,
    As,
    Break,
    Case,
    Catch,
    Class,
    Const,
    Continue,
    Default,
    Do,
    Else,
    Enum,
    Extends,
    Finally,
    For,
    Function,
    Has,
    Hidden,
    If,
    Import,
    InstanceOf,
    Interface,
    Module,
    Native,
    New,
    OrKeyword,
    Private,
    Protected,
    Public,
    Return,
    Static,
    Switch,
    Throw,
    Try,
    Typedef,
    Using,
    Var,
    While,

    // Literals
    Boolean(bool),
    Char(String),
    Double(DoubleLit),
    Float(FloatLit),
    Hex(String),
    HexLong(String),
    Long(String),
    NaN,
    Null,
    Number(String),
    String(String),

    // Identifiers
    /// `$`, the global namespace.
    Bling,
    Identifier(String),
    Me,
    Self_,
    // Comments
    BlockComment(String),
    Comment(String),

    // Operators
    AddAssign,
    And,
    Assign,
    At,
    Bang,
    BangEqual,
    BitAnd,
    BitAndAssign,
    BitOr,
    BitOrAssign,
    BitXor,
    BitXorAssign,
    LeftShift,
    LeftShiftAssign,
    RightShift,
    RightShiftAssign,
    DivAssign,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Minus,
    MinusMinus,
    ModAssign,
    MulAssign,
    Or,
    Percent,
    Plus,
    PlusPlus,
    Slash,
    Star,
    SubAssign,
    Tilde,

    // Delimiters
    Colon,
    Comma,
    Dot,
    FatArrow,
    LBrace,
    LBracket,
    LParen,
    Question,
    RBrace,
    RBracket,
    RParen,
    Semicolon,

    // Special
    Eof,
    /// A character that can't start any token, consumed so lexing can go on.
    Illegal,
}

impl Type {
    /// Whether an expression can start with this token.
    pub fn starts_expression(&self) -> bool {
        matches!(
            self,
            Type::Bang
                | Type::Bling
                | Type::Boolean(_)
                | Type::Char(_)
                | Type::Colon
                | Type::Double(_)
                | Type::Float(_)
                | Type::Hex(_)
                | Type::HexLong(_)
                | Type::Identifier(_)
                | Type::LBrace
                | Type::LBracket
                | Type::LParen
                | Type::Long(_)
                | Type::Me
                | Type::Minus
                | Type::MinusMinus
                | Type::NaN
                | Type::New
                | Type::Null
                | Type::Number(_)
                | Type::Plus
                | Type::PlusPlus
                | Type::Self_
                | Type::String(_)
                | Type::Tilde
        )
    }

    /// Whether a `?` right after the type of an `as` cast makes the type nullable when this token
    /// follows it, rather than starting a ternary. A ternary needs an expression after its `?`,
    /// and a following `:` is read as nullable since `cond ? x as T? : y` is far more common than
    /// a ternary between two symbols.
    pub fn follows_nullable_cast(&self) -> bool {
        *self == Type::Colon || !self.starts_expression()
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Keywords
            Type::AndKeyword => write!(f, "and"),
            Type::As => write!(f, "as"),
            Type::Break => write!(f, "break"),
            Type::Case => write!(f, "case"),
            Type::Catch => write!(f, "catch"),
            Type::Class => write!(f, "class"),
            Type::Const => write!(f, "const"),
            Type::Continue => write!(f, "continue"),
            Type::Default => write!(f, "default"),
            Type::Do => write!(f, "do"),
            Type::Else => write!(f, "else"),
            Type::Enum => write!(f, "enum"),
            Type::Extends => write!(f, "extends"),
            Type::Finally => write!(f, "finally"),
            Type::For => write!(f, "for"),
            Type::Function => write!(f, "function"),
            Type::Has => write!(f, "has"),
            Type::Hidden => write!(f, "hidden"),
            Type::If => write!(f, "if"),
            Type::Import => write!(f, "import"),
            Type::InstanceOf => write!(f, "instanceof"),
            Type::Interface => write!(f, "interface"),
            Type::Module => write!(f, "module"),
            Type::Native => write!(f, "native"),
            Type::New => write!(f, "new"),
            Type::OrKeyword => write!(f, "or"),
            Type::Private => write!(f, "private"),
            Type::Protected => write!(f, "protected"),
            Type::Public => write!(f, "public"),
            Type::Return => write!(f, "return"),
            Type::Static => write!(f, "static"),
            Type::Switch => write!(f, "switch"),
            Type::Throw => write!(f, "throw"),
            Type::Try => write!(f, "try"),
            Type::Typedef => write!(f, "typedef"),
            Type::Using => write!(f, "using"),
            Type::Var => write!(f, "var"),
            Type::While => write!(f, "while"),

            // Literals
            Type::Boolean(b) => write!(f, "{b}"),
            Type::Char(s) => write!(f, "'{s}'"),
            Type::Double(n) => write!(f, "{}{}", n.digits, n.suffix),
            Type::Float(n) => write!(f, "{}", n.digits),
            Type::Hex(s) => write!(f, "0x{s}"),
            Type::HexLong(s) => write!(f, "0x{s}l"),
            Type::Long(n) => write!(f, "{n}l"),
            Type::NaN => write!(f, "NaN"),
            Type::Null => write!(f, "null"),
            Type::Number(n) => write!(f, "{n}"),
            Type::String(s) => write!(f, "{s}"),

            // Identifiers
            Type::Bling => write!(f, "$"),
            Type::Identifier(s) => write!(f, "{s}"),
            Type::Me => write!(f, "me"),
            Type::Self_ => write!(f, "self"),
            // Comments
            Type::BlockComment(s) => write!(f, "/*{s}*/"),
            Type::Comment(s) => write!(f, "//{s}"),

            // Operators
            Type::AddAssign => write!(f, "+="),
            Type::And => write!(f, "&&"),
            Type::Assign => write!(f, "="),
            Type::At => write!(f, "@"),
            Type::Bang => write!(f, "!"),
            Type::BangEqual => write!(f, "!="),
            Type::BitAnd => write!(f, "&"),
            Type::BitAndAssign => write!(f, "&="),
            Type::BitOr => write!(f, "|"),
            Type::BitOrAssign => write!(f, "|="),
            Type::BitXor => write!(f, "^"),
            Type::BitXorAssign => write!(f, "^="),
            Type::LeftShift => write!(f, "<<"),
            Type::LeftShiftAssign => write!(f, "<<="),
            Type::RightShift => write!(f, ">>"),
            Type::RightShiftAssign => write!(f, ">>="),
            Type::DivAssign => write!(f, "/="),
            Type::EqualEqual => write!(f, "=="),
            Type::Greater => write!(f, ">"),
            Type::GreaterEqual => write!(f, ">="),
            Type::Less => write!(f, "<"),
            Type::LessEqual => write!(f, "<="),
            Type::Minus => write!(f, "-"),
            Type::MinusMinus => write!(f, "--"),
            Type::ModAssign => write!(f, "%="),
            Type::MulAssign => write!(f, "*="),
            Type::Or => write!(f, "||"),
            Type::Percent => write!(f, "%"),
            Type::Plus => write!(f, "+"),
            Type::PlusPlus => write!(f, "++"),
            Type::Slash => write!(f, "/"),
            Type::Star => write!(f, "*"),
            Type::SubAssign => write!(f, "-="),
            Type::Tilde => write!(f, "~"),

            // Delimiters
            Type::Colon => write!(f, ":"),
            Type::Comma => write!(f, ","),
            Type::Dot => write!(f, "."),
            Type::FatArrow => write!(f, "=>"),
            Type::LBrace => write!(f, "{{"),
            Type::LBracket => write!(f, "["),
            Type::LParen => write!(f, "("),
            Type::Question => write!(f, "?"),
            Type::RBrace => write!(f, "}}"),
            Type::RBracket => write!(f, "]"),
            Type::RParen => write!(f, ")"),
            Type::Semicolon => write!(f, ";"),

            // Special
            Type::Eof => write!(f, "EOF"),
            Type::Illegal => write!(f, "ILLEGAL"),
        }
    }
}

impl Type {
    /// The keyword or keyword-like literal spelled `ident`, if any.
    pub fn keyword(ident: &str) -> Option<Self> {
        let keyword = match ident {
            "and" => Type::AndKeyword,
            "as" => Type::As,
            "break" => Type::Break,
            "case" => Type::Case,
            "catch" => Type::Catch,
            "class" => Type::Class,
            "const" => Type::Const,
            "continue" => Type::Continue,
            "default" => Type::Default,
            "do" => Type::Do,
            "else" => Type::Else,
            "enum" => Type::Enum,
            "extends" => Type::Extends,
            "finally" => Type::Finally,
            "for" => Type::For,
            "function" => Type::Function,
            "has" => Type::Has,
            "hidden" => Type::Hidden,
            "if" => Type::If,
            "import" => Type::Import,
            "instanceof" => Type::InstanceOf,
            "interface" => Type::Interface,
            "module" => Type::Module,
            "native" => Type::Native,
            "new" => Type::New,
            "or" => Type::OrKeyword,
            "private" => Type::Private,
            "protected" => Type::Protected,
            "public" => Type::Public,
            "return" => Type::Return,
            "static" => Type::Static,
            "switch" => Type::Switch,
            "throw" => Type::Throw,
            "try" => Type::Try,
            "typedef" => Type::Typedef,
            "using" => Type::Using,
            "var" => Type::Var,
            "while" => Type::While,

            "true" => Type::Boolean(true),
            "false" => Type::Boolean(false),
            "NaN" => Type::NaN,
            "null" => Type::Null,

            "me" => Type::Me,
            "self" => Type::Self_,
            _ => return None,
        };

        Some(keyword)
    }
}
