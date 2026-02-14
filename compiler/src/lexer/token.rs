use crate::errors::Span;

/// All token types in the Momiji language
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    Def,
    End,
    If,
    Else,
    Elsif,
    While,
    For,
    In,
    Return,
    True,
    False,
    Nil,
    And,
    Or,
    Not,
    Struct,
    Class,
    Module,
    Import,
    Pub,
    Property,
    Match,
    When,
    Spawn,
    Select,
    Let,
    Mut,

    // Literals
    Int(i64),
    Float(f64),
    String(String),
    Identifier(String),

    // Operators
    Plus,      // +
    Minus,     // -
    Star,      // *
    Slash,     // /
    Percent,   // %
    Eq,        // =
    EqEq,      // ==
    BangEq,    // !=
    Lt,        // <
    LtEq,      // <=
    Gt,        // >
    GtEq,      // >=
    Arrow,     // ->
    FatArrow,  // =>
    Question,  // ?
    Bang,      // !
    Amp,       // &
    Pipe,      // |
    Caret,     // ^
    Tilde,     // ~
    LtLt,      // <<
    GtGt,      // >>
    AmpAmp,    // &&
    PipePipe,  // ||
    PlusEq,    // +=
    MinusEq,   // -=
    StarEq,    // *=
    SlashEq,   // /=
    At,        // @
    DotDot,    // ..
    DotDotDot, // ...

    // Delimiters
    LParen,     // (
    RParen,     // )
    LBracket,   // [
    RBracket,   // ]
    LBrace,     // {
    RBrace,     // }
    Comma,      // ,
    Dot,        // .
    Colon,      // :
    ColonColon, // ::
    Semicolon,  // ;
    Newline,    // \n (significant in some contexts)

    // Special
    Eof,
}

impl TokenKind {
    /// Returns the keyword for a given identifier, if any
    pub fn keyword(ident: &str) -> Option<TokenKind> {
        match ident {
            "def" => Some(TokenKind::Def),
            "end" => Some(TokenKind::End),
            "if" => Some(TokenKind::If),
            "else" => Some(TokenKind::Else),
            "elsif" => Some(TokenKind::Elsif),
            "while" => Some(TokenKind::While),
            "for" => Some(TokenKind::For),
            "in" => Some(TokenKind::In),
            "return" => Some(TokenKind::Return),
            "true" => Some(TokenKind::True),
            "false" => Some(TokenKind::False),
            "nil" => Some(TokenKind::Nil),
            "and" => Some(TokenKind::And),
            "or" => Some(TokenKind::Or),
            "not" => Some(TokenKind::Not),
            "struct" => Some(TokenKind::Struct),
            "class" => Some(TokenKind::Class),
            "module" => Some(TokenKind::Module),
            "import" => Some(TokenKind::Import),
            "pub" => Some(TokenKind::Pub),
            "property" => Some(TokenKind::Property),
            "match" => Some(TokenKind::Match),
            "when" => Some(TokenKind::When),
            "spawn" => Some(TokenKind::Spawn),
            "select" => Some(TokenKind::Select),
            "let" => Some(TokenKind::Let),
            "mut" => Some(TokenKind::Mut),
            _ => None,
        }
    }

    pub fn description(&self) -> &'static str {
        match self {
            TokenKind::Def => "'def'",
            TokenKind::End => "'end'",
            TokenKind::If => "'if'",
            TokenKind::Else => "'else'",
            TokenKind::Elsif => "'elsif'",
            TokenKind::While => "'while'",
            TokenKind::For => "'for'",
            TokenKind::In => "'in'",
            TokenKind::Return => "'return'",
            TokenKind::True => "'true'",
            TokenKind::False => "'false'",
            TokenKind::Nil => "'nil'",
            TokenKind::And => "'and'",
            TokenKind::Or => "'or'",
            TokenKind::Not => "'not'",
            TokenKind::Struct => "'struct'",
            TokenKind::Class => "'class'",
            TokenKind::Module => "'module'",
            TokenKind::Import => "'import'",
            TokenKind::Pub => "'pub'",
            TokenKind::Property => "'property'",
            TokenKind::Match => "'match'",
            TokenKind::When => "'when'",
            TokenKind::Spawn => "'spawn'",
            TokenKind::Select => "'select'",
            TokenKind::Let => "'let'",
            TokenKind::Mut => "'mut'",
            TokenKind::Int(_) => "integer",
            TokenKind::Float(_) => "float",
            TokenKind::String(_) => "string",
            TokenKind::Identifier(_) => "identifier",
            TokenKind::Plus => "'+'",
            TokenKind::Minus => "'-'",
            TokenKind::Star => "'*'",
            TokenKind::Slash => "'/'",
            TokenKind::Percent => "'%'",
            TokenKind::Eq => "'='",
            TokenKind::EqEq => "'=='",
            TokenKind::BangEq => "'!='",
            TokenKind::Lt => "'<'",
            TokenKind::LtEq => "'<='",
            TokenKind::Gt => "'>'",
            TokenKind::GtEq => "'>='",
            TokenKind::Arrow => "'->'",
            TokenKind::FatArrow => "'=>'",
            TokenKind::Question => "'?'",
            TokenKind::Bang => "'!'",
            TokenKind::Amp => "'&'",
            TokenKind::Pipe => "'|'",
            TokenKind::Caret => "'^'",
            TokenKind::Tilde => "'~'",
            TokenKind::LtLt => "'<<'",
            TokenKind::GtGt => "'>>'",
            TokenKind::AmpAmp => "'&&'",
            TokenKind::PipePipe => "'||'",
            TokenKind::PlusEq => "'+='",
            TokenKind::MinusEq => "'-='",
            TokenKind::StarEq => "'*='",
            TokenKind::SlashEq => "'/='",
            TokenKind::At => "'@'",
            TokenKind::DotDot => "'..'",
            TokenKind::DotDotDot => "'...'",
            TokenKind::LParen => "'('",
            TokenKind::RParen => "')'",
            TokenKind::LBracket => "'['",
            TokenKind::RBracket => "']'",
            TokenKind::LBrace => "'{'",
            TokenKind::RBrace => "'}'",
            TokenKind::Comma => "','",
            TokenKind::Dot => "'.'",
            TokenKind::Colon => "':'",
            TokenKind::ColonColon => "'::'",
            TokenKind::Semicolon => "';'",
            TokenKind::Newline => "newline",
            TokenKind::Eof => "end of file",
        }
    }
}

/// A token with its span in the source code
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}
