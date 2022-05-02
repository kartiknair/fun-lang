use crate::common::{Error, Span};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum TokenKind {
    String,
    Int,
    Float,
    Ident,
    Eof,

    // keywords
    Return,
    True,
    False,
    Null,
    If,
    Else,
    While,
    For,
    In,
    Err,
    Catch,

    // symbols
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    Comma,
    Dot,
    Colon,
    ColonEqual,
    Arrow,
    Bang,

    // binary operators
    Equal,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    Lesser,
    Greater,
    LesserEqual,
    GreaterEqual,
    EqualEqual,
    BangEqual,

    AndAnd,
    OrOr,

    Semicolon,
}

impl TokenKind {
    pub fn from_keyword_str(name: &str) -> Option<TokenKind> {
        match name {
            "return" => Some(TokenKind::Return),
            "true" => Some(TokenKind::True),
            "false" => Some(TokenKind::False),
            "null" => Some(TokenKind::Null),
            "if" => Some(TokenKind::If),
            "else" => Some(TokenKind::Else),
            "while" => Some(TokenKind::While),
            "for" => Some(TokenKind::For),
            "in" => Some(TokenKind::In),
            "err" => Some(TokenKind::Err),
            "catch" => Some(TokenKind::Catch),
            _ => None,
        }
    }

    pub fn is_prefix_op(&self) -> bool {
        matches!(*self, Self::Minus | Self::Bang)
    }

    pub fn is_binary_op(&self) -> bool {
        *self >= Self::Equal && *self < Self::OrOr
    }

    pub fn is_comparitive_op(&self) -> bool {
        *self > Self::Lesser && *self < Self::OrOr
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn error_at(&self, message: &str) -> Error {
        Error {
            span: self.span.clone(),
            message: message.into(),
        }
    }
}
