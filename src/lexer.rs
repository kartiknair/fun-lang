use crate::{
    common::{Error, Span},
    token::{Token, TokenKind},
};

use unicode_xid::UnicodeXID;

#[derive(Debug, Clone)]
pub struct Lexer {
    pub source: Vec<char>,

    start: usize,
    current: usize,
    line: usize,
    line_begin: usize,
}

impl Lexer {
    pub fn from_str(source: &str) -> Self {
        Lexer {
            source: source.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
            line_begin: 0,
        }
    }

    pub fn from_chars(chars: Vec<char>) -> Self {
        Lexer {
            source: chars,
            start: 0,
            current: 0,
            line: 1,
            line_begin: 0,
        }
    }

    fn at_end(&mut self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn get_span(&self) -> Span {
        self.start..self.current
    }

    fn create_token(&mut self, kind: TokenKind) -> Token {
        Token {
            kind,
            span: self.get_span(),
        }
    }

    fn peek(&mut self) -> Result<char, Error> {
        if let Some(c) = self.source.get(self.current) {
            Ok(*c)
        } else {
            Err(Error {
                message: "unexpected end of file".into(),
                span: self.get_span(),
            })
        }
    }

    fn lex_string(&mut self, quote: char) -> Result<Token, Error> {
        self.start += 1; // skip the initial '"' in our token span

        while !self.at_end() && self.peek()? != quote {
            if self.peek()? == '\n' {
                return Err(Error {
                    message: "strings must be on a single line".into(),
                    span: self.get_span(),
                });
            }

            if self.peek()? == '\\' {
                self.advance();
                match self.peek()? {
                    'n' => {
                        self.source[self.current] = '\n';
                        self.source.remove(self.current - 1);
                        continue;
                    }
                    'r' => {
                        self.source[self.current] = '\r';
                        self.source.remove(self.current - 1);
                        continue;
                    }
                    't' => {
                        self.source[self.current] = '\t';
                        self.source.remove(self.current - 1);
                        continue;
                    }
                    '\'' => {
                        self.source[self.current] = '\'';
                        self.source.remove(self.current - 1);
                        continue;
                    }
                    '"' => {
                        self.source.remove(self.current - 1);
                        continue;
                    }
                    '\\' => {
                        self.source[self.current] = '\\';
                        self.source.remove(self.current - 1);
                        continue;
                    }
                    _ => {
                        return Err(Error {
                            message: "unrecognized escape sequence".into(),
                            span: self.get_span(),
                        });
                    }
                }
            }

            self.advance();
        }

        if self.at_end() {
            return Err(Error {
                message: "unterminated string literal".into(),
                span: self.get_span(),
            });
        }

        let token = self.create_token(TokenKind::String);
        self.advance(); // skip the closing: "
        Ok(token)
    }

    fn lex_number(&mut self) -> Result<Token, Error> {
        let radix = 10;
        let mut token_kind = TokenKind::Int;

        while !self.at_end() && self.peek()?.is_digit(radix) {
            self.advance()
        }

        if !self.at_end() && self.peek()? == '.' {
            self.current += 1;
            if self.peek()?.is_digit(radix) {
                token_kind = TokenKind::Float;
                while !self.at_end() && self.peek()?.is_digit(radix) {
                    self.advance()
                }
            } else {
                self.current -= 1;
            }
        }

        Ok(self.create_token(token_kind))
    }

    fn lex_ident(&mut self) -> Result<Token, Error> {
        while !self.at_end() && self.peek()?.is_xid_continue() {
            self.advance();
        }

        let lexeme = &self.source[self.start..self.current];
        let keyword_str = lexeme.iter().collect::<String>();

        let token = self.create_token(
            if let Some(keyword_kind) = TokenKind::from_keyword_str(&keyword_str) {
                keyword_kind
            } else {
                TokenKind::Ident
            },
        );

        Ok(token)
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, Error> {
        let mut tokens = Vec::new();

        while !self.at_end() {
            let c = self.peek()?;
            self.advance();

            match c {
                '(' => tokens.push(self.create_token(TokenKind::LeftParen)),
                ')' => tokens.push(self.create_token(TokenKind::RightParen)),
                '{' => tokens.push(self.create_token(TokenKind::LeftBrace)),
                '}' => tokens.push(self.create_token(TokenKind::RightBrace)),
                '[' => tokens.push(self.create_token(TokenKind::LeftBracket)),
                ']' => tokens.push(self.create_token(TokenKind::RightBracket)),
                ',' => tokens.push(self.create_token(TokenKind::Comma)),
                '.' => tokens.push(self.create_token(TokenKind::Dot)),
                ':' => {
                    if self.peek()? == '=' {
                        self.advance();
                        tokens.push(self.create_token(TokenKind::ColonEqual));
                    } else {
                        tokens.push(self.create_token(TokenKind::Colon));
                    }
                }
                '&' => {
                    if self.peek()? == '&' {
                        self.advance();
                        tokens.push(self.create_token(TokenKind::AndAnd));
                    } else {
                        return Err(Error {
                            span: self.get_span(),
                            message: "expected '&' after '&'".into(),
                        });
                    }
                }
                '|' => {
                    if self.peek()? == '|' {
                        self.advance();
                        tokens.push(self.create_token(TokenKind::OrOr))
                    } else {
                        return Err(Error {
                            span: self.get_span(),
                            message: "expected '|' after '|'".into(),
                        });
                    }
                }
                '=' => {
                    if self.peek()? == '=' {
                        self.advance();
                        tokens.push(self.create_token(TokenKind::EqualEqual));
                    } else {
                        tokens.push(self.create_token(TokenKind::Equal));
                    }
                }
                '!' => {
                    if self.peek()? == '=' {
                        self.advance();
                        tokens.push(self.create_token(TokenKind::BangEqual));
                    } else {
                        tokens.push(self.create_token(TokenKind::Bang));
                    }
                }
                '<' => {
                    if self.peek()? == '=' {
                        self.advance();
                        tokens.push(self.create_token(TokenKind::LesserEqual));
                    } else {
                        tokens.push(self.create_token(TokenKind::Lesser));
                    }
                }
                '>' => {
                    if self.peek()? == '=' {
                        self.advance();
                        tokens.push(self.create_token(TokenKind::GreaterEqual))
                    } else {
                        tokens.push(self.create_token(TokenKind::Greater))
                    }
                }
                '-' => {
                    if self.peek()? == '>' {
                        self.advance();
                        tokens.push(self.create_token(TokenKind::Arrow))
                    } else {
                        tokens.push(self.create_token(TokenKind::Minus))
                    }
                }
                '+' => tokens.push(self.create_token(TokenKind::Plus)),
                '*' => tokens.push(self.create_token(TokenKind::Star)),
                '%' => tokens.push(self.create_token(TokenKind::Percent)),

                '/' => {
                    if self.peek()? == '/' {
                        self.advance();
                        while !self.at_end() && self.peek()? != '\n' {
                            self.advance();
                        }
                    } else {
                        tokens.push(self.create_token(TokenKind::Slash))
                    }
                }

                ';' => tokens.push(self.create_token(TokenKind::Semicolon)),

                '"' => tokens.push(self.lex_string(c)?),
                '\'' => tokens.push(self.lex_string(c)?),

                '\n' => {
                    if let Some(prev_token) = tokens.last() {
                        if prev_token.kind != TokenKind::Semicolon
                            && (prev_token.kind == TokenKind::Ident
                                || prev_token.kind == TokenKind::RightParen
                                || prev_token.kind == TokenKind::RightBrace
                                || prev_token.kind == TokenKind::RightBracket
                                || prev_token.kind == TokenKind::Return
                                || prev_token.kind == TokenKind::Int
                                || prev_token.kind == TokenKind::Float
                                || prev_token.kind == TokenKind::True
                                || prev_token.kind == TokenKind::False
                                || prev_token.kind == TokenKind::Null
                                || prev_token.kind == TokenKind::String)
                        {
                            tokens.push(self.create_token(TokenKind::Semicolon))
                        }
                    }
                }

                _ if c.is_whitespace() => {
                    // do nothing
                }

                _ => {
                    if c.is_digit(10) {
                        tokens.push(self.lex_number()?)
                    } else if c.is_xid_start() {
                        tokens.push(self.lex_ident()?)
                    } else {
                        return Err(Error {
                            message: "unexpected character".into(),
                            span: self.get_span(),
                        });
                    }
                }
            };

            self.start = self.current;
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            span: 0..0,
        });

        Ok(tokens)
    }
}
