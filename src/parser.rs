use crate::{
    ast,
    common::Error,
    token::{Token, TokenKind},
};

#[derive(Debug, Clone)]
struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Assoc {
    Ltr,
    Rtl,
}

#[derive(Debug, Clone, Copy)]
struct OpInfo {
    prec: u8,
    assoc: Assoc,
}

impl TokenKind {
    fn op_info(&self) -> OpInfo {
        match self {
            Self::Percent => OpInfo {
                prec: 6,
                assoc: Assoc::Ltr,
            },
            Self::Star => OpInfo {
                prec: 6,
                assoc: Assoc::Ltr,
            },
            Self::Slash => OpInfo {
                prec: 6,
                assoc: Assoc::Ltr,
            },

            Self::Plus => OpInfo {
                prec: 5,
                assoc: Assoc::Ltr,
            },
            Self::Minus => OpInfo {
                prec: 5,
                assoc: Assoc::Ltr,
            },

            Self::Lesser => OpInfo {
                prec: 4,
                assoc: Assoc::Ltr,
            },
            Self::LesserEqual => OpInfo {
                prec: 4,
                assoc: Assoc::Ltr,
            },
            Self::Greater => OpInfo {
                prec: 4,
                assoc: Assoc::Ltr,
            },
            Self::GreaterEqual => OpInfo {
                prec: 4,
                assoc: Assoc::Ltr,
            },

            Self::EqualEqual => OpInfo {
                prec: 3,
                assoc: Assoc::Ltr,
            },
            Self::BangEqual => OpInfo {
                prec: 3,
                assoc: Assoc::Ltr,
            },

            Self::AndAnd => OpInfo {
                prec: 2,
                assoc: Assoc::Ltr,
            },
            Self::OrOr => OpInfo {
                prec: 2,
                assoc: Assoc::Ltr,
            },

            Self::Equal => OpInfo {
                prec: 1,
                assoc: Assoc::Ltr,
            },

            _ => {
                panic!("`op_info()` has not been implemented for token: {:?}", self)
            }
        }
    }
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, current: 0 }
    }

    fn peek(&self) -> Result<&Token, Error> {
        if let Some(token) = self.tokens.get(self.current) {
            Ok(token)
        } else {
            Err(Error {
                message: "unexpected end of file".into(),
                span: self.tokens.last().unwrap().span.clone(),
            })
        }
    }

    fn error_at_current(&self, message: &str) -> Error {
        Error {
            message: message.into(),
            span: {
                if let Some(token) = self.tokens.get(self.current) {
                    token.span.clone()
                } else {
                    self.tokens.last().unwrap().span.clone()
                }
            },
        }
    }

    fn expect(&mut self, kind: TokenKind, message: &str) -> Result<&Token, Error> {
        if let Some(token) = self.tokens.get(self.current) {
            if token.kind == kind {
                self.current += 1;
                Ok(token)
            } else {
                Err(Error {
                    message: message.into(),
                    span: token.span.clone(),
                })
            }
        } else {
            Err(Error {
                message: message.into(),
                span: self.tokens.last().unwrap().span.clone(),
            })
        }
    }

    fn parse_block(&mut self) -> Result<ast::Block, Error> {
        self.expect(TokenKind::LeftBrace, "expect block")?;

        let mut stmts = Vec::new();
        while self.peek()?.kind != TokenKind::RightBrace {
            let expr = self.parse_expr()?;
            self.expect(TokenKind::Semicolon, "expect ';' after expression in block")?;
            stmts.push(expr);
        }

        self.expect(TokenKind::RightBrace, "unclosed block")?;

        Ok(ast::Block { stmts })
    }

    fn parse_primary(&mut self) -> Result<ast::Expr, Error> {
        let mut expr = None;

        let token = self.peek()?.clone();

        match &token.kind {
            TokenKind::LeftParen => {
                let start = self.current;

                self.current += 1;

                if self.peek()?.kind != TokenKind::RightParen
                    && self.peek()?.kind != TokenKind::Ident
                {
                    // Not a function literal
                    let expr_within_grouping = self.parse_expr()?;
                    self.expect(TokenKind::RightParen, "missing closing ')' in grouping")?;
                    expr = Some(expr_within_grouping)
                } else {
                    let mut parameters = Vec::new();
                    if self.peek()?.kind != TokenKind::RightParen {
                        loop {
                            let param_ident = self
                                .expect(TokenKind::Ident, "expect parameter name")?
                                .clone();
                            parameters.push(param_ident);

                            if self.peek()?.kind != TokenKind::Comma {
                                break;
                            } else {
                                self.current += 1;
                            }
                        }
                    }
                    self.expect(TokenKind::RightParen, "missing closing ')'")?;

                    if self.peek()?.kind != TokenKind::Arrow && parameters.len() == 1 {
                        // Not a function literal, just a grouping with a variable inside
                        expr = Some(ast::Expr {
                            kind: ast::VarExpr {
                                ident: parameters[0].clone(),
                            }
                            .into(),
                            span: start..self.peek()?.span.end,
                        });
                    } else if self.peek()?.kind != TokenKind::Arrow && parameters.len() != 1 {
                        return Err(self
                            .peek()?
                            .error_at("comma seperated grouping expressions are not supported"));
                    } else {
                        self.current += 1;

                        let fun_body = self.parse_expr()?;
                        expr = Some(ast::Expr {
                            span: start..fun_body.span.end,
                            kind: ast::FunLit {
                                parameters,
                                body: Box::new(fun_body),
                            }
                            .into(),
                        });
                    }
                }
            }
            TokenKind::If => {
                self.current += 1;

                let condition = self.parse_expr()?;
                let if_block = self.parse_block()?;
                let mut elif_stmts = Vec::new();
                let mut else_block = None;

                while self.peek()?.kind == TokenKind::Else {
                    self.current += 1;
                    if self.peek()?.kind == TokenKind::If {
                        if else_block.is_some() {
                            return Err(self.error_at_current("else if after final else block"));
                        }

                        self.current += 1;
                        let elif_cond = self.parse_expr()?;
                        let elif_block = self.parse_block()?;
                        elif_stmts.push((elif_cond, elif_block));
                    } else {
                        else_block = Some(self.parse_block()?);
                    }
                }

                expr = Some(ast::Expr {
                    kind: ast::IfExpr {
                        condition: Box::new(condition),
                        if_block,
                        elif_stmts,
                        else_block,
                    }
                    .into(),
                    span: token.span.clone(),
                });
            }
            TokenKind::While => {
                self.current += 1;

                let condition = self.parse_expr()?;
                let block = self.parse_block()?;

                expr = Some(ast::Expr {
                    kind: ast::WhileStmt {
                        condition: Box::new(condition),
                        block,
                    }
                    .into(),
                    span: token.span.clone(),
                });
            }
            TokenKind::Return => {
                self.current += 1;

                let value = self.parse_expr()?;

                expr = Some(ast::Expr {
                    kind: ast::ReturnStmt {
                        value: Box::new(value),
                    }
                    .into(),
                    span: token.span.clone(),
                });
            }
            TokenKind::LeftBrace => {
                let start = self.current;

                self.current += 1;
                if self.peek()?.kind == TokenKind::Ident || self.peek()?.kind == TokenKind::String {
                    let first_key = self.peek()?.clone();
                    self.current += 1;

                    if self.current < self.tokens.len() && self.peek()?.kind == TokenKind::Colon {
                        self.current += 1;
                        let first_value = self.parse_expr()?;

                        let mut inits = vec![(first_key, first_value)];
                        while self.current < self.tokens.len()
                            && self.peek()?.kind == TokenKind::Comma
                        {
                            self.current += 1;
                            if self.peek()?.kind == TokenKind::RightBrace {
                                break;
                            }

                            let key_token = self.peek()?.clone();
                            if !(key_token.kind == TokenKind::Ident
                                || key_token.kind == TokenKind::String)
                            {
                                return Err(key_token.error_at("expect key in object literal to be either string or identifier"));
                            }

                            self.current += 1;

                            self.expect(
                                TokenKind::Colon,
                                "expect ':' after key in object literal",
                            )?;
                            let value = self.parse_expr()?;

                            inits.push((key_token, value));
                        }

                        let rbrace_token =
                            self.expect(TokenKind::RightBrace, "unclosed object literal")?;
                        expr = Some(ast::Expr {
                            kind: ast::ObjectLit { inits }.into(),
                            span: start..rbrace_token.span.end,
                        })
                    } else if self.peek()?.kind == TokenKind::RightBrace {
                        expr = Some(ast::Expr {
                            kind: ast::ObjectLit { inits: vec![] }.into(),
                            span: start..self.peek()?.span.end,
                        })
                    } else {
                        // We have a block that starts with a string or ident but no colon.
                        // e.g.
                        // b := {
                        //   "foo";
                        //   bar;
                        //   5 + 6;
                        // }
                        self.current -= 3;
                        expr = Some(ast::Expr {
                            kind: self.parse_block()?.into(),
                            span: token.span.clone(),
                        });
                    }
                } else {
                    self.current -= 1;
                    expr = Some(ast::Expr {
                        kind: self.parse_block()?.into(),
                        span: token.span.clone(),
                    });
                }
            }

            TokenKind::Int
            | TokenKind::Float
            | TokenKind::String
            | TokenKind::Null
            | TokenKind::True
            | TokenKind::False => {
                self.current += 1;
                expr = Some(ast::Expr {
                    span: token.span.clone(),
                    kind: ast::Lit { token }.into(),
                })
            }
            TokenKind::LeftBracket => {
                let slice_literal_start = self.current;
                self.current += 1;

                let mut elements = Vec::new();
                if self.peek()?.kind != TokenKind::RightBracket {
                    loop {
                        if self.peek()?.kind == TokenKind::RightBracket {
                            break; // allow trailing comma
                        }

                        elements.push(self.parse_expr()?);

                        if self.peek()?.kind != TokenKind::Comma {
                            break;
                        } else {
                            self.current += 1;
                        }
                    }
                }

                let rbracket_token =
                    self.expect(TokenKind::RightBracket, "unclosed array literal")?;

                expr = Some(ast::Expr {
                    kind: ast::ArrLit { elements }.into(),
                    span: slice_literal_start..rbracket_token.span.end,
                })
            }
            TokenKind::Ident => {
                self.current += 1;

                if self.current < self.tokens.len() && self.peek()?.kind == TokenKind::ColonEqual {
                    self.current += 1;
                    let init = self.parse_expr()?;

                    expr = Some(ast::Expr {
                        span: token.span.clone(),
                        kind: ast::VarDecl {
                            ident: token,
                            init: Box::new(init),
                        }
                        .into(),
                    });
                } else {
                    expr = Some(ast::Expr {
                        span: token.span.clone(),
                        kind: ast::VarExpr {
                            ident: token.clone(),
                        }
                        .into(),
                    });
                }
            }
            _ if token.kind.is_prefix_op() => {
                self.current += 1;
                let target = self.parse_primary()?;
                expr = Some(ast::Expr {
                    span: token.span.clone(),
                    kind: ast::UnaryExpr {
                        op: token.clone(),
                        expr: Box::new(target),
                    }
                    .into(),
                });
            }
            _ => {}
        }

        let mut expr = if let Some(expr) = expr {
            expr
        } else {
            return Err(self.error_at_current("expected expression"));
        };

        while self.peek()?.kind == TokenKind::LeftParen
            || self.peek()?.kind == TokenKind::LeftBracket
            || self.peek()?.kind == TokenKind::Dot
        {
            match self.peek()?.kind {
                TokenKind::LeftParen => {
                    self.current += 1;

                    let mut args = Vec::new();
                    if self.peek()?.kind != TokenKind::RightParen {
                        while self.peek()?.kind != TokenKind::RightParen {
                            args.push(self.parse_expr()?);

                            if self.peek()?.kind == TokenKind::Comma {
                                self.current += 1;
                            }
                        }
                    }

                    let rparen_token = self.expect(
                        TokenKind::RightParen,
                        "missing closing ')' in call expression",
                    )?;

                    expr = ast::Expr {
                        span: expr.span.start..rparen_token.span.end,
                        kind: ast::CallExpr {
                            callee: Box::new(expr),
                            args,
                        }
                        .into(),
                    };
                }
                TokenKind::LeftBracket => {
                    self.current += 1;

                    let idx = self.parse_expr()?;
                    let rbracket_token = self.expect(
                        TokenKind::RightBracket,
                        "missing closing ']' in index expression",
                    )?;

                    expr = ast::Expr {
                        span: expr.span.start..rbracket_token.span.end,
                        kind: ast::IdxExpr {
                            target: Box::new(expr),
                            idx: Box::new(idx),
                        }
                        .into(),
                    };
                }
                TokenKind::Dot => {
                    let dot_token = self.peek()?.clone();

                    self.current += 1;
                    let ident = self
                        .expect(
                            TokenKind::Ident,
                            "expect identifier after `.` in get expression",
                        )?
                        .clone();

                    expr = ast::Expr {
                        span: expr.span.start..ident.span.end,
                        kind: ast::BinaryExpr {
                            left: Box::new(expr),
                            right: Box::new(ast::Expr {
                                span: ident.span.clone(),
                                kind: ast::VarExpr { ident }.into(),
                            }),
                            op: dot_token,
                        }
                        .into(),
                    };
                }
                _ => {}
            }
        }

        Ok(expr)
    }

    fn parse_prec_expr(&mut self, mut lhs: ast::Expr, min_prec: u8) -> Result<ast::Expr, Error> {
        let mut lookahead = self.peek()?.clone();

        while lookahead.kind.is_binary_op() && lookahead.kind.op_info().prec >= min_prec {
            let op = lookahead;
            self.current += 1;
            let mut rhs = self.parse_primary()?;
            lookahead = self.peek()?.clone();

            while lookahead.kind.is_binary_op()
                && ((lookahead.kind.op_info().assoc == Assoc::Ltr
                    && lookahead.kind.op_info().prec > op.kind.op_info().prec)
                    || (lookahead.kind.op_info().assoc == Assoc::Rtl
                        && lookahead.kind.op_info().prec == op.kind.op_info().prec))
            {
                rhs = self.parse_prec_expr(rhs, min_prec + 1)?;
                lookahead = self.peek()?.clone();
            }

            lhs = ast::Expr {
                span: lhs.span.start..rhs.span.end,
                kind: ast::BinaryExpr {
                    op,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                }
                .into(),
            };
        }

        Ok(lhs)
    }

    fn parse_expr(&mut self) -> Result<ast::Expr, Error> {
        let primary = self.parse_primary()?;
        self.parse_prec_expr(primary, 0)
    }
}

pub fn parse(tokens: &[Token]) -> Result<Vec<ast::Expr>, Error> {
    let mut stmts = Vec::new();

    if !tokens.is_empty() {
        let mut parser = Parser::new(tokens);
        while parser.peek()?.kind != TokenKind::Eof {
            let expr = parser.parse_expr()?;
            parser.expect(
                TokenKind::Semicolon,
                "expect ';' after top-level expression",
            )?;
            stmts.push(expr);
        }
    }

    Ok(stmts)
}
