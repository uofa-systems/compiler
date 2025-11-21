use crate::error::CompileError;
use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Number(i64),
    Identifier(String),

    // Keywords
    IntKeyword,
    ReturnKeyword,
    IfKeyword,
    ElseKeyword,
    WhileKeyword,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Assign,
    EqualTo,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,

    // Punctuation
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    Comma,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub fn lex(input: &str) -> Result<Vec<Token>, CompileError> {
    let mut tokens = Vec::new();
    let mut i = 0;
    let bytes = input.as_bytes();

    while i < bytes.len() {
        let start = i;
        let c = bytes[i] as char;

        if c.is_whitespace() {
            i += 1;
            continue;
        }

        if c.is_ascii_alphabetic() || c == '_' {
            let mut ident = String::new();
            while i < bytes.len()
                && ((bytes[i] as char).is_ascii_alphanumeric() || (bytes[i] as char) == '_')
            {
                ident.push(bytes[i] as char);
                i += 1;
            }
            let end = i;
            let kind = match ident.as_str() {
                "int" => TokenKind::IntKeyword,
                "return" => TokenKind::ReturnKeyword,
                "if" => TokenKind::IfKeyword,
                "else" => TokenKind::ElseKeyword,
                "while" => TokenKind::WhileKeyword,
                _ => TokenKind::Identifier(ident),
            };
            tokens.push(Token {
                kind,
                span: Span { start, end },
            });
            continue;
        }

        if c.is_ascii_digit() {
            let mut value: i64 = 0;
            while i < bytes.len() && (bytes[i] as char).is_ascii_digit() {
                value = value * 10 + (bytes[i] as char).to_digit(10).unwrap() as i64;
                i += 1;
            }
            let end = i;
            tokens.push(Token {
                kind: TokenKind::Number(value),
                span: Span { start, end },
            });
            continue;
        }

        let (kind, len) = match c {
            '+' => (TokenKind::Plus, 1),
            '-' => (TokenKind::Minus, 1),
            '*' => (TokenKind::Star, 1),
            '/' => (TokenKind::Slash, 1),
            '(' => (TokenKind::LParen, 1),
            ')' => (TokenKind::RParen, 1),
            '{' => (TokenKind::LBrace, 1),
            '}' => (TokenKind::RBrace, 1),
            ';' => (TokenKind::Semicolon, 1),
            ',' => (TokenKind::Comma, 1),
            '=' => {
                if i + 1 < bytes.len() && bytes[i + 1] as char == '=' {
                    (TokenKind::EqualTo, 2)
                } else {
                    (TokenKind::Assign, 1)
                }
            }
            '!' => {
                if i + 1 < bytes.len() && bytes[i + 1] as char == '=' {
                    (TokenKind::NotEqual, 2)
                } else {
                    return Err(CompileError::new_with_span(
                        "Unexpected character: '!'",
                        Span {
                            start,
                            end: start + 1,
                        },
                    ));
                }
            }
            '<' => {
                if i + 1 < bytes.len() && bytes[i + 1] as char == '=' {
                    (TokenKind::LessThanEqual, 2)
                } else {
                    (TokenKind::LessThan, 1)
                }
            }
            '>' => {
                if i + 1 < bytes.len() && bytes[i + 1] as char == '=' {
                    (TokenKind::GreaterThanEqual, 2)
                } else {
                    (TokenKind::GreaterThan, 1)
                }
            }
            _ => {
                return Err(CompileError::new_with_span(
                    format!("Unexpected character: {}", c),
                    Span {
                        start,
                        end: start + 1,
                    },
                ));
            }
        };

        i += len;
        let end = i;
        tokens.push(Token {
            kind,
            span: Span { start, end },
        });
    }

    Ok(tokens)
}
