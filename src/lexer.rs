use crate::error::CompileError;
use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Number(i64),
    StringLiteral(String),
    Identifier(String),
    IntKeyword,
    CharKeyword,
    VoidKeyword,
    StructKeyword,
    TypedefKeyword,
    EnumKeyword,
    SizeofKeyword,
    ReturnKeyword,
    IfKeyword,
    ElseKeyword,
    WhileKeyword,
    DoKeyword,
    ForKeyword,
    SwitchKeyword,
    CaseKeyword,
    DefaultKeyword,
    ExternKeyword,
    BreakKeyword,
    ContinueKeyword,
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
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Ampersand,
    Pipe,
    Caret,
    Tilde,
    AmpAmp,
    PipePipe,
    LessLess,
    GreaterGreater,
    Bang,
    Semicolon,
    Colon,
    Comma,
    Ellipsis,
    Dot,
    Arrow,
    Increment,
    Decrement,
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
        if c == '/' {
            if i + 1 < bytes.len() && bytes[i + 1] == b'/' {
                i += 2;
                while i < bytes.len() && bytes[i] != b'\n' {
                    i += 1;
                }
                continue;
            }
        }
        if c == '"' {
            let mut s = String::new();
            i += 1;
            while i < bytes.len() {
                let curr = bytes[i] as char;
                if curr == '"' {
                    break;
                }
                if curr == '\\' {
                    s.push(curr);
                    i += 1;
                    if i < bytes.len() {
                        s.push(bytes[i] as char);
                        i += 1;
                    }
                    continue;
                }
                s.push(curr);
                i += 1;
            }
            if i >= bytes.len() {
                return Err(CompileError::new_with_span(
                    "Unterminated string",
                    Span::new(start, i),
                ));
            }
            i += 1;
            tokens.push(Token {
                kind: TokenKind::StringLiteral(s),
                span: Span { start, end: i },
            });
            continue;
        }
        if c.is_ascii_alphabetic() || c == '_' {
            let mut ident = String::new();
            while i < bytes.len()
                && ((bytes[i] as char).is_ascii_alphanumeric() || bytes[i] == b'_')
            {
                ident.push(bytes[i] as char);
                i += 1;
            }
            let end = i;
            let kind = match ident.as_str() {
                "int" => TokenKind::IntKeyword,
                "char" => TokenKind::CharKeyword,
                "void" => TokenKind::VoidKeyword,
                "struct" => TokenKind::StructKeyword,
                "typedef" => TokenKind::TypedefKeyword,
                "enum" => TokenKind::EnumKeyword,
                "sizeof" => TokenKind::SizeofKeyword,
                "return" => TokenKind::ReturnKeyword,
                "if" => TokenKind::IfKeyword,
                "else" => TokenKind::ElseKeyword,
                "while" => TokenKind::WhileKeyword,
                "do" => TokenKind::DoKeyword,
                "for" => TokenKind::ForKeyword,
                "switch" => TokenKind::SwitchKeyword,
                "case" => TokenKind::CaseKeyword,
                "default" => TokenKind::DefaultKeyword,
                "extern" => TokenKind::ExternKeyword,
                "break" => TokenKind::BreakKeyword,
                "continue" => TokenKind::ContinueKeyword,
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
            tokens.push(Token {
                kind: TokenKind::Number(value),
                span: Span { start, end: i },
            });
            continue;
        }
        let (kind, len) = match c {
            '+' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'+' {
                    (TokenKind::Increment, 2)
                } else {
                    (TokenKind::Plus, 1)
                }
            }
            '-' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'>' {
                    (TokenKind::Arrow, 2)
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'-' {
                    (TokenKind::Decrement, 2)
                } else {
                    (TokenKind::Minus, 1)
                }
            }
            '*' => (TokenKind::Star, 1),
            '/' => (TokenKind::Slash, 1),
            '(' => (TokenKind::LParen, 1),
            ')' => (TokenKind::RParen, 1),
            '{' => (TokenKind::LBrace, 1),
            '}' => (TokenKind::RBrace, 1),
            '[' => (TokenKind::LBracket, 1),
            ']' => (TokenKind::RBracket, 1),
            ';' => (TokenKind::Semicolon, 1),
            ':' => (TokenKind::Colon, 1),
            ',' => (TokenKind::Comma, 1),
            '~' => (TokenKind::Tilde, 1),
            '^' => (TokenKind::Caret, 1),
            '&' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'&' {
                    (TokenKind::AmpAmp, 2)
                } else {
                    (TokenKind::Ampersand, 1)
                }
            }
            '|' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'|' {
                    (TokenKind::PipePipe, 2)
                } else {
                    (TokenKind::Pipe, 1)
                }
            }
            '!' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    (TokenKind::NotEqual, 2)
                } else {
                    (TokenKind::Bang, 1)
                }
            }
            '<' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    (TokenKind::LessThanEqual, 2)
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'<' {
                    (TokenKind::LessLess, 2)
                } else {
                    (TokenKind::LessThan, 1)
                }
            }
            '>' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    (TokenKind::GreaterThanEqual, 2)
                } else if i + 1 < bytes.len() && bytes[i + 1] == b'>' {
                    (TokenKind::GreaterGreater, 2)
                } else {
                    (TokenKind::GreaterThan, 1)
                }
            }
            '.' => {
                if i + 2 < bytes.len() && bytes[i + 1] == b'.' && bytes[i + 2] == b'.' {
                    (TokenKind::Ellipsis, 3)
                } else {
                    (TokenKind::Dot, 1)
                }
            }
            '=' => {
                if i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                    (TokenKind::EqualTo, 2)
                } else {
                    (TokenKind::Assign, 1)
                }
            }
            _ => {
                return Err(CompileError::new_with_span(
                    format!("Unexpected char '{}'", c),
                    Span {
                        start,
                        end: start + 1,
                    },
                ));
            }
        };
        i += len;
        tokens.push(Token {
            kind,
            span: Span { start, end: i },
        });
    }
    Ok(tokens)
}
