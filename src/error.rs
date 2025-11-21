use crate::span::Span;
use std::fmt;

#[derive(Debug)]
pub struct CompileError {
    pub message: String,
    pub span: Option<Span>,
}

impl CompileError {
    // A constructor for errors without a specific location.
    pub fn new(message: impl Into<String>) -> Self {
        CompileError {
            message: message.into(),
            span: None,
        }
    }

    // A new constructor for errors that have a specific location.
    pub fn new_with_span(message: impl Into<String>, span: Span) -> Self {
        CompileError {
            message: message.into(),
            span: Some(span),
        }
    }

    pub fn display(&self, src: &str) -> String {
        let mut result = format!("Error: {}", self.message);
        if let Some(span) = self.span {
            // Find the line and column number
            let mut line_start = 0;
            let mut line_num = 1;
            for (i, c) in src.char_indices() {
                if i >= span.start {
                    break;
                }
                if c == '\n' {
                    line_start = i + 1;
                    line_num += 1;
                }
            }

            let line_end = src[line_start..]
                .find('\n')
                .map(|i| line_start + i)
                .unwrap_or(src.len());
            let line = &src[line_start..line_end];
            let col_num = span.start - line_start + 1;

            // Append location info to the error message.
            result.push_str(&format!("\n --> at line {}, column {}", line_num, col_num));
            result.push_str(&format!("\n   |\n"));
            result.push_str(&format!("{:>2} | {}\n", line_num, line));
            result.push_str(&format!(
                "   | {}{}",
                " ".repeat(col_num),
                "^".repeat(span.end - span.start)
            ));
        }
        result
    }
}

impl From<String> for CompileError {
    fn from(message: String) -> Self {
        CompileError::new(message)
    }
}

impl From<&str> for CompileError {
    fn from(message: &str) -> Self {
        CompileError::new(message)
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for CompileError {}
