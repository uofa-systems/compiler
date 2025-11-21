pub mod ast;
pub mod codegen;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod typecheck;

use error::CompileError;

/// High-level “compile one source string” function.
/// For a starter project, you can just print codegen output to stdout.
pub fn compile(src: &str) -> Result<(), CompileError> {
    let tokens = lexer::lex(src)?;
    let ast = parser::parse(&tokens)?; // Now returns a Function
    let typed_ast = typecheck::typecheck(&ast)?; // Now takes a &Function
    codegen::generate(&typed_ast)?; // Now takes a &Function
    Ok(())
}
