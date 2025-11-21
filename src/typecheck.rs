use crate::ast::Program;
use crate::error::CompileError;

pub fn typecheck(ast: &Program) -> Result<Program, CompileError> {
    // A real type checker would:
    // 1. Build a symbol table of all functions.
    // 2. For each function:
    //    a. Add parameters to a local scope.
    //    b. Traverse the function body.
    //    c. Check that variables are declared (we don't have declarations yet).
    //    d. Check that function calls match function signatures.
    //    e. Verify that binary/unary operations are valid for the types involved.
    Ok(ast.clone())
}
