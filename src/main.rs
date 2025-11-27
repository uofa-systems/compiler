mod ast;
mod codegen;
mod error;
mod ir;
mod lexer;
mod lower;
mod optimizer;
mod parser;
mod preprocessor;
mod span;
mod typecheck;

use std::env;
use std::path::Path;
use std::process::Command;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: compiler <input.c> [-o output] [-S]");
        std::process::exit(1);
    }

    let mut input_file = String::new();
    let mut output_file = if cfg!(windows) {
        "a.exe".to_string()
    } else {
        "a.out".to_string()
    };
    let mut emit_asm_only = false;
    let mut i = 1;

    while i < args.len() {
        match args[i].as_str() {
            "-o" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: -o requires a filename");
                    std::process::exit(1);
                }
                output_file = args[i].clone();
            }
            "-S" => emit_asm_only = true,
            arg => {
                if arg.ends_with(".c") {
                    input_file = arg.to_string();
                } else {
                    eprintln!("Unknown argument: {}", arg);
                    std::process::exit(1);
                }
            }
        }
        i += 1;
    }

    if input_file.is_empty() {
        eprintln!("Error: No input .c file");
        std::process::exit(1);
    }

    let raw_src = std::fs::read_to_string(&input_file).expect("Failed to read file");

    let src = match preprocessor::preprocess(&raw_src, Path::new(&input_file)) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Preprocessor Error: {}", e);
            std::process::exit(1);
        }
    };

    let asm_code = match compile(&src) {
        Ok(asm) => asm,
        Err(e) => {
            eprintln!("{}", e.display(&src));
            std::process::exit(1);
        }
    };

    if emit_asm_only {
        let asm_filename = output_file.replace(".exe", "").replace(".out", "") + ".s";
        std::fs::write(&asm_filename, asm_code).expect("Failed to write .s");
    } else {
        let temp_asm = format!("temp_{}.s", std::process::id());
        std::fs::write(&temp_asm, asm_code).expect("Failed to write temp asm");

        let assembler = env::var("RISCV_CC").unwrap_or("riscv64-linux-gnu-gcc".to_string());
        let status = Command::new(&assembler)
            .arg(&temp_asm)
            .arg("-o")
            .arg(&output_file)
            .arg("-static")
            .status();
        let _ = std::fs::remove_file(&temp_asm);

        if let Ok(s) = status {
            if !s.success() {
                std::process::exit(1);
            }
        } else {
            eprintln!("Error: Assembler '{}' not found.", assembler);
            std::process::exit(1);
        }
    }
}

fn compile(src: &str) -> Result<String, error::CompileError> {
    let tokens = lexer::lex(src)?;
    let ast = parser::parse(&tokens)?;
    let typed_ast = typecheck::typecheck(&ast)?;
    let mut ir = lower::lower(&typed_ast);
    optimizer::optimize(&mut ir);
    Ok(codegen::generate(&ir))
}
