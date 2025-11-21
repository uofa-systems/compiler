fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <input-file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let src = std::fs::read_to_string(filename).unwrap_or_else(|e| {
        eprintln!("Failed to read {}: {}", filename, e);
        std::process::exit(1);
    });

    if let Err(err) = compiler::compile(&src) {
        eprintln!("{}", err.display(&src));
        std::process::exit(1);
    }
}
