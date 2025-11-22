# RISC-V Compiler

This is a compiler written in Rust that translates a C-like language into **RISC-V 64-bit Assembly (RV64)**. It supports functions, recursion, integer arithmetic, and control flow.

## Table of Contents
- [Prerequisites](#prerequisites)
- [Project Setup](#project-setup)
- [How to Use](#how-to-use)
- [Language Features](#language-features)
- [Technical Details](#technical-details)

## Prerequisites

To build the compiler and run the generated code, you will need:

1.  **Rust**: To build the compiler itself.
    *   Install via [rustup.rs](https://rustup.rs/).
2.  **RISC-V Toolchain**: To assemble and link the output.
    *   Ubuntu/Debian: `sudo apt install gcc-riscv64-linux-gnu`
3.  **QEMU**: To emulate the RISC-V processor on your machine.
    *   Ubuntu/Debian: `sudo apt install qemu-user`
4.  **OPTIONAL: RARS**: This can be used instead of gcc and qemu to run the generated RISC-V code. 

## How to Use

### 1. Create a Source File
Create a file named `input.c` (or any name you prefer) with your code.

**Example: Recursive Fibonacci**
```c
int fib(int n) {
    if (n < 2) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

int main() {
    return fib(10);
}
```

### 2. Compile to Assembly
Run the Rust compiler to generate RISC-V assembly code. We redirect stdout to a file (e.g., `output.s`).

```bash
cargo run --quiet -- input.c > output.s
```

### 3. Assemble and Link
Use the RISC-V GCC cross-compiler to turn the assembly into an executable binary. We use the `-static` flag so QEMU doesn't need to look for dynamic libraries.

```bash
riscv64-linux-gnu-gcc -static output.s -o program
```

### 4. Run with QEMU
Execute the binary using QEMU user-mode emulation.

```bash
qemu-riscv64 ./program
```

### 5. Check the Result
Since the generated `main` function returns the result as the program exit code, you can check it using `echo $?`.

```bash
echo $?
# Should output 55 for fib(10)
```

## Language Features

*   **Data Types**: 64-bit signed integers (`int`).
*   **Variables**: Local variable declarations (`int x = 5;`).
*   **Control Flow**:
    *   `if (condition) { ... } else { ... }`
    *   `while (condition) { ... }`
*   **Functions**: Recursive calls, return statements.
*   **Operators**:
    *   Arithmetic: `+`, `-`, `*`, `/`
    *   Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
    *   Assignment: `=`

## Technical Details

*   **Target Architecture**: RISC-V 64-bit (RV64I).
*   **Register Usage**:
    *   `a0`: Function return value and first argument.
    *   `a0-a5`: Function arguments (up to 6).
    *   `s0` (fp): Frame pointer, used to access local variables on the stack.
    *   `sp`: Stack pointer, maintained 16-byte aligned.
    *   `ra`: Return address.
*   **Code Generation**:
    *   The compiler uses a stack-machine approach.
    *   Expressions push results to the stack; operators pop operands from the stack.
    *   While not the most efficient (heavy memory usage), it is robust and easy to debug.
