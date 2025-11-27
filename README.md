# RISC-V C Compiler

This is a small C compiler written in Rust.
It takes a `.c` file, compiles it to RISC-V 64-bit assembly, then uses a RISC-V GCC to assemble and link it into a Linux RISC-V binary.

By default it:

- Reads a single `input.c`
- Writes a RISC-V binary called `a.out` (or `a.exe` on Windows)
- Uses a RISC-V GCC cross-compiler to assemble/link
- Can optionally stop after generating `.s` assembly

---

## Requirements

You need two things:

1. **Rust (for building the compiler itself)**
   Install via <https://rustup.rs>.

2. **RISC-V 64-bit Linux GCC + QEMU (for running the compiled programs)**

   The compiler expects a cross-compiler that looks like `riscv64-linux-gnu-gcc` by default.

   ### Linux (Ubuntu / Debian)

   ```bash
   sudo apt update
   sudo apt install build-essential gcc-riscv64-linux-gnu qemu-user
    ```

This gives you:

* `riscv64-linux-gnu-gcc`  (used to assemble and link)
* `qemu-riscv64`           (used to run the RISC-V binary on x86)

### Linux (Arch)

```bash
sudo pacman -S base-devel riscv64-linux-gnu-gcc qemu-user
```

Package names may vary slightly; you want:

* a `riscv64-linux-gnu-gcc` cross-compiler
* `qemu-riscv64` (comes from `qemu-user`)

### macOS

Using Homebrew:

```bash
# Rust
brew install rustup
rustup-init

# RISC-V and QEMU
brew tap riscv-software-src/riscv
brew install riscv-gnu-toolchain qemu
```

Make sure the RISC-V GCC is in your `PATH`. If it’s not named
`riscv64-linux-gnu-gcc`, you can point the compiler to it using `RISCV_CC`
(explained below).

### Windows

The easiest way is **WSL** (Windows Subsystem for Linux):

1. Install Ubuntu in WSL.

2. Inside Ubuntu, run:

   ```bash
   sudo apt update
   sudo apt install build-essential gcc-riscv64-linux-gnu qemu-user
   ```

3. Install Rust in WSL:

   ```bash
   curl https://sh.rustup.rs -sSf | sh
   ```

Then follow the Linux usage instructions from inside WSL.

---

## Building the Compiler

Clone the repo and build:

```bash
git clone https://github.com/uofa-systems/compiler
cd compiler
cargo build --release
```

The compiler binary will be at:

* `target/release/compiler` (Linux / macOS / WSL)
* `target\release\compiler.exe` (Windows)

For quick testing you can also run it directly via:

```bash
cargo run -- <args...>
```

---

## Command-Line Usage

The compiler’s interface (from `main.rs`) is:

```text
Usage: compiler <input.c> [-o output] [-S]
```

* `<input.c>` — C source file (must end with `.c`)
* `-o output` — set the output file name

  * default: `a.out` on Unix, `a.exe` on Windows
* `-S` — write assembly only (`.s`) and **do not** assemble/link

The compiler:

1. Reads the input file.
2. Runs the internal preprocessor.
3. Lexes, parses, type-checks, lowers to IR, and optimizes.
4. Generates RISC-V assembly.
5. If `-S` is **not** used, it:

   * writes a temp `temp_<pid>.s`
   * calls a RISC-V GCC (`riscv64-linux-gnu-gcc -static`)
   * deletes the temp file.

The RISC-V GCC executable is chosen as:

* Environment variable `RISCV_CC`, if set
* Otherwise: `riscv64-linux-gnu-gcc`

If the assembler executable cannot be found or fails, the compiler exits with a non-zero status.

---

## Basic Example (Linux / macOS / WSL)

### 1. Write a C file

```c
extern printf(char* fmt, ...);

int main() {
    printf("Hello from RISC-V!\n");
    return 0;
}
```

### 2. Compile and link with default output

Using `cargo run`:

```bash
cargo run -- hello.c
```

This will produce:

* `a.out` (RISC-V 64-bit Linux binary)

Or using the built binary:

```bash
./target/release/compiler hello.c
```

### 3. Run the program with QEMU

```bash
qemu-riscv64 ./a.out
```

You should see:

```text
Hello from RISC-V!
```

---

## Using `-o` and `-S`

### Custom output name

```bash
cargo run -- hello.c -o hello_rv64
qemu-riscv64 ./hello_rv64
```

### Assembly only

If you just want the `.s` file:

```bash
cargo run -- hello.c -o hello_rv64 -S
```

This will:

* Strip `.exe` / `.out` from `hello_rv64` and write `hello_rv64.s`
* **Not** call the RISC-V GCC

You can then assemble and link manually:

```bash
riscv64-linux-gnu-gcc -static hello_rv64.s -o hello_rv64
qemu-riscv64 ./hello_rv64
```

---

## Choosing a Different RISC-V GCC

If your cross-compiler has a different name (for example `riscv64-unknown-linux-gnu-gcc`), set the `RISCV_CC` environment variable:

```bash
export RISCV_CC=riscv64-unknown-linux-gnu-gcc
cargo run -- hello.c
```

On Windows PowerShell:

```powershell
$env:RISCV_CC = "riscv64-unknown-linux-gnu-gcc"
cargo run -- hello.c
```

The compiler will then use that instead of `riscv64-linux-gnu-gcc`.

---

## Running on Real Hardware

If you have an actual RISC-V Linux machine or board:

1. Build the RISC-V binary on your host (using this compiler + cross-compiler).
2. Copy the resulting binary (e.g. `a.out`) to the RISC-V system.
3. Run it directly:

   ```bash
   chmod +x a.out
   ./a.out
   ```

QEMU is only needed to run RISC-V code on non-RISC-V machines.
