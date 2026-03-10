# C → x86-64 Compiler

[![License: BSD3](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)
[![GHC](https://img.shields.io/badge/GHC-9.2-blueviolet.svg)](https://www.haskell.org/ghc/)
[![Haskell](https://img.shields.io/badge/language-Haskell-5e5086.svg)](https://www.haskell.org/)
[![Docker](https://img.shields.io/badge/docker-ready-2496ED?logo=docker&logoColor=white)](Dockerfile)

A toy C-to-x86-64 compiler written in Haskell. It parses a (large) subset of C,
generates AT&T-syntax x86-64 assembly, and then calls **GCC** to assemble and
link the final binary.

---

## Supported C Features

| Feature | Notes |
|---|---|
| Types | `int`, `short`, `char`, `long`, `float`, `double`, `void`, pointers, arrays, `struct`, `union`, `enum` |
| Arithmetic operators | `+`, `-`, `*`, `/`, `%` |
| Bitwise operators | `&`, `\|`, `^`, `~`, `>>`, `<<` |
| Logical operators | `&&`, `\|\|`, `!` |
| Comparison operators | `==`, `!=`, `<`, `>`, `<=`, `>=` |
| Assignment | `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `\|=`, `^=`, `>>=`, `<<=` |
| Increment / decrement | prefix `++` / `--` |
| Control flow | `if`/`else`, `while`, `for`, `goto`, `break`, `continue` |
| Functions | declarations and definitions (up to 5 arguments) |
| Pointers | `&` (reference), `*` (dereference) |
| String literals | stored in `.data` section |
| Comments | `//` line comments and `/* */` block comments |

---

## Prerequisites

### Building from Source

- [GHC](https://www.haskell.org/ghc/) (tested with **9.2**)
- [Stack](https://docs.haskellstack.org/en/stable/README/) **≥ 2.7**
- **GCC** (must be on `PATH` at runtime for linking)

### Using Docker

- [Docker](https://docs.docker.com/get-docker/) **≥ 20**

---

## Building from Source

```bash
# Clone the repository
git clone https://github.com/kozlov721/compiler.git
cd compiler

# Build (Stack will download all Haskell dependencies automatically)
stack build

# Optionally install the binary to ~/.local/bin
stack install
```

The resulting executable is named **`compiler-exe`** (or `compiler` after
`stack install`).

---

## Running the Compiler

```
compiler-exe [GCC_OPTIONS] <input.c>
```

- **`<input.c>`** – the C source file to compile (must be the **last** argument).
- Any extra arguments are forwarded verbatim to GCC (e.g. `-o my_program`).

### Examples

```bash
# Compile hello.c → a.out (default GCC output)
compiler-exe hello.c

# Compile hello.c and name the output binary hello
compiler-exe -o hello hello.c

# Link against a library (e.g. libm)
compiler-exe -o math_demo -lm math_demo.c
```

---

## Using Docker

### 1. Build the Docker image

```bash
docker build -t compiler .
```

The image uses a two-stage build: the first stage compiles the Haskell source
with GHC/Stack, and the second stage produces a minimal Debian image that
contains only GCC and the `compiler` binary.

### 2. Run the compiler via Docker

Mount the directory that contains your C source files to `/src` inside the
container (the working directory):

```bash
docker run --rm -v "$(pwd):/src" compiler [GCC_OPTIONS] <input.c>
```

#### Examples

```bash
# Compile hello.c in the current directory → a.out
docker run --rm -v "$(pwd):/src" compiler hello.c

# Compile and name the output binary
docker run --rm -v "$(pwd):/src" compiler -o hello hello.c
```

> **Tip:** You can create a shell alias to avoid typing the full `docker run`
> command every time:
>
> ```bash
> alias compiler='docker run --rm -v "$(pwd):/src" compiler'
> compiler -o hello hello.c
> ```

---

## Running the Tests

```bash
stack test
```

Test cases live in `test/resources/`. Each `.c` file should exit with code
**42** to be reported as passing.

---

## Project Structure

```
.
├── app/
│   └── Main.hs          # Entry point – argument parsing, GCC invocation
├── src/
│   ├── AST.hs           # Abstract syntax tree data types
│   ├── Parser.hs        # Parsec-based C parser
│   ├── Compiler.hs      # Code generation (x86-64 AT&T assembly)
│   └── Utils.hs         # Helper utilities
├── test/
│   └── Spec.hs          # Test runner
├── Dockerfile           # Multi-stage Docker build
├── package.yaml         # hpack package description
└── stack.yaml           # Stack resolver and build configuration
```

---

## License

BSD 3-Clause – see [LICENSE](LICENSE).
