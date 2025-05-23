# Polymede

**Polymede** is a functional programming language and a compiler that targets WebAssembly. It features a type system with parametric polymorphism, algebraic data types, and a monadic command system for handling side effects.

## Features

### Language Features
- **Parametric Polymorphism**: Generic functions and data types with `forall` quantification
- **Algebraic Data Types**: Support for both enums and inductive types
- **Pattern Matching**: Comprehensive pattern matching with `match` and `fold` expressions
- **Lambda Functions**: First-class functions with closure support
- **Command Monad**: Structured approach to side effects with `do` notation
- **Type Inference**: Type checking and inference system

### Technical Features
- **Multi-stage Compilation**: Separation between language stages
- **WebAssembly Target**: Compiles to WebAssembly modules
- **Memory Management**: Custom graph memory machine for execution
- **Foreign Function Interface**: Integration with external WebAssembly functions

## Architecture

The project is organized as a Rust workspace with the following modules:

```
polymede/
├── ast/           # Abstract Syntax Tree and parser
├── compiler/      # Main compilation pipeline
├── runtime/       # WebAssembly runtime support  
├── wasm/          # WebAssembly code generation
└── examples/      # Example Polymede programs
```

## Compilation Pipeline

Polymede uses a three-stage compilation process:

```
.pmd files → AST → GMM → WebAssembly
```

1. **Parsing**: Source code (`.pmd`) is parsed into an Abstract Syntax Tree
2. **Type Checking**: Type checking including formation rules
3. **GMM Compilation**: Translation to Graph Memory Machine intermediate representation
4. **WebAssembly Generation**: Final compilation to WASM

## Language Syntax

### Type Declarations

```polymede
// Enum types
type Bool = enum {
| T
| F
}

// Inductive types (recursive)
type List = # forall A
  ind { List .
  | Nil
  | Cons(A, List)
  }
```

### Function Definitions

```polymede
// Simple function
fn not = # Bool -> Bool : { x .
  match x {
  | T . F
  | F . T
  }
}

// Polymorphic function
fn id = forall { A .
  # A -> A
  { x . x }
}
```

### Pattern Matching and Fold

```polymede
// Pattern matching
fn first = # forall a, b . # Pair(a, b) -> a : { p .
  match p {
  | Pair(x, y) . x
  }
}

// Structural recursion with fold
fn length = # forall A . # List(A) -> Nat : { xs .
  fold xs {
  | Nil . Zero
  | Cons(x, result) . Succ(result)
  }
}
```

### Command System

```polymede
// Command with do notation
let hello = # Cmd[Unit]
  do {
    print("hello, world");
    < Unit
  }
```

## Examples

The `examples/` directory contains various sample programs:

- **000_bool.pmd**: Basic boolean operations
- **001_nat.pmd**: Natural numbers with arithmetic
- **002_list.pmd**: List data structure with operations
- **005_hello_world.pmd**: Simple command example
- **010_lambda.pmd**: Higher-order functions and closures

## Current Status

Current TODOs include:

- **Error Messages**: Improving human-readable error reporting
- **Nested Patterns**: Enhanced pattern matching compilation
- **Fold Statements**: Better desugaring of fold into match + recursion
- **Runtime Optimizations**: Command tracing and memory management improvements
- **Functors**: Type system features for recursive types

## Development

### Project Structure
- `ast/src/base.rs`: Core AST definitions
- `compiler/src/main.rs`: Main compilation entry point
- `compiler/src/polymede_compiler.rs`: Polymede → GMM compilation
- `compiler/src/gmm_compiler.rs`: GMM → WebAssembly compilation
- `examples/`: Sample Polymede programs
