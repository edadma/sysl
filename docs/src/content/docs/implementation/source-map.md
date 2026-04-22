---
title: Source map
description: Where each compiler concern lives in the Sysl source tree.
---

import { FileTree } from '@astrojs/starlight/components';

A flat inventory of the compiler source. Use this as a jumping-off point when you're
tracking down a feature or chasing a bug.

<FileTree>
- trisc/
  - sysl/src/main/scala/io/github/edadma/trisc/
    - SyslLexer.scala tokenisation, indentation handling, literate extraction
    - SyslParser.scala recursive-descent parser
    - SyslAST.scala untyped AST node definitions
    - SyslAnalyzer.scala type checking, generics, traits, contracts — the biggest file
    - SyslTypedAST.scala typed AST consumed by the backends
    - SyslType.scala internal type representation (i8..i64, refs, slices, generics, traits)
    - SyslInterpreter.scala reference tree-walk interpreter
    - SyslTriscCodegen.scala TRISC assembly backend
    - SyslLLVMCodegen.scala LLVM IR backend
    - SyslDriver.scala multi-module orchestration, #if resolution, module graph sort
    - ModuleMeta.scala .smeta serialization for cross-module metadata
    - SyslStdlib.scala standard-library module metadata integration
  - sysl-cli/src/main/scala/io/github/edadma/trisc/
    - syslCli.scala entry point for sysl compile / run / test / doc
  - std/ **standard library (literate Sysl)**
  - oskit/ **SLIX kernel and servers — large real-world Sysl program**
  - examples/ runnable sample programs
  - sysl-reference.md canonical language reference (this site is generated alongside)
  - sysl/src/test/scala/ reference test suite for every feature
</FileTree>

## Reference by feature

| Feature | Primary file(s) |
|---|---|
| Indentation-sensitive lexing | `SyslLexer.scala` |
| Literate `.lsysl` tangling | `SyslLexer.scala` |
| Expression precedence | `SyslParser.scala` |
| Pattern matching (parse) | `SyslParser.scala` |
| Pattern matching (analyse) | `SyslAnalyzer.scala` |
| `match` exhaustiveness | `SyslAnalyzer.scala` |
| Generics (monomorphisation) | `SyslAnalyzer.scala` |
| Traits, `impl`, operator overload | `SyslAnalyzer.scala` |
| `require`, `ensure`, `old()` | `SyslAnalyzer.scala` |
| Loop `variant` / `invariant` | `SyslAnalyzer.scala` |
| Struct `invariant` | `SyslAnalyzer.scala` + backends |
| `within` / `where` types | `SyslAnalyzer.scala` + backends |
| `not null` pointers | `SyslAnalyzer.scala` + backends |
| Closures and capture analysis | `SyslAnalyzer.scala` + both codegens |
| String layout and concatenation | both codegens |
| Ref counting and `deinit` | both codegens |
| `volatile` loads and stores | `SyslLLVMCodegen.scala` |
| `#test` discovery | `SyslDriver.scala` + `syslCli` |
| `sysl doc` renderer | `sysl-cli` |

## Companion projects in the repo

| Subproject | Purpose |
|---|---|
| `asm/` | TRISC assembler (reads `.asm`, writes `.tof`) |
| `tof/` | TRISC Object File format and linker |
| `cpu/` | TRISC CPU emulator (the interpreter for `.tof` images) |
| `mem/` | Composable memory model used by the emulator |
| `trisc-cli/` | Command-line front-end for assembler / linker / emulator |
| `oskit/` | SLIX kernel, drivers, and userspace programs — all in Sysl |
| `std/` | Standard library (literate Sysl) |
| `docs/` | This documentation site (Astro + Starlight) |

## See also

- [Compiler pipeline](/implementation/pipeline/)
- [Analyzer architecture](/implementation/analyzer-architecture/)
- [Testing strategy](/implementation/testing/)
