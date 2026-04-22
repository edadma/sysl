---
title: Compiler pipeline
description: How a line of Sysl source travels from text to machine code.
---

import { Steps, Aside, Tabs, TabItem } from '@astrojs/starlight/components';

Every compile runs through the same five stages. Most bugs show up in one of them
specifically — knowing which one is where debugging starts.

<Steps>

1. **Lexing** — `SyslLexer.scala`

   Turns a source string into a token stream. The lexer is indentation-aware: it emits
   explicit `INDENT` / `DEDENT` / `NEWLINE` tokens for Python-style block syntax, and it
   understands the inline `do` / `then` forms that let you write single-line bodies.

   Literate (`.lsysl`) files are handled here too — the lexer consumes only the indented
   code blocks and discards the Markdown prose.

2. **Parsing** — `SyslParser.scala`

   Recursive-descent parser that produces the untyped AST in `SyslAST.scala`. Handles
   every syntactic form the language has: module declarations, imports, attributes,
   generics, traits, `match`, `if`, `for`, closures, literate prose-interleaved source,
   and conditional compilation markers (left as AST nodes for the driver to resolve).

3. **Analysis** — `SyslAnalyzer.scala`

   The biggest and most interesting stage. Does:

   - Name resolution (scopes, imports, module-qualified references).
   - Type inference and type checking — including generic monomorphisation and
     trait-method dispatch.
   - Exhaustiveness checks on `match` over tagged unions.
   - Contract typing (`require`, `ensure`, loop `variant`/`invariant`, struct
     `invariant`, `old(...)`).
   - Operator overloading resolution — `a + b` on a user type becomes a call to
     `Add.add(a, b)` resolved to a specific `impl` block.
   - Emits the typed AST in `SyslTypedAST.scala`.

   See [Analyzer Architecture](/implementation/analyzer-architecture/).

4. **Backend** — `SyslInterpreter.scala` / `SyslTriscCodegen.scala` / `SyslLLVMCodegen.scala`

   All three consume the typed AST. The interpreter walks it directly. The TRISC codegen
   emits assembly in the TRISC ISA and hands it to the assembler. The LLVM codegen emits
   `.ll` text suitable for `clang`.

   The backends share a lowering style — struct returns as hidden pointers, strings as
   fat pointers, refs with a 16-byte header — but each has target-specific twists
   (r1 calling convention on TRISC, `i8*` envs for closures on LLVM, etc.).

5. **Link** — the host toolchain

   For TRISC, `triscCli` links `.tof` files with a user-supplied linker script into a
   bootable image. For LLVM, `clang` takes over. For the interpreter, there is no link
   step — the typed AST is the executable.

</Steps>

## Where each concern ends up

| Concern | Stage | File |
|---|---|---|
| Indentation blocks | Lexing | `SyslLexer.scala` |
| Operator precedence | Parsing | `SyslParser.scala` |
| Literate `.lsysl` handling | Lexing | `SyslLexer.scala` |
| Conditional `#if` | Driver | `SyslDriver.scala` |
| Type inference | Analysis | `SyslAnalyzer.scala` |
| Generic instantiation | Analysis | `SyslAnalyzer.scala` |
| Trait/impl dispatch | Analysis | `SyslAnalyzer.scala` |
| `match` exhaustiveness | Analysis | `SyslAnalyzer.scala` |
| Contract lowering | Analysis + backend | `SyslAnalyzer.scala` + each codegen |
| String / ref layout | Backends | `SyslTriscCodegen.scala`, `SyslLLVMCodegen.scala` |
| Stack vs heap closures | Backends | same |
| `volatile` semantics | LLVM backend | `SyslLLVMCodegen.scala` |
| Multi-module ordering | Driver | `SyslDriver.scala` |
| `.smeta` cross-module cache | Driver + analyser | `ModuleMeta.scala` |

## Tracing a compile

<Tabs>
<TabItem label="Interpreter">

```bash
sbt "syslCliJVM/run run hello.sysl --trace-analyzer"
```

`--trace-analyzer` dumps every type decision the analyser makes. Use it when a generic
instantiation is going somewhere surprising.

</TabItem>
<TabItem label="TRISC">

```bash
sbt "syslCliJVM/run compile hello.sysl --emit asm -o hello.asm"
```

The emitted assembly is human-readable. Every function is labelled with its mangled name;
every runtime call (`malloc`, `__puts`, contract traps) is an explicit symbol.

</TabItem>
<TabItem label="LLVM">

```bash
sbt "syslCliJVM/run compile hello.sysl --backend llvm --emit ll -o hello.ll"
```

The emitted IR is normal LLVM. You can pipe it through `opt -O2 -S` to see what the
optimiser does with it.

</TabItem>
</Tabs>

## See also

- [Analyzer architecture](/implementation/analyzer-architecture/)
- [Source map](/implementation/source-map/)
- [Testing strategy](/implementation/testing/)
