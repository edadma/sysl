---
title: Compiler overview
description: A tour of the Sysl compiler — pipeline, backends, testing, and where each concern lives in the source tree.
---

import { CardGrid, LinkCard, FileTree, Aside } from '@astrojs/starlight/components';

The Sysl compiler is written in Scala 3. It runs on the JVM, on Node via Scala.js, and as a
native binary via Scala Native — the three cross-platform modules share almost all their
code. The full compiler is around 15,000 lines.

<FileTree>
- trisc/sysl/src/main/scala/io/github/edadma/trisc/
  - SyslLexer.scala indentation-sensitive tokeniser
  - SyslParser.scala grammar → AST
  - SyslAST.scala untyped AST nodes
  - SyslAnalyzer.scala semantic analysis and type checking
  - SyslTypedAST.scala typed IR consumed by the backends
  - SyslType.scala internal type representation
  - SyslInterpreter.scala reference tree-walk interpreter
  - SyslTriscCodegen.scala TRISC assembly codegen
  - SyslLLVMCodegen.scala LLVM IR codegen
  - SyslDriver.scala multi-module orchestration
  - ModuleMeta.scala cross-module metadata (.smeta)
  - SyslStdlib.scala standard-library metadata integration
</FileTree>

## Pipeline

<CardGrid>
  <Card title="Lexing" icon="pencil">
    `SyslLexer.scala` — indentation-aware tokenisation. Emits explicit `INDENT` /
    `DEDENT` / `NEWLINE` tokens, similar to Python's `tokenize` but with a custom
    approach to inline `do` / `then` forms.
  </Card>
  <Card title="Parsing" icon="puzzle">
    `SyslParser.scala` — a recursive-descent parser that produces the untyped AST
    (`SyslAST.scala`). Handles module declarations, imports, attributes, all
    expression and statement forms, generics, traits, literate `.lsysl` extraction,
    and conditional compilation.
  </Card>
  <Card title="Analysis" icon="approve-check">
    `SyslAnalyzer.scala` — name resolution, type inference, generic instantiation,
    trait resolution, exhaustiveness checking, contract typing, and typed AST
    output. The biggest single file in the compiler. Details on the
    [Analyzer Architecture](/implementation/analyzer-architecture/) page.
  </Card>
  <Card title="Backends" icon="rocket">
    Three of them: the interpreter (`SyslInterpreter.scala`) is the reference
    implementation; the TRISC backend (`SyslTriscCodegen.scala`) emits assembly
    for the teaching ISA; the LLVM backend (`SyslLLVMCodegen.scala`) emits IR.
    They all consume the same typed AST.
  </Card>
</CardGrid>

## Multi-module orchestration

`SyslDriver.scala` coordinates compilation across files:

1. **Discover** sources (`*.sysl` and `*.lsysl`) passed on the command line or found in a
   directory.
2. **Pre-parse** to extract module declarations and import lists.
3. **Topologically sort** the module graph — circular imports are an error.
4. **Compile** in dependency order. Each module's public symbols are cached in a `.smeta`
   file; downstream modules read the cache instead of re-parsing.
5. **Emit** once all modules have been analysed. The driver invokes the selected backend
   per module and links the outputs (for TRISC, via the same linker TRISC assembly uses).

`.smeta` files also carry `#pure`, `#deprecated`, and trait-impl information across module
boundaries so the analyser doesn't need to re-read a module's full source to check a call
site.

## Conditional compilation

`#if` / `#else` / `#endif` blocks are resolved in the driver, before the per-file analyser
runs. The driver maintains a set of defined symbols (`TARGET`, `DEBUG`, `BARE_METAL`, …),
strips the inactive branches, and passes the remaining source to the parser.

## `sysl-cli` — the command-line front-end

`sysl-cli/` wraps the compiler with commands:

| Command | What it does |
|---|---|
| `sysl run <file>` | Compile and run via the interpreter |
| `sysl compile <file>` | Produce `.tof`, `.asm`, or `.ll` output |
| `sysl test <file-or-dir>` | Discover and run `#test`-annotated functions |
| `sysl doc <file-or-dir>` | Render `.lsysl` prose to HTML |

## Tests

The test suite in `sysl/src/test/scala/` is the behavioural contract. It covers parser,
analyser, interpreter, and both codegens. Over 500 focused tests across 40+ files, organised
by feature area (generics, traits, tagged unions, contracts, closures, modules, …).

See [Testing Strategy](/implementation/testing/) for layout and workflow.

## See also

<CardGrid>
  <LinkCard title="Compiler pipeline" href="/implementation/pipeline/" description="Lexing → parsing → analysis → codegen, in detail." />
  <LinkCard title="Analyzer architecture" href="/implementation/analyzer-architecture/" description="Symbol tables, type resolution, generic instantiation, trait dispatch." />
  <LinkCard title="Source map" href="/implementation/source-map/" description="Where each concern lives in the codebase." />
  <LinkCard title="Testing strategy" href="/implementation/testing/" description="How the compiler is tested and how to add tests." />
</CardGrid>
