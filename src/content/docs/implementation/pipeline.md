---
title: Compiler Pipeline
description: How SysL source flows from parse through analysis to execution/codegen.
---

## High-level stages

Based on `trisc/sysl/src/main/scala/io/github/edadma/trisc`:

1. **Lexing** (`SyslLexer.scala`)  
   Tokenization with indentation-aware lexical rules and literal handling.
2. **Parsing** (`SyslParser.scala`)  
   Produces untyped AST (`SyslAST.scala`) from source.
3. **Analysis / typing** (`SyslAnalyzer.scala`)  
   Name resolution, type checking, generic instantiation, trait/impl resolution, typed AST output (`SyslTypedAST.scala`).
4. **Backend execution/codegen**
   - interpreter: `SyslInterpreter.scala`
   - TRISC backend: `SyslTriscCodegen.scala`
   - LLVM backend: `SyslLLVMCodegen.scala`

`SyslDriver.scala` orchestrates multi-source compilation, module/import handling, conditional compilation resolution, dependency ordering, and metadata flow.

## Key orchestration behavior

- Source parsing and conditional declaration resolution
- Import/module extraction and topological sort
- Package/module metadata caching via `ModuleMeta`
- Extern-name no-mangle registration across units

## See also

- [Source Map](/implementation/source-map/)
- [Analyzer Architecture](/implementation/analyzer-architecture/)
- [Modules and Imports](/reference/modules-and-imports/)
