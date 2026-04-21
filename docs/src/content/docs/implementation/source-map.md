---
title: Source Map
description: Where major SysL implementation concerns live in the codebase.
---

## Core sources (`trisc/sysl/src/main/scala/io/github/edadma/trisc`)

- `SyslLexer.scala`: tokenization and lexical grammar details
- `SyslParser.scala`: grammar and AST construction
- `SyslAST.scala`: untyped AST node model
- `SyslAnalyzer.scala`: semantic analysis, type system, generic/trait machinery
- `SyslTypedAST.scala`: typed IR used by execution/codegen
- `SyslInterpreter.scala`: runtime model for interpreter backend
- `SyslTriscCodegen.scala`: TRISC backend emission
- `SyslLLVMCodegen.scala`: LLVM backend emission
- `SyslDriver.scala`: end-to-end compilation orchestration
- `SyslType.scala`: internal type representation
- `ModuleMeta.scala`: symbol metadata interchange (`.smeta`)
- `SyslStdlib.scala`: stdlib module metadata integration

## Language reference source

- `trisc/sysl-reference.md`: canonical language manual currently used as primary specification text.

## See also

- [Compiler Pipeline](/implementation/pipeline/)
- [Analyzer Architecture](/implementation/analyzer-architecture/)
- [Testing Strategy](/implementation/testing/)
