---
title: Analyzer Architecture
description: Symbol tables, type resolution, generic specialization, and semantic checks.
---

## Role

`SyslAnalyzer` performs semantic analysis and builds typed AST:

- scope/symbol resolution
- function/global/type registration
- compatibility checks and coercions
- generic and trait machinery
- expression/statement typing

## Core subsystems

- Symbol and scope stacks for locals/globals
- Type resolution over named, pointer/ref, array/slice, tuple, and function types
- Generic template caches and instantiation maps
- Trait/impl registries and operator dispatch map
- Match-pattern analysis and expected-type flow for inference

## Main source

- `trisc/sysl/src/main/scala/io/github/edadma/trisc/SyslAnalyzer.scala`

## Related definitions

- typed AST: `trisc/sysl/src/main/scala/io/github/edadma/trisc/SyslTypedAST.scala`
- type model: `trisc/sysl/src/main/scala/io/github/edadma/trisc/SyslType.scala`

## See also

- [Compiler Pipeline](/implementation/pipeline/)
- [Source Map](/implementation/source-map/)
- [Type System](/reference/types/)
- [Generics](/reference/generics/)
- [Traits and Operator Dispatch](/reference/traits-and-operators/)
