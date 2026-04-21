---
title: Testing Strategy
description: Current test surface for parser, analyzer, runtime, and language features.
---

## Test location

Primary tests live under:

- `trisc/sysl/src/test/scala/io/github/edadma/trisc`

## Coverage shape

The suite includes dedicated files for:

- parser/syntax (`SyslModuleSyntaxTests`, `SyslLoopSyntaxTests`, `SyslTypeSyntaxTests`)
- semantic analysis/type checking (`SyslAnalyzerTests`, `SyslTypeCheckTests`, trait/generic tests)
- runtime/interpreter semantics (control flow, refs, slices, strings, pointer behavior)
- language features (`SyslTryOpTests`, `SyslTaggedUnionTests`, `SyslStringInterpolationTests`, etc.)
- backend-specific checks (`SyslLLVMTests`)

## Practical workflow

- Add or update behavior in `sysl/` implementation.
- Add focused tests in `src/test/...` for parser/analyzer/runtime impacts.
- Keep docs in this site aligned with both `sysl-reference.md` and test-backed behavior.

## See also

- [Analyzer Architecture](/implementation/analyzer-architecture/)
- [Compiler Pipeline](/implementation/pipeline/)
- [Attributes and Tests](/reference/attributes-and-testing/)
