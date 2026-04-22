---
title: Testing strategy
description: How the Sysl compiler is tested, and how to add tests when you change behaviour.
---

import { Aside, Tabs, TabItem } from '@astrojs/starlight/components';

The compiler's test suite is the behavioural contract. Every language feature has a dedicated
test file; every bug fix lands with a regression test; every backend is exercised against the
interpreter's reference behaviour.

## Layout

Tests live under `sysl/src/test/scala/io/github/edadma/trisc/`. File naming is by feature:

| Area | Example files |
|---|---|
| Parser / syntax | `SyslModuleSyntaxTests.scala`, `SyslLoopSyntaxTests.scala`, `SyslTypeSyntaxTests.scala` |
| Analysis / type checking | `SyslAnalyzerTests.scala`, `SyslTypeCheckTests.scala`, trait / generic tests |
| Runtime semantics | control flow, refs, slices, strings, pointer behaviour |
| Language features | `SyslTryOpTests.scala`, `SyslTaggedUnionTests.scala`, `SyslStringInterpolationTests.scala`, `SyslClosureTests.scala`, `SyslContractTests.scala` |
| Backend-specific | `SyslLLVMTests.scala`, `SyslTriscCodegenTests.scala` |

## Framework

Tests use ScalaTest `FreeSpec`. Test helpers provide compact runners:

```scala
class ExampleTests extends AnyFreeSpec {
  "fibonacci returns 55 for n=10" in {
    val result = run("""
      fib(n: int) -> int
          if n <= 1 then return n
          fib(n - 1) + fib(n - 2)

      main() -> int = fib(10)
    """)
    result shouldBe 55
  }
}
```

`run(source)` compiles and evaluates via the interpreter. `output(source)` captures stdout
instead. Backend-specific runners (`runTrisc`, `runLlvm`) round-trip through the codegen and
an emulator or compiler-and-run.

## Running tests

<Tabs>
<TabItem label="All tests">

```bash
sbt test
```

Runs every test across every backend. Takes a few minutes.

</TabItem>
<TabItem label="One file">

```bash
sbt "testOnly *SyslContractTests"
```

Pattern-matches against the class name.

</TabItem>
<TabItem label="One test">

```bash
sbt "testOnly *SyslContractTests -- -z 'old captures pointee'"
```

`-z` is a substring match against the test's full name.

</TabItem>
</Tabs>

## When you change behaviour

1. **Add a test first**, under the closest matching feature file — or start a new file if
   there isn't one.
2. **Make sure it fails** the way you expect before making your change.
3. **Implement** the change.
4. **Re-run the full test suite** — behaviour cross-cuts files more than you'd think, and the
   three backends can diverge in ways nobody expects.

If your change is behaviour-visible, update the [language reference](/reference/types/) as
well — the reference is the normative spec, the test suite is the enforcement mechanism, and
they should agree.

<Aside type="caution" title="Backends must agree">
  If a test passes under the interpreter but fails under TRISC or LLVM codegen, the
  interpreter is the reference — the failing backend has the bug. Opposite-case bugs are rare
  and usually expose a missing feature in the interpreter.
</Aside>

## Literate-mode tests

Any `#test`-annotated function inside a `.lsysl` file runs under `sysl test`. The
[standard library](/stdlib/overview/) uses this for every module — the tests live next to
the code and the prose.

```bash
sysl test std/crypto/sha256/sha256.lsysl
```

Running the literate tests is a useful smoke test for the whole compiler: if they pass, the
lexer, parser, analyser, interpreter, and the standard-library surface are all working.

## See also

- [Attributes and testing](/reference/attributes-and-testing/) — the `#test` attribute and
  related runtime helpers.
- [Literate Sysl](/reference/literate-sysl/) — running tests inside `.lsysl` files.
