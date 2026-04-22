---
title: Installation
description: Get the Sysl compiler running on your machine.
---

import { Steps, Tabs, TabItem, Aside, FileTree } from '@astrojs/starlight/components';

Sysl today lives inside the [trisc monorepo](https://github.com/edadma/trisc) — the same
repository as the TRISC assembler, emulator, linker, and the SLIX operating system. In the
future the compiler will move to its own repository; for now, everything ships together.

## Prerequisites

- **[sbt](https://www.scala-sbt.org/)** 1.9 or newer
- **JDK 17+** (any modern OpenJDK works — Temurin is a good default)
- **git**
- *(optional)* **clang** with LLVM 17+ if you want to use the LLVM backend

<Aside type="tip">
  sbt is configured with `-J-Xmx4g` in the repo's `.sbtopts`; the first compile will want
  a few gigs of RAM and five minutes of CPU time. Subsequent builds are incremental.
</Aside>

## Getting the source

<Steps>

1. Clone the repository.

   ```bash
   git clone https://github.com/edadma/trisc.git
   cd trisc
   ```

2. Verify the build.

   ```bash
   sbt compile
   ```

   On first run, sbt downloads Scala 3, the Scala Native and Scala.js toolchains, and a handful
   of other dependencies. Grab a coffee.

3. Run the test suite (optional but recommended).

   ```bash
   sbt test
   ```

</Steps>

## Running a Sysl program

Sysl has three backends, each invoked through `sbt`:

<Tabs>
<TabItem label="Interpreter">

```bash
sbt "syslCliJVM/run run examples/fibonacci.sysl"
```

The interpreter is the fastest way to iterate. It's also the reference implementation — if
the TRISC and LLVM backends disagree with it, the interpreter is right by definition.

</TabItem>
<TabItem label="TRISC">

```bash
# Compile to a TRISC Object File (.tof), link against the runtime, run the emulator.
sbt "syslCliJVM/run compile examples/fibonacci.sysl --emit tof -o /tmp/fib.tof"
sbt "triscCliJVM/run run /tmp/fib.tof"
```

The TRISC backend produces assembly you can read. Use `--emit asm` to dump assembly, or
`disasm` a `.tof` file to see what the linker did.

</TabItem>
<TabItem label="LLVM">

```bash
sbt "syslCliJVM/run compile examples/fibonacci.sysl --backend llvm --emit ll -o /tmp/fib.ll"
clang /tmp/fib.ll -o /tmp/fib
/tmp/fib
```

Full native compilation. The generated IR is normal LLVM — it goes through every optimiser
pass and produces machine code indistinguishable from hand-written C.

</TabItem>
</Tabs>

## Repository layout

Sysl's sources live under the `sysl/` subproject. Neighbour projects are the assembler,
emulator, linker, and the SLIX kernel.

<FileTree>
- trisc/
  - sysl/ **Sysl compiler and interpreter**
    - src/main/scala/io/github/edadma/trisc/
      - SyslLexer.scala indentation-sensitive tokeniser
      - SyslParser.scala AST producer
      - SyslAnalyzer.scala type checker and semantic analysis
      - SyslInterpreter.scala tree-walk interpreter
      - SyslTriscCodegen.scala TRISC assembly codegen
      - SyslLLVMCodegen.scala LLVM IR codegen
      - SyslDriver.scala multi-module orchestration
    - src/test/scala/ **the reference test suite**
  - sysl-cli/ command-line entry points (`sysl compile`, `sysl run`, `sysl test`, `sysl doc`)
  - std/ **standard library in literate Sysl (.lsysl)**
  - examples/ runnable sample programs
  - sysl-reference.md canonical normative reference
  - oskit/ SLIX kernel and servers — the biggest Sysl program in existence
  - trisc-cli/ assembler, linker, emulator front-end
</FileTree>

## Editor support

The site ships a TextMate grammar at `src/grammars/sysl.tmLanguage.json` used for syntax
highlighting here. Drop it into your editor's language support directory for colours. First-
class LSP is on the roadmap but not shipped yet — for now, the compiler's error messages are
the best feedback you get.

## Next steps

- [Hello, Sysl](/guides/hello-sysl/) — your first program on all three backends, with the
  generated assembly side-by-side.
- [Tour of the language](/guides/tour/) — a whirlwind tour of every major feature.
- [Systems programming in Sysl](/guides/systems-programming/) — MMIO, bare metal, and the
  no-allocator mode.
