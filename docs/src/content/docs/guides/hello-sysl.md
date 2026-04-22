---
title: Hello, Sysl
description: Your first program on all three backends, with the generated assembly side-by-side.
---

import { Tabs, TabItem, Steps, Aside } from '@astrojs/starlight/components';

Create a file called `hello.sysl`:

```sysl title="hello.sysl"
// Every executable Sysl program exports a function called `main` that returns
// an `int` — the process exit code.  Strings print via `puts`; expressions
// print via `println`.

main() -> int
    puts("hello, sysl")
    0
```

That's it. There's no preamble, no `stdio.h`, no module declaration required for a standalone
file. `puts` is a builtin; so are `print`, `println`, `putchar`, `len`, `append`, and a
handful of others.

## Running it

<Tabs>
<TabItem label="Interpreter">

```bash
sbt "syslCliJVM/run run hello.sysl"
```

Output:

```
hello, sysl
```

The interpreter is single-threaded and walks the typed AST directly. It's fast enough for
development and slow enough that you'll want to move to one of the native backends for real
work.

</TabItem>
<TabItem label="TRISC">

```bash
sbt "syslCliJVM/run compile hello.sysl --emit tof -o /tmp/hello.tof"
sbt "triscCliJVM/run run /tmp/hello.tof"
```

Output:

```
hello, sysl
```

TRISC is a 16-bit RISC teaching ISA — five instruction formats, a couple dozen instructions,
deterministic cycle counts. Kernels, drivers, and whole operating systems fit inside
well under a megabyte.

</TabItem>
<TabItem label="LLVM">

```bash
sbt "syslCliJVM/run compile hello.sysl --backend llvm --emit ll -o /tmp/hello.ll"
clang /tmp/hello.ll -o /tmp/hello
/tmp/hello
```

Output:

```
hello, sysl
```

The LLVM backend emits IR that goes through the standard optimisation pipeline. Compile times
are clang's, runtime performance is clang's, and the binary has no Sysl runtime dependency
beyond a handful of intrinsics.

</TabItem>
</Tabs>

## What the compiler produced

Under the hood, `puts("hello, sysl")` compiles to:

1. A read-only string constant holding the twelve bytes `hello, sysl\0`.
2. A call to the runtime's `puts` symbol with the string's fat pointer
   `{ptr: *u8, len: i64}` packed into registers.
3. `return 0` in the calling convention's return slot.

Everything else — string layout, the `(ptr, len)` calling convention, the `main` symbol, the
return path — is the same across all three backends. The same source file produces bytecode
for the interpreter, TRISC assembly, and LLVM IR without changing a line.

## A slightly bigger hello

Interpolated strings (`s"..."`), format strings (`f"..."`), and generics all work on every
backend too:

```sysl title="hello-bigger.sysl"
greet[T](name: T) = puts(s"hello, $name")

main() -> int
    greet("world")
    greet(42)                       // auto-converted via str()
    greet(3.14)

    val version = 1
    val build   = 0xBEEF
    puts(f"sysl v$version.0 build $build%04x")

    0
```

Output:

```
hello, world
hello, 42
hello, 3.140000
sysl v1.0 build beef
```

The `greet[T]` function is monomorphised — one copy per concrete `T` — exactly like Go or
Rust. There is no boxing, no interface dispatch, no heap allocation for the call.

## Where to go next

- [Tour of the language](/guides/tour/) — walk through every major feature with runnable
  code.
- [Functions](/reference/functions/) — block bodies, expression bodies, defaults, named
  arguments, contracts, closures, generics.
- [Standard library](/stdlib/overview/) — what you get for free.
