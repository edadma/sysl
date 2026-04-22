---
title: Introduction
description: What Sysl is, what problems it solves, and how it compares to the languages you already know.
---

Sysl is a systems programming language. It's the language the [SLIX](https://github.com/edadma/trisc)
microkernel is written in, the language the [TRISC](https://github.com/edadma/trisc) teaching
emulator is bootstrapped from, and the language a growing [standard library](/stdlib/overview/)
— regex engines, crypto, containers, I/O — is built on.

It has three backends: a tree-walk interpreter, a codegen to the 16-bit TRISC teaching ISA,
and an LLVM codegen for native performance. The same source compiles on all three.

## Design principles

**Know where your bytes live.**
Every type has a known size and a known allocation site. `var v = Point(1, 2)` is on the
stack. `val r = new Point(1, 2)` is on the heap, refcounted. `var p: *Point = &v` is a raw,
unmanaged pointer. These are three different types with three different rules, and the
compiler enforces the difference.

**No implicit integer promotion.**
`u8 + u8` produces a `u8` that wraps. If you want `u8 + u8 → int`, you widen by hand:
`int(a) + int(b)`. This matches Go, Rust, and Swift. C's integer conversion rules are a
source of bugs; Sysl opts out.

**The type system carries your intent.**
`type Age = int within 0..150` is a real subtype with runtime bounds checking. `*T not null`
is a real subtype that traps at produce sites. `#pure` is a static check that a function
performs no observable side effects. Assertions and invariants are in the source, not in
comments.

**One language, many scales.**
Non-escaping closures capture on the caller's stack — no allocator needed. `volatile` on a
struct field stops the compiler from touching a memory-mapped register. Inline `asm` blocks
let you drop to the metal when you have to. The same language writes boot code, a scheduler,
a shell, and a regex engine.

## What Sysl is not

- **Not a garbage collector.** Refs use a header-based refcount. When the count hits zero,
  the `deinit` runs, children are decremented, and the block is freed. Cycles leak unless you
  break them with a raw pointer — intentional, to keep the runtime predictable.

- **Not async-first.** There are no futures or coroutines in the language today. Concurrency
  is whatever your platform provides (kernel threads on SLIX, pthreads under POSIX, bare
  interrupts on TRISC).

- **Not a successor to C++.** The standard library is small and opinionated. There is no
  exception machinery; failures are `Result[T, E]` or a trap. Operator overloading is
  limited to traits the compiler recognises (`Add`, `Ord`, …) — no custom operators, no
  ADL.

## Quick comparison

| Feature                       | Sysl                                   | C                      | Go             | Rust                |
|-------------------------------|----------------------------------------|------------------------|----------------|---------------------|
| Value / ref / raw pointer     | ✅ all three, first-class               | raw pointers only      | implicit       | `T`, `Rc<T>`, `*T`  |
| Ref counting                  | built-in `&T`                           | by hand                | GC             | `Rc<T>` / `Arc<T>`  |
| Tagged unions + pattern match | ✅                                     | no                     | no             | ✅                  |
| Design by contract            | `require` / `ensure` / `invariant`      | no                     | no             | via macros          |
| Range-constrained types       | `int within 0..150`                     | no                     | no             | no                  |
| Literate programming          | `.lsysl`                                | no                     | no             | no                  |
| No implicit int promotion     | ✅                                     | 😬                     | ✅             | ✅                  |
| LLVM target                   | ✅                                     | ✅                     | via gccgo      | ✅                  |
| Teaching-ISA target           | ✅ (TRISC)                              | kind of                | no             | no                  |

## Where to go next

- If you want to **run something now**, jump to [Installation](/guides/installation/) and
  [Hello, Sysl](/guides/hello-sysl/).
- If you want **a tour of the language** before committing, read [Tour of the language](/guides/tour/).
- If you're deciding whether Sysl fits **your problem**, read
  [Systems programming](/guides/systems-programming/).
- If you want **precise semantics**, the [Language reference](/reference/types/) is the
  source of truth.
