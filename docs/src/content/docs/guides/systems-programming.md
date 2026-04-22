---
title: Systems programming in Sysl
description: MMIO, bare metal, the no-allocator subset, and why Sysl makes kernel development bearable.
---

import { Aside, Tabs, TabItem } from '@astrojs/starlight/components';

This page is about what Sysl looks like when you're writing a kernel, a driver, or bare-metal
firmware — the places where you can't afford a garbage collector, can't assume malloc works,
and have to care about exactly which instructions hit the bus.

## The no-allocator subset

Sysl is usable without a heap allocator at all. The subset that works in this mode:

- Every value type (`i8`, `i64`, `f64`, `bool`, fixed arrays `[n]T`, structs, simple enums).
- Raw pointers `*T`, including `*T not null`.
- Inline `asm` blocks.
- Non-escaping closures that capture only primitives (`int`, `*T`, etc.) — the environment
  lives on the caller's stack frame.
- `const` initialised at compile time.
- Tagged unions, as long as you don't use `new`.

What you **don't** get without an allocator:

- `&T` ref-counted heap allocations (they call `malloc`).
- `new [n]T` dynamic arrays.
- `string` concatenation and `s"..."` / `f"..."` interpolation (which allocate).
- Closures that capture refcounted values (they promote the environment to the heap).

In practice, the [SLIX kernel](https://github.com/edadma/trisc/tree/dev/oskit) uses a
hand-rolled `std.alloc` plus `sbrk` for anything that needs a heap, and stays in the
no-allocator subset everywhere else.

## Memory-mapped I/O

The canonical example. Declare the device registers as a `volatile`-fielded struct, cast an
integer address to `*Regs not null`, and the compiler stops touching those accesses:

```sysl
struct UartRegs
    volatile status: u32           // RO — TX ready, RX data available, errors
    volatile data:   u32           // RW — read a byte, write a byte
    ier:             u32           // interrupt enable — not volatile
    baud:            u32

const UART_BASE = 0x10000000
const TX_READY  = 0x20
const RX_READY  = 0x01

uart_putc(c: int)
    val regs = *UartRegs not null(UART_BASE)

    while (regs.status & TX_READY) == 0 do { }

    regs.data = u32(c)

uart_getc() -> int
    val regs = *UartRegs not null(UART_BASE)

    while (regs.status & RX_READY) == 0 do { }

    int(regs.data)
```

The `not null` subtype means the cast traps immediately if `UART_BASE` is ever zero at a
produce site — cheap belt-and-braces safety.

<Aside type="tip" title="volatile semantics">
  LLVM emits `load volatile` / `store volatile` for every read and write of a `volatile`
  field. The TRISC backend doesn't optimise loads and stores at all, so `volatile` is a no-op
  on TRISC; but mark your MMIO structs anyway, for portability and readability.
</Aside>

## Inline assembly

Sysl's `asm` statement takes a raw TRISC or target-specific instruction as a string literal:

```sysl
// TRISC — halt the CPU.
halt_cpu() = asm("halt")

// TRISC — software trap.
raise_trap(n: int) = asm("trap 0")

// x86-64 via the LLVM backend.
cpuid(leaf: u32) = asm("cpuid")
```

Complex asm with inputs and outputs uses the LLVM backend's extended-asm syntax; the TRISC
backend accepts only bare instructions.

## Non-escaping closures on the stack

```sysl
// A simple search with a predicate — the closure captures `target` from the
// enclosing scope.  Because `target` is a primitive int and the parameter is
// not marked @escaping, the capture environment lives on the caller's stack.
find_first(xs: []int, pred: (int) -> bool) -> int
    for i, v in xs
        if pred(v) then return i
    -1

search(xs: []int, target: int) -> int
    find_first(xs, v -> v == target)     // no malloc, no free
```

By default, closure parameters are **non-escaping**: the callee promises not to store the
closure anywhere persistent, so the env can live on the caller's stack. The compiler trusts
the annotation. Mark the parameter `@escaping` if the callee needs to stash the closure in a
global, struct field, or return value — that forces the env onto the heap.

## Design by contract, at the device layer

Contracts are especially valuable in kernel code, where a bug shows up as a reboot instead of
a stack trace. Use them to document and enforce invariants at the driver API:

```sysl
struct RingBuffer
    data: *byte not null
    cap:  int
    head: int
    tail: int
    invariant 0 <= head < cap
    invariant 0 <= tail < cap
    invariant cap > 0

RingBuffer.push(b: byte) -> bool
    ensure (old(self.head) + 1) % self.cap == self.head || result == false
    val next = (self.head + 1) % self.cap

    if next == self.tail then return false   // full

    *(self.data + self.head) = b
    self.head = next
    true
```

Strip them with `--no-contracts` for release if the overhead matters. Every contract failure
uses the same trap path (`trap 1`, error code 4), so the kernel's panic handler already knows
what to do with it.

## Bare-metal hello world on TRISC

The [`examples/bare-metal-hello`](https://github.com/edadma/trisc/tree/dev/examples/bare-metal-hello)
directory is a complete "from the boot vector" example:

```sysl
// A linker script places this at 0x100, after the 20-entry vector table.
// The vector table's slot 0 is the initial SSP, slot 1 is the initial PC.
extern puts(s: string)

main() -> int
    puts("hello from bare metal")
    asm("halt")                             // no OS to return to
    0
```

No imports, no allocator, no OS. `puts` is an `extern` resolved at link time against the
runtime's UART driver. `asm("halt")` stops the CPU because there's nowhere to return.

Build it:

```bash
sbt "syslCliJVM/run compile examples/bare-metal-hello/hello.sysl --emit tof -o /tmp/hello.tof"
sbt "triscCliJVM/run link examples/bare-metal-hello/hello.ld /tmp/hello.tof -o /tmp/boot.tof"
sbt "triscCliJVM/run run /tmp/boot.tof"
```

## Pointers as integers

The cast `*T(addr)` is Sysl's only way to turn an integer into a pointer. It's explicit —
there's no implicit `int → *T` conversion anywhere in the language. That makes address
arithmetic self-documenting:

```sysl
const FB_BASE = 0x40000000
const FB_WIDTH = 640
const FB_HEIGHT = 480

put_pixel(x: int, y: int, rgb: u32)
    val fb = *u32(FB_BASE)
    *(fb + y * FB_WIDTH + x) = rgb
```

## Endianness, alignment, and layout

Structs are laid out in declaration order with natural alignment. No reordering. No padding
the compiler hides. The size of a struct is exactly the sum of its field sizes plus whatever
alignment-driven padding the target requires.

```sysl
struct Header                 // TRISC: 16 bytes total
    magic:  u32               // offset 0
    flags:  u16               // offset 4
    // 2 bytes padding to 4-byte align the next field
    length: u32               // offset 8
    checksum: u32             // offset 12
```

Use `sizeof(T)` to confirm. For packed layouts where padding is unacceptable, lay out with
`u8` arrays and read/write the fields explicitly.

## What to read next

- [Volatile in the type reference](/reference/types/#volatile)
- [`not null` pointers](/reference/types/#not-null-pointers)
- [Design by contract](/reference/functions/#design-by-contract--require--ensure)
- [Calling convention](/reference/builtins-and-runtime/)
