---
title: Tour of the language
description: A whirlwind tour of Sysl — types, memory, contracts, generics, and traits — with runnable examples throughout.
---

import { Aside, Tabs, TabItem } from '@astrojs/starlight/components';

Half an hour of reading should be enough to recognise every major feature of Sysl when you
see it in the wild. This page is a tour, not a reference — when you want precise semantics,
the [reference section](/reference/types/) is the source of truth.

## Variables and types

```sysl
val x = 42                      // immutable, inferred int
var y: int = 42                 // mutable, explicit type
var z: i64 = 1_000_000_000      // underscore separators allowed
const BASE = 0x1000             // compile-time constant, folded at use sites

volatile var mmio: u32 = 0      // no load/store optimisation
```

Sysl has the usual primitive types (`i8`..`i64`, `u8`..`u64`, `f32`, `f64`, `bool`, `unit`)
plus `string` as a 16-byte fat pointer and `char` as an alias for `u32` (so char literals
are Unicode codepoints, not bytes).

Integer arithmetic **never** implicitly widens. `u8 + u8` is `u8` and wraps on overflow. Cast
explicitly if you want more width:

```sysl
var a: u8 = 200
var b: u8 = 100
var big: int = int(a) + int(b)    // 300 — explicit widening
var wrap: u8 = a + b              // 44  — wrapping at u8 width
```

## Three ways to own memory

```sysl
struct Point
    x: int
    y: int

var v = Point(10, 20)             // value: Point, stack, no refcount
val r = new Point(10, 20)         // ref:   &Point, heap, refcounted
var p: *Point = &v                // ptr:   *Point, raw, unmanaged
```

- `new` moves the struct to the heap and gives you `&T`.
- `&value` takes the address and gives you `*T`.
- There is no path from `*T` back to `&T` — you can't manufacture a refcount.

Refs carry a header: a 64-bit refcount and a deinit pointer. When the refcount hits zero, the
deinit runs (child refs are decremented, string fields are freed), and the block returns to
the allocator.

## Tagged unions and pattern matching

```sysl
enum Shape
    Circle(radius: int)
    Rect(w: int, h: int)
    Empty

area(s: Shape) -> int
    s match
        Circle(r)   -> r * r * 3
        Rect(w, h)  -> w * h
        Empty       -> 0
```

Pattern matches are **exhaustive** on tagged unions. A missing variant is a compile error
that lists every variant you forgot. Guards (`Circle(r) if r > 0 -> ...`) don't count toward
exhaustiveness — the guard might be false.

Generic enums make `Option[T]` and `Result[T, E]` real first-class sum types:

```sysl
enum Option[T]
    Some(value: T)
    None

enum Result[T, E]
    Ok(value: T)
    Err(error: E)
```

And the postfix `?` operator unwraps the success variant or early-returns the failure:

```sysl
parse_pair(s: string) -> Option[int]
    val a = parseInt(s, 0)?      // returns None if parse fails
    val b = parseInt(s, 3)?
    Some(a + b)
```

## Control flow

```sysl
// if / elif / else — indentation-based blocks, like Python.
if x > 0
    positive()
elif x == 0
    zero()
else
    negative()

// Inline form for single-line bodies.
if x > 0 then positive()

// Match — no fallthrough.
x match
    1       -> doA()
    2, 3    -> doB()                  // multiple values per arm
    1..10   -> doSmall()              // range match
    _ if x > 0 -> doPositive()        // guards
    else    -> doDefault()

// Chained comparisons — parsed as a conjunction.
if 0 <= x <= 100 then grade(x)

// for/while — indentation blocks, with inline `do` for one-liners.
for i in 1..10 do print(i)            // inclusive, 1..10
for i in 0..<n do print(i)            // exclusive upper bound
for i in 10 downTo 0 step 2 ...       // counting down
for v in arr do process(v)            // each element
for i, v in arr do print(i, v)        // with index
```

Loops can be labelled and `break` / `continue` can target outer loops by name:

```sysl
outer: for i in 0..<n
    for j in 0..<m
        if grid[i][j] == target then break outer
        use(grid[i][j])
```

## Functions

```sysl
// Expression body.
add(a: int, b: int) -> int = a + b

// Block body — indentation-sensitive.
factorial(n: int) -> int
    if n <= 1 then return 1
    return n * factorial(n - 1)

// Inferred return type.
double(x: int) = x * 2

// Default parameters, named arguments.
greet(x: int, y: int = 10) -> int = x + y
greet(1, y = 20)                        // named arg, optional
```

Functions can be generic, carry trait bounds, and monomorphise per concrete type:

```sysl
max[T: Ord](a: T, b: T) -> T
    if a > b then a else b
```

Closures use arrow syntax and capture **by value**:

```sysl
apply(f: (int) -> int, x: int) -> int = f(x)

main() -> int = apply(x -> x * 2, 21)   // 42
```

Non-escaping closures that only capture primitives live on the caller's stack — no malloc,
usable from kernels and bare-metal code.

## Design by contract

```sysl
sqrt(x: f64) -> f64
    require x >= 0.0
    ensure result >= 0.0
    ensure result * result <= x + 1.0e-6
    var r = x / 2.0
    for _ in 0..20 do r = 0.5 * (r + x / r)
    r
```

- `require` runs once on entry.
- `ensure` runs before every return, with `result` bound to the return value.
- `old(expr)` captures a value at function entry for use inside `ensure`.

On top of function contracts there are **loop contracts**:

```sysl
var remaining = 100
while remaining > 0
    variant remaining                   // must strictly decrease each iteration
    invariant remaining >= 0            // must hold at every iteration
    remaining = remaining - step()
```

And **struct invariants**:

```sysl
struct Range
    lo: int
    hi: int
    invariant lo <= hi
```

All three can be stripped with `--no-contracts` for a release build.

## Range-typed integers

```sysl
type Age = int within 0..150
type Prob = f64 within 0.0..<1.0
type Even = int where value % 2 == 0

enum Day
    Mon; Tue; Wed; Thu; Fri; Sat; Sun

for d in Day::Range do print(Day::Image(d))  // "Mon", "Tue", ...

Day::First              // Mon
Day::Succ(Day.Tue)      // Wed
Age::Valid(180)         // false, never traps

if Age::Valid(raw) then
    var a: Age = raw    // the range check will pass
```

A range-constrained type is a subtype of its base — no cast needed. A `new` range type is
nominally distinct: `type Meters = new f64 within 0.0..1e9`, and `Meters` values can't be
added to raw `f64`s.

## Traits and operator overloading

```sysl
trait Ord[T]
    cmp(a: T, b: T) -> int
    lt(a: T, b: T) -> bool = cmp(a, b) < 0    // default body
    gt(a: T, b: T) -> bool = cmp(a, b) > 0

struct Vec2
    x: int
    y: int

impl Add[Vec2]
    add(a: Vec2, b: Vec2) -> Vec2 = Vec2(a.x + b.x, a.y + b.y)

main() -> int
    val c = Vec2(1, 2) + Vec2(10, 20)       // desugars to Add.add(...)
    c.x * 100 + c.y
```

Operators on user types are trait-method calls the compiler resolves statically. Built-in
numeric operators use native instructions as before.

## Strings and interpolation

```sysl
val name = "Ada"
val age  = 36
puts(s"hello, $name")                       // interpolation
puts(f"$name is $age%03d years old")        // printf-style
```

`s"..."` interpolates; `f"..."` adds printf-style format specifiers (`%d`, `%x`, `%08X`,
`%-10s`, and the rest). Plain `"..."` literals never interpolate — `$` is just a character.

## Volatile and MMIO

```sysl
struct UartRegs
    volatile status: u32
    volatile data:   u32

const UART_BASE = 0x10000000
uart_putc(c: int)
    val regs = *UartRegs not null(UART_BASE)
    while (regs.status & 0x20) == 0 do { }
    regs.data = u32(c)
```

Struct fields marked `volatile` are never coalesced or reordered by the LLVM backend. The
TRISC backend doesn't optimise loads and stores at all, so `volatile` is a no-op there.

## Literate programming

`.lsysl` files are Markdown with indented code blocks. Prose at column 0 is documentation;
anything indented four or more spaces is source code.

```
    module demo

    hello() -> int = 42

More prose — explaining why `hello` returns 42.

    answer() -> int = hello() + 0
```

The compiler tangles the indented blocks; the documentation toolchain (`sysl doc`) renders
the prose with syntax-highlighted code. The entire standard library is written this way — you
can read the spec and the implementation in the same file.

## Unit tests

```sysl
#test
test_add_basic() -> unit
    assert(1 + 1 == 2, "math is broken")

#test(should_panic: "out of range")
test_bounds_trap() -> unit
    var arr: [3]int
    val _ = arr[10]                 // traps: expected
```

`sysl test <path>` finds every `#test`-annotated function in the file or directory and runs
them with pass/fail output and timings. `should_panic` and substring-matched expected messages
let you test the failure paths too.

## What comes next

- [Systems programming in Sysl](/guides/systems-programming/) — MMIO, bare metal, the
  no-allocator subset.
- [Language reference](/reference/types/) — precise, normative semantics.
- [Standard library](/stdlib/overview/) — what you get for free.
