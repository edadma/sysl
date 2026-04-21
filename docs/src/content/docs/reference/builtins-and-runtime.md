---
title: Builtins and Runtime Semantics
description: Built-in functions, coercion, runtime safety, TRISC ABI, and conditional compilation.
---

## Builtin functions

| Function | Signature | Description |
|---|---|---|
| `putchar` | `(c: u32) -> u32` | Output single character |
| `print` | `(n: int)` | Print integer |
| `println` | `(n: int)` | Print integer with newline |
| `puts` | `(s: string)` | Print string |
| `len` | `(x) -> int` | Length of string, array, slice, or `&[]T` |
| `cap` | `(x) -> int` | Capacity of slice or `&[]T` |
| `append` | `(s: []T, elem: T) -> []T` | Append to slice (Go semantics) |
| `str` | `(x) -> string` | Convert int / bool / float to string |
| `string` | `(ptr: *T, len: int) -> string` | Construct string from pointer + length |
| `string` | `(s: []byte) -> string` | Construct string from byte slice |
| `malloc` | `(size: i64) -> *i8` | Allocate heap memory |
| `free` | `(ptr: *i8)` | Free heap memory |
| `calloc` | `(count: i64, size: i64) -> *i8` | Allocate zeroed memory |
| `realloc` | `(ptr: *i8, size: i64) -> *i8` | Resize allocation |
| `sbrk` | `(increment: i32) -> *i8` | Extend heap (POSIX) |
| `panic` | `(msg: string) -> unit` | Halt with message (trap 1, error code 4) |
| `assert` | `(cond: bool, msg: string) -> unit` | Panic with `msg` if `cond` is false |
| `expect` | `(actual: i64, expected: i64, msg: string) -> unit` | Panic with `"msg: expected N, got M"` if values differ |
| `abort` | `()` | Terminate execution (trap 1, error code 3) |
| `sizeof` | `(type-or-expr) -> int` | Size in bytes |

User-defined functions shadow builtins of the same name.

### Overflow intrinsics

`wrapping_add` / `wrapping_sub` / `wrapping_mul` / `saturating_add` / `saturating_sub` / `saturating_mul` — see the [Integer Overflow section of Types](/reference/types/#integer-overflow).

## Type compatibility and coercion

### Implicit widening

- Signed: `i8` → `i16` → `i32` → `i64`
- Unsigned: `u8` → `u16` → `u32` → `u64`
- Cross-sign: `u8` → `i16` (unsigned fits in wider signed)
- Float: `f32` → `f64`
- Int to float: any integer → `f32` or `f64`

### Mixed signed / unsigned

Operations between signed and unsigned types are allowed when the unsigned value fits entirely within the signed type's range:

```sysl
var b: byte = 200       // u8
var x: int = b + 1      // OK: u8 fits in i32
if b == 0 then ...      // OK: u8 compared with i32 literal

var big: u32 = 100
var y: int = big + 1    // ERROR: u32 doesn't fit in i32
```

### Explicit casts required

- `bool` ↔ `int`: use `int(flag)` or `bool(n)`
- `int` ↔ pointer: use `*i8(addr)` or `i64(ptr)`
- `*T` → `T` (except `self` inside methods): write `*ptr`

## Memory model

Sysl has three allocation modes: stack-allocated values, ref-counted `&T`, and raw `*T`. Ref-counted values can trigger a user-defined `Type.deinit()` when the refcount reaches zero.

For heap-allocated byte buffers, use `malloc` / `free`. For ref-counted arrays, `new [n]T` returns an `&[]T` that is freed automatically.

## Runtime safety

Codegen emits `trap 1` for runtime errors. On the OS, the trap handler terminates the faulting thread and outputs `!N` where N is the error code. On bare metal, execution halts.

| Error code | Condition |
|---|---|
| 1 | Array / slice index out of bounds |
| 2 | Null pointer (malloc returned null) |
| 3 | `abort()` called |
| 4 | `panic()` or `assert()` failure |

Contract violations (`require` / `ensure`) trap through the same path.

## Conditional compilation

```sysl
#if DEBUG
    var verbose = true
#else
    var verbose = false
#endif

#if !BARE_METAL
    import posix.stdlib.*
#endif

#if TARGET == "trisc"
    extern halt()
#endif

#if VERSION != "1.0"
    import new_api.*
#endif
```

**Condition forms:**

- `#if SYMBOL` — true if the symbol is defined and not `"false"`, `"0"`, or `""`
- `#if !SYMBOL` — negation
- `#if SYMBOL == "value"` — string equality
- `#if SYMBOL != "value"` — string inequality

## Calling convention (TRISC ABI)

| Register | Purpose |
|---|---|
| r0 | Zero register (hardwired to 0) |
| r1 | First argument / return value |
| r2 – r3 | Scratch (caller-saved) |
| r4 | Call address temp |
| r5 | Frame pointer |
| r6 | Link register (return address) |
| r7 | Stack pointer |

- At most one scalar argument in r1; additional arguments pushed right-to-left on the stack.
- Struct / string return: caller allocates a return slot and passes a hidden pointer as the first arg in r1.
- String arguments: 16 bytes `{ptr, len}` pushed on the stack.
- Closures use r3 as the hidden env pointer.
- `mul Rd, Rs1, Rs2` writes high bits to `r((d + 1) & 7)` — never use `mul r4` / `r5` / `r6` as destination.

## See also

- [Types](/reference/types/)
- [Arrays, Slices, and Pointers](/reference/arrays-slices-pointers/)
- [Functions](/reference/functions/)
- [Attributes and Tests](/reference/attributes-and-testing/)
