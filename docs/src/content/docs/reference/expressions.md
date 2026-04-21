---
title: Expressions and Operators
description: Literals, operators, casts, if/if-is, sizeof, and the `?` try operator.
---

## Literals

```sysl
42                    // int (i32)
0xFF                  // hex literal
100u32                // typed literal suffix
3.14                  // double (f64)
1.5e10                // scientific notation
'A'                   // char literal (u32, value 65)
'\n'                  // escape char
"hello"               // string literal
true, false           // bool
[1, 2, 3]             // array literal
1_000_000             // underscore separators (decimal, hex, float, exponent)
0xDEAD_BEEF           // grouping for readability
0xFF_00_FF_00u32      // combined with type suffix
3.141_592             // underscores in fractional part
```

### Type suffixes

Integer literals accept a type suffix that forces a specific type:

```sysl
42i8   42i16   42i32   42i64
200u8  1000u16 100u32  0xFFu64
```

Float literals (`3.14`, `1e5`) default to `f64` but coerce to `f32` in context (`var x: f32 = 1.5` works without a cast). Mixed-width float arithmetic widens to the wider operand; `f64 → f32` requires explicit `f32(x)`.

:::note
TRISC stores `f32` as 4 bytes in memory but works with it as `f64` in registers (using `f32tof64` / `f64tof32` at memory boundaries). LLVM uses native `float` throughout.
:::

### Escape sequences

```
\n    newline
\t    tab
\r    carriage return
\0    null (0x00)
\\    literal backslash
\'    literal single quote
\"    literal double quote
\xNN  hex byte (e.g., \x1b for ESC)
```

## Operators (by precedence, lowest to highest)

| Precedence | Operators | Associativity |
|---|---|---|
| 1 | `\|\|` | left |
| 2 | `&&` | left |
| 3 | `==` `!=` `<` `>` `<=` `>=` | left (chainable) |
| 4 | `\|` | left |
| 5 | `^` | left |
| 6 | `&` | left |
| 7 | `<<` `>>` | left |
| 8 | `+` `-` | left |
| 9 | `*` `/` `%` | left |
| 10 | `-` `!` `~` `*` `&` `++` `--` (prefix) | right |
| 11 | `[]` `.` `()` `++` `--` (postfix) | left |

Note: bitwise operators bind tighter than comparisons (unlike C), so `x & mask == 0` works as expected.

## Chained comparisons

```sysl
if 1 <= x <= 10 then ...    // equivalent to: 1 <= x && x <= 10
if a < b < c < d then ...   // all pairs checked, short-circuits
```

## Increment / decrement

```sysl
++x    // prefix: increments x, returns new value
x++    // postfix: returns old value, then increments
--x    // prefix decrement
x--    // postfix decrement
```

## Compound assignment

```sysl
x += 5    x -= 3    x *= 2    x /= 4    x %= 7
x &= 0xFF x |= 0x01 x ^= 0xAA x <<= 2   x >>= 1

// Also on pointers (scaled by element size)
p += 2    p -= 1
```

## Casts

```sysl
// Numeric
int(true)         // bool -> int: 1
bool(42)          // int -> bool (nonzero)
byte(0x1FF)       // truncate to u8: 255
char(65)          // int -> u32: 65
i64(3.14)         // float -> int: 3
f32(3.14)         // f64 -> f32 (precision narrowing)
f64(x: f32)       // f32 -> f64 (lossless; also implicit)

// Pointer / int conversions
*i8(address)      // int -> pointer
*Point(address)   // int -> struct pointer
i64(ptr)          // pointer -> int
bool(ptr)         // pointer -> bool (null = false)

// Pointer-to-pointer
*byte(charPtr)    // *T -> *U (any pointer to any pointer)
*i8(refVal)       // &T -> *i8 (ref to raw pointer)

// Function pointers
i64(funcPtr)      // func -> int (address)
bool(funcPtr)     // func -> bool (non-null = true)

// Array decay (address of first element)
*byte(arr)        // [N]T -> *byte
*i64(arr)         // [N]T -> *i64
i64(arr)          // [N]T -> i64 (address as integer)
string(arr, len)  // [N]byte + len -> string
```

## sizeof

```sysl
sizeof(int)        // 4
sizeof(*int)       // 8
sizeof(Point)      // sum of fields + padding
sizeof([10]int)    // 40
```

## If expression

```sysl
x = if cond then a else b
result = if x > 0 then x else -x
```

## If-is (pattern matching in if)

`if expr is Pattern then ...` is sugar for a single-arm `match`. The pattern binds variables in the then-branch (like Rust's `if let`):

```sysl
// Extract value or use default
val v = if r is Ok(x) then x else -1

// Guard with pattern
if o is Some(x) then
    process(x)

// Block body with else
if parse(s) is Ok(val, pos) then
    handle(val, pos)
else
    report_error()
```

Desugars to `match` at parse time — no new analyzer or runtime machinery.

## `?` operator (try)

The postfix `?` operator on an enum value unwraps the success variant or early-returns the failure variant from the enclosing function. The standard ergonomic for `Option[T]` and `Result[T, E]`.

```sysl
enum Option[T]
    Some(value: T)
    None

parseAndDouble(s: string, start: int) -> Option[int]
    x = parseInt(s, start)?        // unwrap Some(x), or early-return None
    Some(x * 2)
```

**Rules:**

- Applies to monomorphized generic enum values where the enum has exactly two variants and the first variant has exactly one field (the success type).
- The enclosing function's return type must be the **same** enum type as the value being `?`-unwrapped (no error-type conversion yet).
- `expr?` desugars at analyze time to:
  ```
  match expr
      Success(v) -> v
      Failure(...) -> return Failure(...)
  ```
- The result type of `expr?` is the success variant's field type.

`a?.field` chains with field access; multiple `?`s across separate statements also work.

## Call forms and member access

- direct calls: `f(x)`
- method calls: `obj.method(...)`
- indirect function-pointer / closure calls: `(*f)(x)` or `f(x)`
- indexing and slicing: `a[i]`, `a[lo:hi]`
- chained indexing: `grid[i][j]`
- field access: `p.x`, auto-deref for `*T`: `ptr.field` desugars to `(*ptr).field`

## See also

- [Types](/reference/types/)
- [Statements and Control Flow](/reference/statements-and-control-flow/)
- [Traits and Operators](/reference/traits-and-operators/)
- [Strings](/reference/strings/)
- [Arrays, Slices, and Pointers](/reference/arrays-slices-pointers/)
