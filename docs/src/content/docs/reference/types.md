---
title: Type System
description: Scalar types, composite types, enums, tagged unions, type declarations.
---

Sysl has value types, reference-counted refs (`&T`), and raw pointers (`*T`) as three orthogonal ways to use the same data. Most types are fixed-width and laid out without hidden overhead.

## Scalar types

| Type | Alias | Size | Description |
|------|-------|------|-------------|
| `i8` | | 1 byte | signed 8-bit integer |
| `i16` | `short` | 2 bytes | signed 16-bit integer |
| `i32` | `int` | 4 bytes | signed 32-bit integer |
| `i64` | `long` | 8 bytes | signed 64-bit integer |
| `u8` | `byte` | 1 byte | unsigned 8-bit integer |
| `u16` | `ushort` | 2 bytes | unsigned 16-bit integer |
| `u32` | `char`, `uint` | 4 bytes | unsigned 32-bit integer (Unicode codepoint) |
| `u64` | `ulong` | 8 bytes | unsigned 64-bit integer |
| `f32` | `float` | 4 bytes | IEEE-754 single-precision |
| `f64` | `double` | 8 bytes | IEEE-754 double-precision |
| `bool` | | 1 byte | `true` / `false` |
| `unit` | | 0 bytes | no value |
| `string` | | 16 bytes | fat pointer `{ptr: *u8, len: i64}` |

## Integer overflow

All integer arithmetic wraps at the declared type width. There is **no implicit integer promotion** — `u8 + u8` produces `u8`, not `int`. To avoid wrapping, widen explicitly: `int(a) + int(b)`.

- Unsigned types wrap via modular arithmetic: `u8(255) + u8(1)` → `0`
- Signed types wrap via two's complement: `int(2147483647) + 1` → `-2147483648`
- `i64` / `u64` use the full register width and do not truncate

This matches Go / Rust / Swift. C-style implicit integer promotion is not used.

### Overflow intrinsics

For explicit overflow behaviour, use the polymorphic intrinsics. All take two operands of the same integer type and return the same type:

| Intrinsic | Behaviour |
|-----------|-----------|
| `wrapping_add(a, b)` | Two's-complement wrap on overflow |
| `wrapping_sub(a, b)` | Two's-complement wrap on underflow |
| `wrapping_mul(a, b)` | Low bits of the true product |
| `saturating_add(a, b)` | Clamp to the type's MAX (MIN for signed underflow) |
| `saturating_sub(a, b)` | Clamp to the type's MIN (0 for unsigned) |
| `saturating_mul(a, b)` | Clamp to MAX/MIN on overflow |

```sysl
var a: u8 = 200
var b: u8 = 100
wrapping_add(a, b)     // 44   (300 & 0xFF)
saturating_add(a, b)   // 255  (clamped to u8 MAX)
saturating_sub(b, a)   // 0    (clamped to u8 MIN)
```

:::note
TRISC backend does not yet support `saturating_*` on 64-bit types or `saturating_mul` on `u32`. LLVM backend supports all widths.
:::

## Composite types

```sysl
*T              // raw pointer (8 bytes, unmanaged)
*T not null     // raw pointer constrained to be non-null at produce sites
&T              // ref-counted reference (8 bytes, auto-freed at rc=0)
[n]T            // fixed-size array (n * sizeof(T), stack-allocated)
[]T             // slice: {ptr, len, cap} (16 bytes)
&[]T            // ref-counted heap array (from `new [n]T`)
(T1, T2, T3)    // tuple (desugars to anonymous struct)
(P1, P2) -> R   // function pointer / closure (16 bytes: {func_ptr, env_ptr})
```

### `not null` pointers

`*T not null` is a subtype of `*T` with a runtime check: every assignment, parameter bind, return, or cast that produces a `*T not null` value verifies the pointer is non-null. A null value traps at the produce site. The check is inserted via the same `where`-predicate mechanism used for user-defined predicates (a synthesized checker function per inner type).

`*T not null` is pointer-compatible with `*T`, so it can be passed anywhere a `*T` is expected.

## Struct types

```sysl
struct Point
    x: int
    y: int

struct Node
    value: int
    next: *Node          // recursive via pointer
```

### Struct invariants

A struct may declare one or more `invariant <bool>` clauses among its fields. Each invariant is type-checked at declaration time (must be `bool`) and re-evaluated at every check site on a value of that struct type. Bare field names are in scope; module-level `const`s and globals are also in scope.

```sysl
struct Account
    balance: int
    limit: int
    invariant balance >= -limit

struct Range
    lo: int
    hi: int
    invariant lo <= hi
    invariant hi - lo <= 100        // multiple clauses: all must hold
```

A violating mutation traps via the standard contract-check path. Invariants fire on:

- **construction-site init:** `var a: Account = Account(0, 100)`
- **whole-value reassignment:** `a = Account(50, 100)`
- **field assignment:** `a.balance = -200` (also through pointer/ref: `(*p).balance = -200`)
- **field compound assignment:** `a.balance -= 50`

The invariant expression is re-evaluated at each check site, so an invariant that mentions a side-effecting expression re-runs those side effects. Use plain field reads.

`--no-contracts` strips the runtime check while keeping the type check.

## Enums (simple)

Simple enums are integer constants with auto-incrementing values:

```sysl
enum Color
    Red                  // 0
    Green                // 1
    Blue = 10            // explicit value
    Yellow               // 11 (auto-increment)
```

Access via `Color.Red`. Simple enums can also serve as distinct types in type positions; bare variant names work as constructors:

```sysl
enum ParseError
    EmptyInput
    BadDigit
    Overflow

parse(s: string) -> Result[i64, ParseError]
    if len(s) == 0 then return Err(EmptyInput)
    Ok(42)
```

## Tagged unions (data enums)

Variants can carry data (Rust-style tagged unions):

```sysl
enum Shape
    Circle(radius: int)
    Rect(w: int, h: int)
    Empty
```

**Construction:**

```sysl
s = Circle(5)
e = Empty                // bare name
e2 = Shape.Empty         // qualified form also works
```

**Pattern matching:**

```sysl
s match
    Circle(r) -> r * r * 3
    Rect(w, h) -> w * h
    Empty -> 0
    Circle(r) if r > 10 -> 1    // guard with binding
```

**Exhaustiveness.** A `match` on a data-enum value must cover every variant, or include a wildcard `_ -> ...` or `else -> ...` default. Missing variants produce a compile error listing them. Guarded arms (`Circle(r) if r > 0 -> ...`) do not count toward exhaustiveness since the guard may be false. Non-enum matches (on integers or strings, for example) do not require exhaustiveness — the user is responsible for covering their own domain.

**Heap-allocated enums (`new` on variants).** `new Variant(args)` heap-allocates an enum value and returns a ref-counted `&EnumType`. Enables recursive data structures:

```sysl
enum Expr
    Lit(value: int)
    Add(left: &Expr, right: &Expr)

eval_expr(e: &Expr) -> int
    *e match
        Lit(v) -> v
        Add(l, r) -> eval_expr(l) + eval_expr(r)
```

**Memory layout.** `{tag: i32, padding, data: union-of-variants}`. `sizeof(EnumType)` returns total size including tag and padding.

## Type declarations

Two orthogonal modifiers compose, plus optional runtime checks:

```sysl
type Callback = (int) -> int                           // plain alias (transparent)
type Age      = int within 0..150                      // subtype: base-compatible, range-checked
type Meters   = new f64                                // derived: nominally distinct
type SafeAge  = new int within 0..150                  // derived + constrained
type Even     = int where value % 2 == 0               // arbitrary predicate
type PosEven  = int within 0..100 where value % 2 == 0 // within + where combined
```

| Form | Base-compatible? | Runtime check? |
|------|------------------|----------------|
| `type A = B` | yes | no |
| `type A = B within r` | yes | range |
| `type A = B where p` | yes | predicate |
| `type A = new B` | no | no |
| `type A = new B within r` | no | range |
| `type A = new B where p` | no | predicate |
| `type A = [new] B within r where p` | … | both |

**Range syntax.** Bounds must be numeric literals (including `char`, which is `u32`) or references to a `const`; a unary sign is allowed.

| Syntax | Meaning | Example |
|--------|---------|---------|
| `lo..hi` | inclusive `[lo, hi]` | `type Age = int within 0..150` |
| `lo..<hi` | exclusive upper `[lo, hi)` | `type Prob = f64 within 0.0..<1.0` |

**Where predicates.** `where <bool-expr>` attaches a boolean predicate. Inside the predicate, `value` refers to the value being checked. The predicate runs at every produce site (assignment, parameter bind, return, explicit cast). Unlike `within`, `where` predicates are not compile-time folded — even for literal arguments.

**Compatibility.**

- **Subtypes** (without `new`) are transparently compatible with the base type; runtime checks fire on each produce site.
- **Derived types** (with `new`) are nominally distinct from the base and from other derived types over the same base. Mixing them in arithmetic or assignment is a compile error. Use `Meters(3.0)` to wrap and `f64(m)` to unwrap. Arithmetic between two values of the same derived type yields that derived type.
- Out-of-range literal bounds are caught at compile time; runtime violations trap.

### Type aliases

Plain aliases are transparent names for existing types:

```sysl
type IntPtr = *int
type Callback = (int) -> int
```

## Type attributes (`T::Attr`)

Range-constrained types and simple enums expose their metadata through `::`-suffixed attributes. They work like Ada's `'Attr` notation, retargeted to sysl's `::` separator.

```sysl
type Age = int within 0..150
enum Day { Mon; Tue; Wed; Thu; Fri; Sat; Sun }

Age::First           // 0
Age::Last            // 150
Age::Range           // for-loop sugar (see below)

Day::First           // Mon  (value 0)
Day::Last            // Sun  (value 6)
Day::Image(d)        // "Tue"      for d = Day.Tue
Day::Value("Tue")    // Day.Tue    parses a string back to its variant
Day::Pos(d)          // 1          for d = Day.Tue
Day::Val(2)          // Day.Wed    variant at position n
Day::Succ(d)         // Wed        for d = Day.Tue
Day::Pred(d)         // Mon        for d = Day.Tue

Age::Succ(a)         // a + 1, traps if a is already 150
Age::Pred(a)         // a - 1, traps if a is already 0
Age::Valid(raw)      // bool — true iff raw is in 0..150, never traps
```

| Attribute     | Applies to                              | Result                                                |
|---------------|-----------------------------------------|-------------------------------------------------------|
| `T::First`    | `within`-constrained int, simple enum   | lower bound / first variant's value                   |
| `T::Last`     | `within`-constrained int, simple enum   | upper bound (minus 1 if `..<`) / last variant         |
| `T::Range`    | same                                    | only valid in `for i in T::Range` — inclusive scan    |
| `T::Image(x)` | simple enum, constrained numeric type   | variant name string / `str(x)` for numerics           |
| `T::Value(s)` | simple enum                             | variant whose name equals `s`; traps on no match      |
| `T::Valid(x)` | `within`-constrained int, simple enum   | bool — does `x` satisfy the constraint? never traps   |
| `T::Pos(x)`   | simple enum                             | 0-based declaration position                          |
| `T::Val(n)`   | simple enum                             | variant at position `n`; traps on out-of-range        |
| `T::Succ(x)`  | `within`-constrained int, simple enum   | next value; traps at the upper end                    |
| `T::Pred(x)`  | `within`-constrained int, simple enum   | previous value; traps at the lower end                |

**Lowering.** `::First` and `::Last` fold to compile-time constants. `::Valid` on a `within`-int type folds to an inline `x >= lo && x <= hi` (or `< hi` for `..<`). The rest lower to synthesized helper functions (`__image_T`, `__value_T`, `__valid_T`, `__pos_T`, `__val_T`, `__succ_T`, `__pred_T`) emitted once per target type.

**Trapping.** `::Pos` / `::Value` on an unknown input, `::Val` on an out-of-range position, `::Succ` past the upper bound, and `::Pred` past the lower bound all trap via the standard contract-check path. `::Valid` is the non-throwing complement — it returns a bool so the caller can branch:

```sysl
if Age::Valid(raw) then
    var a: Age = raw           // safe: the range check will pass
```

`::Value` and `::Image` round-trip: `T::Value(T::Image(x)) == x` for every variant `x`.

**`::Range` is for-loop sugar.** `for i in T::Range body` parses as `for i in T::First..T::Last body`. `for i in reverse T::Range body` desugars the other way, `for i in T::Last downTo T::First body`. Using `::Range` outside a for-loop is a compile error. See [Statements and Control Flow](/reference/statements-and-control-flow/) for the loop forms.

**Limitations.**

- Float-based `within` types do not yet support `::First` / `::Last` (and therefore none of the others).
- `::Valid` is not yet stripped by `--no-contracts` since it is introspection, not a contract trap. Every other `::*` trap path is elided when contracts are disabled — invalid input then yields garbage (`-1` for enum helpers, `v+1` / `v-1` past the bound for `within` `::Succ`/`::Pred`).

## Three allocation modes

The same struct definition supports three usage modes:

| Declaration | Type | Semantics |
|---|---|---|
| `var v = Point(10, 20)` | `Point` | stack-allocated, bitwise copy |
| `val r = new Point(10, 20)` | `&Point` | heap-allocated, ref-counted |
| `var p: *Point = &v` | `*Point` | raw, unmanaged |

### Conversion rules

- `ref → value`: not implicit (use `.copy()`)
- `value → ref`: `new Point(v)`
- `ref → ptr`: `&r` (unsafe, no refcount change)
- `ptr → ref`: **always an error** (can't manufacture a refcount)
- `value → ptr`: `&v` (address-of)
- `ptr → value`: `*p` (dereference); also implicit for `self` only

## Volatile

The `volatile` qualifier prevents the compiler from optimising away, reordering, or coalescing loads and stores. Use it for MMIO registers and shared-memory variables.

```sysl
volatile var mmio_status: u32 = 0
volatile var shared_flag: int

struct UartRegs
    volatile status: u32
    volatile data: u32
    baud: int           // non-volatile, normal optimisation allowed
```

The LLVM backend emits `load volatile` / `store volatile`. The TRISC backend is unaffected — it does not optimise loads/stores.

## See also

- [Generics](/reference/generics/)
- [Arrays, Slices, and Pointers](/reference/arrays-slices-pointers/)
- [Traits and Operators](/reference/traits-and-operators/)
- [Expressions and Operators](/reference/expressions/)
