---
title: Traits and Operators
description: Trait declarations, impl blocks, default methods, and operator overloading.
---

Traits describe a set of methods a type may implement. Each trait is parameterized by a subject type `T` (the type that will conform). Methods may have default bodies; implementers override or inherit them. There is **no orphan rule** — any `impl` may be written anywhere.

## Traits and `impl` blocks

```sysl
trait Ord[T]
    cmp(a: T, b: T) -> int                  // required (no body)
    lt(a: T, b: T) -> bool = cmp(a, b) < 0  // default body
    le(a: T, b: T) -> bool = cmp(a, b) <= 0
    gt(a: T, b: T) -> bool = cmp(a, b) > 0
    ge(a: T, b: T) -> bool = cmp(a, b) >= 0

impl Ord[int]
    cmp(a: int, b: int) -> int = a - b

main() -> int
    if Ord.lt(3, 5) then 1 else 0
```

### Rules

- A trait method with a body is a **default**; implementers may override it.
- A trait method without a body is **required**; every impl must provide it.
- `impl Trait[T]` for the same `(trait, type)` pair may appear only once.
- Calls via `Trait.method(args)` infer the concrete target type from argument types and dispatch to the matching impl's method.
- Inside a default body, unqualified calls to sibling trait methods (`cmp(a, b)` inside `lt`) resolve to the current impl's methods.

## Monomorphization

Each impl method — whether provided or synthesized from a default — compiles to a mangled top-level function (`Ord_cmp_i32`, `Ord_lt_i32`, etc.). There is **no runtime dispatch** — trait calls are resolved statically.

## Trait bounds on generics

Generic type parameters can require trait conformance:

```sysl
maxOf[T: Ord](a: T, b: T) -> T             // T must implement Ord
bothCheck[T: Ord + Eq](a: T, b: T)         // T must implement Ord AND Eq
```

Bounds are checked at each call site when the concrete type arguments are known.

## Operator overloading via traits

Operators on user-defined struct and enum types desugar to trait method calls. The compiler maps each operator to a fixed `(trait, method)` pair and dispatches through the impl registered for the operand type.

| Operator | Trait | Method | Signature |
|---|---|---|---|
| `<` `<=` `>` `>=` | `Ord` | `lt` `le` `gt` `ge` | `(T, T) -> bool` |
| `==` `!=` | `Eq` | `eq` `ne` | `(T, T) -> bool` |
| `+` | `Add` | `add` | `(T, T) -> T` |
| `-` | `Sub` | `sub` | `(T, T) -> T` |
| `*` | `Mul` | `mul` | `(T, T) -> T` |
| `/` | `Div` | `div` | `(T, T) -> T` |

```sysl
struct Vec2
    x: int
    y: int

trait Add[T]
    add(a: T, b: T) -> T

impl Add[Vec2]
    add(a: Vec2, b: Vec2) -> Vec2 = Vec2(a.x + b.x, a.y + b.y)

main() -> int
    a = Vec2(1, 2)
    b = Vec2(10, 20)
    c = a + b                  // desugars to Add.add(a, b) → Add_add_Vec2(a, b)
    c.x * 100 + c.y
```

Built-in numeric operators are unaffected — `3 + 4` on `int` still uses the native instruction. Dispatch through a trait only applies when the left operand is a struct or enum type.

Operator sugar composes with generic functions. Inside `max[T](a: T, b: T)`, writing `a > b` works for any `T` with an `Ord` impl, checked at instantiation time.

## See also

- [Generics](/reference/generics/)
- [Type System](/reference/types/)
- [Expressions and Operators](/reference/expressions/)
- [Analyzer Architecture](/implementation/analyzer-architecture/)
