---
title: Generics
description: Generic functions, structs, tagged unions, trait bounds, and monomorphization.
---

Sysl supports parametric polymorphism for functions, structs, and tagged unions. The compiler **monomorphizes** each unique set of type arguments — one specialized copy per combination, just like Go generics or C++ templates. There is no runtime dispatch for generics.

## Generic functions

Type parameters are declared in square brackets after the function name. Type arguments are inferred from call-site argument types.

```sysl
// Identity — works for any type
id[T](x: T) -> T = x

// Swap via pointers
swap[T](a: *T, b: *T)
    var tmp: T = *a
    *a = *b
    *b = tmp

// Multiple type parameters
pair_first[K, V](k: K, v: V) -> K = k

// Instantiation-time checking: operations on T checked when T is pinned.
// max[int] works; max[bool] errors because > is not defined for bool.
max[T](a: T, b: T) -> T
    if a > b then a else b

main() -> int
    var x = 10
    var y = 20
    swap(&x, &y)        // T inferred as int
    max(1.5, 2.5)       // T inferred as f64
    id(42)              // T inferred as int
```

### Trait bounds

A type parameter may be constrained to types that implement one or more traits:

```sysl
maxOf[T: Ord](a: T, b: T) -> T         // T must implement Ord
bothCheck[T: Ord + Eq](a: T, b: T)     // T must implement Ord AND Eq
```

Bounds are checked at each call site when concrete type arguments are known. An unsatisfied bound produces a clear error naming the missing trait and the type parameter. Inside the generic body, operators like `a > b` and `a == b` route through the bounded trait's methods.

### Rules

- Type parameters may appear in parameter types, return type, and local variable type annotations.
- Type arguments are **inferred** from argument types (explicit type arguments are a future phase).
- Each unique `(function, type-args)` combination produces one specialized copy, cached and mangled (e.g. `swap_i32`).
- Operations on a type parameter invalid for the concrete type produce an error at the call site.

## Generic structs

```sysl
struct Pair[T]
    a: T
    b: T

struct Tuple[K, V]
    key: K
    value: V

main() -> int
    p = Pair(10, 20)                   // T inferred as int
    q: Pair[i64] = Pair(1i64, 2i64)
    t = Tuple(5, 'A')
    p.a + p.b + int(q.a) + t.key
```

### Rules

- Type parameters appear in square brackets after the struct name.
- Field types may reference the type parameters.
- Constructor calls infer type arguments from argument types.
- Explicit type annotations (`Pair[int]`) may be used in variable declarations and parameter types.
- Each `(struct, type-args)` pair produces one monomorphized struct with a mangled name (e.g. `Pair_i32`, `Tuple_i32_u32`).
- Generic functions and generic structs compose — `swapPair[T](p: *Pair[T])` is fully supported.

### Methods on generic structs

Generic structs can carry methods. Include the type parameters after the struct name:

```sysl
struct MinHeap[T]
    data: []T
    less: (T, T) -> bool

MinHeap[T].len() -> int = len(self.data)

MinHeap[T].push(v: T)
    self.data = append(self.data, v)
    self._sift_up(len(self.data) - 1)
```

`MinHeap[T].push(v: T)` desugars into a generic function `MinHeap_push[T](__self__: *MinHeap[T], v: T)`. Calling `h.push(42)` on a `MinHeap[int]` instantiates `MinHeap_i32_push`. Trait bounds work the same way:

```sysl
MinHeap[T: Ord].sorted_push(v: T)
```

## Generic tagged unions

Tagged unions may declare type parameters — the foundation for `Option[T]`, `Result[T, E]`, and similar sum types.

```sysl
enum Option[T]
    Some(value: T)
    None

enum Result[T, E]
    Ok(value: T)
    Err(error: E)

safeDiv(a: int, b: int) -> Option[int]
    if b == 0 then None
    else Some(a / b)

main() -> int
    r = safeDiv(20, 4)
    r match
        Some(v) -> v
        None -> -1
```

### Rules

- Type parameters in square brackets after the enum name.
- Variant field types may reference the type parameters.
- Each `(enum, type-args)` pair produces one monomorphized `EnumType` (e.g. `Option_i32`, `Result_i32_string`).
- Pattern matching uses the scrutinee's concrete enum type to look up variants.

### Type inference

Variant constructors prefer to infer type args from argument types (`Some(42)` infers `T = int`). When a variant doesn't pin all type parameters — e.g. `Ok(42)` for `Result[T, E]` leaves `E` unknown — the analyzer consults the **expected type** from context:

| Context | Expected type source |
|---|---|
| `var x: Option[int] = None` | the declared variable type |
| `fn f() -> Result[int, string] { Ok(42) }` | the function's return type |

Without an expected type and incomplete argument-based inference, the compiler errors asking for an explicit type annotation.

## See also

- [Type System](/reference/types/)
- [Traits and Operators](/reference/traits-and-operators/)
- [Functions](/reference/functions/)
- [Analyzer Architecture](/implementation/analyzer-architecture/)
