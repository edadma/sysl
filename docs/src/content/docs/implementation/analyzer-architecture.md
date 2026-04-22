---
title: Analyzer architecture
description: Symbol tables, type inference, generic monomorphisation, trait dispatch, and contract lowering — inside SyslAnalyzer.
---

import { Aside } from '@astrojs/starlight/components';

`SyslAnalyzer` is the compiler's type-checker, semantic-analyser, and lowering front-end all
in one. It takes the untyped AST from the parser and produces a fully typed AST that the
backends consume.

## Responsibilities

- **Name resolution.** Locals, module-level symbols, imports (including wildcard and aliased
  imports), module qualifications like `math.sqrt(x)`, and enum variant names both bare
  (`Some(42)`) and qualified (`Option.Some(42)`).
- **Type inference and type checking.** A Hindley-Milner style inference for expressions
  with explicit annotations everywhere they matter (function parameters, return types, `var`
  / `val` declarations). Integer literals carry a default type that can be refined by
  context.
- **Generic monomorphisation.** Every `(generic function, type args)` pair is instantiated
  once. Each instantiation is cached by its mangled name.
- **Trait resolution.** `impl Ord[int]` registers implementations in a per-trait, per-type
  map. Calls through a trait method (including operator overloads) look up the matching
  impl statically — no runtime dispatch.
- **Exhaustiveness checking.** `match` on a tagged-union value must cover every variant or
  include a `_ / else` default.
- **Contract lowering.** `require` / `ensure` / loop `variant` / struct `invariant` are
  typed here and lowered to explicit trap-on-false checks before the backend sees them.
- **Coercions.** Implicit widening for numeric types (`u8 → i16`, `f32 → f64`), array
  decay (`[n]T → *T`), and string-to-pointer (`string → *u8`).

## Data structures

| Structure | What it holds |
|---|---|
| Scope stack | Lexically-nested symbol tables. Push on entering a block, pop on leaving. |
| Type environment | Named types, type aliases, generic parameter bindings during instantiation. |
| Generic template cache | Source AST for each generic function, keyed by short name. |
| Instantiation cache | Already-monomorphised `(name, type-args)` pairs. Deduplicates repeated calls. |
| Trait registry | `Map[TraitName, Map[TypeKey, ImplAST]]`. |
| Operator dispatch map | Maps `(operator, left-type)` to a trait method. |
| Typed AST builder | Accumulates the typed output nodes as analysis proceeds. |

## Generic instantiation flow

1. Call-site arguments are type-checked against the generic signature. This *pins* the type
   parameters by unification.
2. The instantiation cache is queried. If the `(function, type-args)` pair exists, the cached
   mangled name is used.
3. Otherwise, the generic's source AST is cloned with the type parameters substituted, a new
   mangled name is generated (`swap_i32`, `Pair_i32`, …), and the clone is recursively
   analysed in a type environment that binds the type parameters to the pinned types.
4. Operations that are invalid for the concrete type (`>` on `bool`, for example) produce a
   clear error at the call site where the instantiation happens, not at the generic's
   definition site.

## Trait dispatch flow

1. A call to `Ord.lt(a, b)` or `a < b` on a user type is resolved by the operator dispatch
   map to the `Ord.lt` method.
2. The concrete argument types are used as the type key into the trait registry.
3. If a matching `impl Ord[T]` exists, its method is called directly (mangled name:
   `Ord_lt_T`).
4. If no match and the method has a default body, the default body is monomorphised with
   the concrete type and called.
5. If neither matches, the analyser reports the missing impl.

<Aside type="tip" title="Why static dispatch">
  Sysl's traits are monomorphic at the call site. This keeps the language honest about cost
  — every trait-method call is an ordinary direct call. No vtables, no boxed values, no
  hidden allocation.
</Aside>

## Contract lowering

A function body with contracts:

```sysl
sqrt(x: f64) -> f64
    require x >= 0.0
    ensure result >= 0.0
    <body>
```

becomes, after analysis:

```
function sqrt(x: f64) -> f64 {
    if !(x >= 0.0) then trap_with_message("precondition")
    var result: f64 = <body-with-implicit-return-rewritten>
    if !(result >= 0.0) then trap_with_message("postcondition")
    return result
}
```

`old(expr)` inside `ensure` allocates a hidden snapshot local initialised at function entry.
Loop `variant` expressions are hoisted into a per-loop witness variable and checked on each
iteration boundary.

`--no-contracts` strips the check branches while retaining the type check — the precondition
expression still needs to be well-typed even if it won't run.

## Typed AST shape

`SyslTypedAST.scala` defines the output form. Every expression node carries a concrete
`SyslType` (from `SyslType.scala`); there are no open type variables left. Backends never
have to do type inference — they just walk the tree and emit code.

## Where to look for which bug

| Symptom | Likely suspect |
|---|---|
| "type X does not implement trait Y" | Trait registry / impl lookup |
| Generic function called with wrong types | Template cache / pinning logic |
| Non-exhaustive match that passes | `match` analysis for tagged unions |
| Implicit cast missing or wrong | Coercion table |
| Contract not firing | Contract lowering in analyser, or `--no-contracts` |
| Closure capture type wrong | Capture inference in the analyser |

## See also

- [Compiler pipeline](/implementation/pipeline/)
- [Source map](/implementation/source-map/)
- [Generics reference](/reference/generics/)
- [Traits and operators reference](/reference/traits-and-operators/)
