---
title: Functions
description: Function forms, contracts, defaults, named arguments, methods, closures.
---

Functions are the core abstraction. Sysl supports expression-body and block-body forms, default parameters, named arguments, design-by-contract clauses, generics, methods on (possibly generic) structs, and closures.

## Function forms

```sysl
// Expression body
add(a: int, b: int) -> int = a + b

// Block body
factorial(n: int) -> int
    if n <= 1
        return 1
    return n * factorial(n - 1)

// Void function (no return type)
greet(name: *byte)
    puts(name)

// Inferred return type
double(x: int) = x * 2

// No parameters
getAnswer() -> int = 42

// Statement body — for/while/do-while may follow `=`
uart_puts(s: string) = for c in s do uart_putc(int(c))
wait_ready() = while !ready() do noop()
```

## Design by contract — `require` / `ensure`

A block-body function can declare preconditions and postconditions at the top of its body:

```sysl
sqrt(x: f64) -> f64
    require x >= 0.0
    ensure result >= 0.0
    ensure result * result <= x + 1.0e-6
    var r = x / 2.0
    for _ in 0 downTo 20 step 1 do r = 0.5 * (r + x / r)
    r
```

- **`require <bool> [, "message"]`** — evaluated once on function entry. Traps if false.
- **`ensure <bool> [, "message"]`** — evaluated before every return (including implicit fall-through). Traps if false.
- Multiple clauses are allowed, in any order, but must all appear before the first regular statement.
- Checks go through the standard trap path (same as range checks).

### Optional failure message

A comma-separated string literal after the condition is included in the runtime error, matching Scala's `require(cond, msg)`:

```sysl
pos(x: int) -> int
    require x >= 0, "x must be non-negative"
    ensure result > 0, "pos() result must be positive"
    x + 1
```

On failure the error reads `precondition check failed: x must be non-negative` (or `postcondition check failed: ...` for `ensure`). The bare `require <cond>` / `ensure <cond>` form still works and falls back to the generic kind word. Surfaced in the LLVM and interpreter backends; TRISC and SVM currently trap with a fixed error code.

**`result` in `ensure` clauses.** Inside an `ensure` expression, `result` refers to the function's return value. Outside `ensure` it is a normal identifier.

**`old(expr)` in `ensure` clauses.** Captures the value of `expr` at function entry, before any body statement runs:

```sysl
increment(p: *int)
    ensure *p == old(*p) + 1
    *p = *p + 1
```

`old()` may only appear inside `ensure`. Each `old(expr)` allocates a hidden snapshot local initialized at the top of the body, so later mutations do not affect what `old()` sees. `old()` accepts arbitrary expressions (derefs, field accesses, calls), but `old(old(...))` is rejected.

Contracts are not yet supported on expression-body functions or on closures.

### Disabling contracts (`--no-contracts`)

Pass `--no-contracts` to `sysl compile` or `sysl run` to elide every runtime contract check at compile time:

```bash
sysl compile --no-contracts main.sysl
sysl run --no-contracts main.sysl
```

This is the equivalent of Ada's `pragma Assertion_Policy(Disable)` — the user takes responsibility for correctness in exchange for zero runtime overhead. The flag strips:

- `require` / `ensure` clauses on functions
- `invariant` statements inside loops
- `variant` statements inside loops (the entire hoisted check state goes away)
- struct `invariant` clauses (no per-assignment check)
- `where`-predicate bodies (the synthesized predicate function still runs but performs no check)
- `within`-range checks — both the compile-time literal check and the runtime range check
- enum `T::Pos` / `T::Val` / `T::Value` / `T::Succ` / `T::Pred` and `within`-int `T::Succ` / `T::Pred` traps (the helper still returns a value, but invalid input yields garbage: `-1` for enum helpers, `v+1` / `v-1` past the bound for `within` helpers)

What is **not** affected:

- **Type checking.** Contract clauses still type-check at compile time regardless of the flag — only the runtime traps are elided. A clause that fails to compile still fails to compile with `--no-contracts`.
- **`T::Valid(x)`.** Non-throwing introspection, never stripped.
- **`#pure`.** A static analysis pass that always runs.
- **`assert(cond, msg)`, `panic`, `abort`.** Explicit runtime calls, not contracts.

The driver also accepts the equivalent build-config setting `contracts = "off"` (or `"false"` / `"disabled"`).

## Default parameter values

Parameters may have default values. Any parameter with a default must come at the end of the parameter list; once a parameter has a default, all later parameters must too.

```sysl
val BASE = 100

greet(x: int, y: int = 10) -> int = x + y
compute(x: int, k: int = BASE * 2) -> int = x + k

main() -> int
    greet(32)          // 42 — uses default y=10
    greet(32, 100)     // 132 — explicit y=100
    compute(42)        // 242 — k defaults to 200
```

Default expressions are re-evaluated per call (not cached). They can reference module-level `val`s and constants, but not other parameters or local variables. Constant defaults are folded by the analyzer. Default values are not yet supported on generic functions.

## Named arguments

Call arguments can be passed by name using `name = expr`. Named arguments can appear in any order, mix with positional arguments (positional first), and work with defaults — including skipping middle ones:

```sysl
greet(x: int, y: int, z: int = 0) -> int = x * 100 + y * 10 + z

main() -> int
    greet(1, 2, 3)                  // positional
    greet(x = 1, y = 2, z = 3)      // all named
    greet(y = 2, x = 1, z = 3)      // any order
    greet(1, z = 3, y = 2)          // mixed
    greet(1, z = 3)                 // skip middle (y uses default if it had one)
```

Errors: positional after named, unknown parameter name, duplicate named argument, named argument that conflicts with a positional one.

Named arguments are currently supported for regular function calls, struct constructors, and builtins — not yet for generic function instantiation, method calls, or trait methods.

## `def` — auto-call functions

`def` declares a zero-argument function that is automatically called when referenced by bare name. Unlike `val`, a `def` is re-evaluated on every reference and supports forward references (for mutual recursion):

```sysl
var counter = 0
def next_id = counter++         // return type inferred

def pi -> int = 314             // explicit return type

def greeting -> string          // block body
    "hello"

main() -> int
    val a = next_id             // auto-called: 0
    val b = next_id             // auto-called: 1
    a + b + pi                  // 0 + 1 + 314 = 315
```

`&name` gives the function pointer for a `def`:

```sysl
apply_thunk(f: () -> int) -> int = f()

apply_thunk(&next_id)           // passes next_id as a function pointer
```

`def` is also accepted before functions with parameters, where it is purely documentary.

## Methods

Methods are declared with the `StructName.methodName(...)` syntax. The parser prepends a hidden `__self__: *StructName` parameter automatically — **do not write `self` in the parameter list**. Refer to `self` inside the body:

```sysl
struct Point
    x: int
    y: int

Point.magnitude() -> int
    self.x * self.x + self.y * self.y

main() -> int
    var p: Point
    p.x = 3
    p.y = 4
    p.magnitude()                // desugars to Point_magnitude(&p)
```

Inside the body, `self` has type `*StructName` (raw pointer to the instance).

### Methods on generic structs

```sysl
struct MinHeap[T]
    data: []T
    less: (T, T) -> bool

MinHeap[T].len() -> int = len(self.data)

MinHeap[T].push(v: T)
    self.data = append(self.data, v)
    self._sift_up(len(self.data) - 1)
```

`MinHeap[T].push(v: T)` desugars to the generic function `MinHeap_push[T](__self__: *MinHeap[T], v: T)`. Calling `h.push(42)` on a `MinHeap[int]` instantiates `MinHeap_i32_push`. Trait bounds work on generic methods exactly as on generic functions:

```sysl
MinHeap[T: Ord].sorted_push(v: T)
```

## Deinit blocks

```sysl
struct Buffer
    data: *byte
    size: int

Buffer.deinit()
    free(self.data)              // called automatically when &Buffer rc = 0
```

## Defer

```sysl
main() -> int
    f = open("file.txt", O_RDONLY)
    defer close(f)                // runs when function exits
    // ... use f ...
    42                            // close(f) runs after return value is computed
```

Multiple defers execute in LIFO order.

## Function pointers

```sysl
dbl(x: int) -> int = x * 2

main() -> int
    f: (int) -> int = dbl
    f(21)                         // indirect call → 42

    var funcs: [2](int) -> int
    funcs[0] = dbl
    funcs[1] = triple
    funcs[0](10) + funcs[1](10)  // call through array
```

## Closures

Anonymous functions that can capture variables from their enclosing scope. They use the `->` arrow syntax:

```sysl
f = x -> x + 1                    // single param
g = (x, y) -> x + y               // multiple params
h = () -> 42                      // zero params
f = (x: int) -> x * 2             // with type annotations

transform = x ->                  // multi-line body
    val doubled = x * 2
    doubled + 1
```

**Capture semantics.** Closures capture variables **by value** (copy at creation). Mutations to the original after the closure is created do not affect the captured value:

```sysl
var a = 10
f = x -> x + a        // captures a = 10
a = 100
f(32)                  // 42 (captured a is still 10)
```

To share mutable state, capture a pointer (`*T`) or ref (`&T`).

**Type inference.** Parameter types are inferred from context when the closure is passed to a function expecting a specific `(...) -> T`:

```sysl
apply(f: (int) -> int, x: int) -> int = f(x)

main() -> int = apply(x -> x + 1, 41)    // x inferred as int
```

**Closure returning a closure:**

```sysl
make_adder(n: int) -> (int) -> int
    val captured = n
    x -> x + captured

add10 = make_adder(10)
add10(32)                                  // 42
```

### Escaping closures

Function parameters are **non-escaping by default** — the closure's captured environment is stack-allocated. Use `@escaping` to mark parameters where the callee may store the closure beyond the call's lifetime:

```sysl
// Non-escaping (default): env lives on caller's stack frame
sort_by(arr: []int, cmp: (int, int) -> bool)

// Escaping: env is heap-allocated via malloc
on_click(handler: @escaping () -> unit)
```

Non-escaping closures avoid heap allocation, but the compiler trusts the annotation — storing a non-escaping closure into a global, struct field, or returning it is undefined behaviour. Closures with no expected type context (e.g., `val f = x -> x + 1`) default to escaping.

**Implementation.** All function values (including plain function pointers) are 16-byte fat pointers: `{func_ptr: i64, env_ptr: i64}`. Plain function pointers have `env_ptr = 0`. Environment allocation depends on capture types:

- **Non-escaping, all captures non-rc-bearing** (ints, raw pointers, etc.): env is allocated on the caller's stack — no malloc, no free. Makes closures usable in no-allocator (kernel / bare-metal) contexts.
- **Escaping, OR any rc-bearing capture** (strings, refs, string-containing structs): env is heap-allocated with a `[rc: i64 @ -16 | deinit_ptr: i8* @ -8 | data]` header. Scope-exit decrements the env refcount; at zero, a per-closure-id deinit decrements rc-bearing captures and `free`s the env.

On TRISC, the env pointer is passed in `r3`. On LLVM, it is passed as the first hidden parameter `i8* %env`.

## Extern declarations

```sysl
extern putchar(ch: int)
extern sbrk(increment: int) -> *i8
extern var errno: int
```

## See also

- [Generics](/reference/generics/)
- [Traits and Operators](/reference/traits-and-operators/)
- [Statements and Control Flow](/reference/statements-and-control-flow/)
- [Builtins and Runtime Semantics](/reference/builtins-and-runtime/)
