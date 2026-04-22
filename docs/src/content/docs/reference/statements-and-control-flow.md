---
title: Statements and Control Flow
description: Variable declarations, control flow, match, loops, destructuring, return.
---

Statements are separated by newlines. Semicolons (`;`) place multiple statements on a single line:

```sysl
a[0] = 1; a[1] = 2; a[2] = 3
val x = 10; val y = 20
```

## Variables

```sysl
// Immutable
val x = 42
val y: int = 42

// Mutable
var x = 42
var y: int = 42
x = 100                  // reassignment

// Uninitialized (zero-initialised)
var x: int
var arr: [10]int
var p: *Node

// Inferred type (mutable by default in blocks)
x = 42                   // inferred as int
name = "hello"           // inferred as string

// Volatile — prevents load/store optimisation
volatile var status: u32 = 0
```

### Discard binding (`_`)

`_` is a write-only binding (Go / Rust style). You can bind to it; you cannot reference it; multiple `_` bindings in the same scope don't collide:

```sysl
val _ = foo()             // evaluate for side effects, discard
val _ = bar()             // fine — no collision
val _: int = 7            // type annotations allowed
var _ = baz()

// In destructuring patterns
_, y = pair()             // discard first, bind second
val x, _ = pair()
val a, _, c = triple()
_, _ = pair()             // discard all
```

### Global variables

```sysl
var count = 0             // module-level mutable
val MAX = 100             // module-level immutable
```

### Compile-time constants (`const`)

```sysl
const BASE = 0x1000
const STATUS = BASE + 4           // folded to 0x1004
const DATA = BASE + 8             // folded to 0x1008
const MASK = 0xFF & (1 << 4)      // folded to 0x10

type Age = int within 0..MAX_AGE  // const usable in `within` bounds
```

`const` declares a compile-time integer constant. The initializer must be foldable; the declaration emits no storage. Supported in initializers: `+`, `-`, `*`, `/`, `%`, `<<`, `>>`, `&`, `|`, `^`, unary `-` / `~`, numeric / char / bool literals, and references to other `const` names.

An initializer that cannot be folded is a compile error. Values are truncated to the target type's width. `const` is valid at both module and function scope. Currently only integer types are supported — `const PI: f64 = 3.14` is not yet accepted.

`val` is also folded when the initializer is constant, but — unlike `const` — it allocates storage and accepts non-const initializers. Prefer `const` when you want the zero-storage guarantee.

## Assignment forms

- direct assignment: `x = expr`
- destructuring / parallel assignment: `a, b = b, a`
- compound assignment: `x += 1`
- lvalue assignment through index, field, and dereference paths

## Control flow

```sysl
// if / elif / else
if x > 0
    positive()
elif x == 0
    zero()
else
    negative()

// if-then (inline)
if x > 0 then positive()

// match (value matching, no fallthrough)
x match
    1 -> doA()
    2, 3 -> doB()              // multiple values per arm
    _ -> doDefault()           // wildcard
    else -> doDefault()        // alternative

// match as expression
y = x match
    1 -> "one"
    2, 3 -> "few"
    else -> "many"

// match with guards
x match
    _ if x > 10 -> "big"
    _ if x > 0 -> "positive"
    else -> "non-positive"

// range matching (inclusive)
x match
    1..10 -> "small"
    11..100 -> "medium"
    else -> "large"

// struct destructuring
p match
    Point(x, y) -> x + y
    Point(_, y) -> y
    Point(x, y) if x == 0 -> y

// tagged union (data enum) matching — must be exhaustive
s match
    Circle(r) -> r * r * 3
    Rect(w, h) -> w * h
    Empty -> 0
    Circle(r) if r > 5 -> 1

// match with block bodies
x match
    1 ->
        a = compute()
        doSomething(a)
    else -> fallback()
```

A `match` on a data-enum value must cover every variant, or include a wildcard `_ -> ...` or `else -> ...` default. Missing variants produce a compile error listing them. Guarded arms do not count toward exhaustiveness. Non-enum matches (on integers, strings, etc.) do not require exhaustiveness.

## Loops

```sysl
// while
while cond
    body

// while-do (inline)
while i < 10 do i++

// do-while
do
    body
while cond

// C-style for
for i = 0; i < 10; i++
    body

// for-do (inline)
for i = 0; i < 10; i++ do sum += i

// for-in range (inclusive — includes upper bound)
for i in 1..5                  // 1, 2, 3, 4, 5

// for-in range (exclusive)
for i in 0..<5                 // 0, 1, 2, 3, 4

// for-in counting down
for i in 10 downTo 0           // 10, 9, ..., 0

// for-in with step
for i in 0..100 step 5          // 0, 5, 10, ..., 100
for i in 0..<30 step 3          // 0, 3, 6, ..., 27
for i in 20 downTo 0 step 4     // 20, 16, 12, 8, 4, 0

// Iterate values over arrays / slices / strings
for v in arr
    body                        // v = each element

// Iterate with index and value
for i, v in arr
    body                        // i = index, v = arr[i]

// Iterate backward — over a range, a T::Range, or a collection
for i in reverse 0..<5          // 4, 3, 2, 1, 0
for d in reverse Day::Range     // Sun, Sat, ..., Mon
for v in reverse arr            // index len-1 down to 0

// `in` as range membership operator
x in 1..4                       // true if 1 <= x <= 4
x in 1..<4                      // true if 1 <= x < 4
x !in 1..4                      // negated membership

// break and continue
while true
    if done then break
    if skip then continue
    process()
```

`reverse` is a contextual keyword — it acts as one only directly after `in` in a for-loop. Existing identifiers named `reverse` (e.g. `std.slices.reverse`) are unaffected.

### Named loops and labeled break / continue

A loop (`for`, `while`, or `do`) can carry a name via a leading `label:` prefix. `break label` and `continue label` then target the named enclosing loop rather than the innermost one:

```sysl
outer: for i in 0..<rows
    for j in 0..<cols
        if grid[i][j] == target then break outer        // exits both loops
        if grid[i][j] == 0 then continue outer          // next iteration of outer
        use(grid[i][j])
```

Rules:

- A label is an identifier followed by `:` immediately before `for`, `while`, or `do`.
- Unlabeled `break` / `continue` always target the innermost enclosing loop, regardless of whether it has a label.
- The same label cannot be used on a nested loop (would make `break label` ambiguous). Sibling (non-nested) reuse is fine.
- Labels live in their own namespace — they do not collide with local variable names.
- An unknown label, or a `break` / `continue` outside any loop, is a compile error.

### Loop `variant` — termination witness

`variant <expr>` at the top of a loop body asserts that `expr` strictly decreases between iterations and stays `>= 0`. The first iteration has no prior value to compare against and is exempt; every subsequent one traps if either condition is violated:

```sysl
var remaining = 100
while remaining > 0
    variant remaining               // monotonic-decrease witness
    remaining = remaining - step()  // if step() ever returns <= 0, this traps
```

It works on `for`, `while`, and `do-while`. The clause must appear at the top level of the loop body — uses elsewhere (inside an `if`, after a regular statement, etc.) are rejected.

Internally, the analyzer hoists an init flag and a previous-value slot out of the loop. `--no-contracts` elides the entire hoisted check state.

`variant` is purely about termination; it does not enforce any other invariant. Pair it with `invariant` clauses for full state safety.

```sysl
// for-loop with both
for i in 0..<n
    invariant i >= 0
    variant n - i
    process(i)
```

## Destructuring and parallel assignment

Tuples can be destructured with or without parentheses:

```sysl
// Declaration (new variables)
q, r = divmod(17, 5)           // Go-style, creates q and r as var
(q, r) = divmod(17, 5)          // parenthesized form
val q, r = divmod(17, 5)        // immutable
var q, r = divmod(17, 5)        // explicit mutable

// Parallel assignment (existing variables)
a = 10
b = 20
a, b = b, a                     // swap: RHS fully evaluated first

// Works on named structs too
p = Point(10, 20)
x, y = p                        // x = p.x, y = p.y (field order)

// And ref structs
r = new Point(3, 4)
a, b = r

// Mixed declared / undeclared is an error
a = 10
a, b = 20, 30                   // ERROR: a exists but b doesn't
```

Rules for `a, b = ...` without `val` / `var`:

- All names new → declaration as `var`.
- All names exist as `var` → parallel assignment.
- Mixed → error.

## Return

```sysl
return                  // unit return
return expr             // return single value
return a, b             // return tuple (no parens needed)
```

The last expression in a block is an implicit return.

## `invariant` — assertion with loop-body focus

`invariant <bool>` is an assertion that traps if the condition is false. It is primarily intended at the top of loop bodies (and re-checks on each iteration), but also works as a general in-function assertion. It reuses the same trap path as `require` / `ensure`.

```sysl
main() -> int
    var sum = 0
    var i = 0
    while i < 5
        invariant i >= 0
        invariant sum >= 0
        sum = sum + i
        i++
    sum
```

As a general assertion:

```sysl
main() -> int
    var x = 42
    invariant x > 0
    x
```

## `static_assert` — compile-time assertion

`static_assert(cond [, "msg"])` at module scope fails the compile if `cond` is false. The condition must be compile-time evaluable — `const` references, `sizeof`, arithmetic, and comparison operators are supported.

```sysl
static_assert(sizeof(int) == 4)
static_assert(sizeof(i64) == 8, "i64 must be 8 bytes")

struct Header
    magic: u32
    version: u32
    flags: u64

static_assert(sizeof(Header) == 16, "Header layout fixed by protocol")

const PAGE_SIZE = 4096
static_assert(PAGE_SIZE % 4096 == 0)
```

A non-foldable condition (e.g. a function call) is itself a compile error: `not compile-time evaluable`. On failure, the optional message is reported; without a message, the error reads `static_assert failed`.

## Inline assembly

```sysl
asm("halt")
asm("trap 0")
```

## See also

- [Expressions and Operators](/reference/expressions/)
- [Functions](/reference/functions/)
- [Arrays, Slices, and Pointers](/reference/arrays-slices-pointers/)
- [Attributes and Tests](/reference/attributes-and-testing/)
