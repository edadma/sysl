---
title: Attributes and Tests
description: Attribute syntax, `#test`, `#deprecated`, and the `sysl test` runner.
---

Attributes are annotations prefixed with `#` that attach to the following declaration. They appear on their own line(s) immediately before the declaration:

```sysl
#test
test_copy_basic() -> unit
    0

#inline
#deprecated("use foo2")
foo() -> int = 1
```

## Forms

- Flag: `#name`
- With arguments: `#name(arg1, arg2, ...)` — arguments are literals (string, int, bool), bare identifiers, or `key: value` pairs

Multiple attributes stack on separate preceding lines. Unknown attribute names are stored as-is (no error), so new attributes can be introduced incrementally.

`#if` / `#else` / `#endif` (conditional compilation) use `#` but are not attributes — they work differently.

## `#test` — unit tests

Functions marked `#test` are unit tests. Requirements:

- zero parameters
- returns `unit` (or no return type)
- not generic
- not a method

A test **passes** iff it does not panic. A panic (`panic("msg")`, `abort()`, or any runtime trap) fails the test.

```sysl
#test
test_trivial() -> unit
    assert(1 + 1 == 2, "math is broken")

#test("descriptive name shown in output")
test_with_display_name() -> unit
    0
```

### `should_panic` — tests that must panic

```sysl
#test(should_panic)
test_guard() -> unit
    panic("this must fire")

#test(should_panic: "out of range")
test_bounds() -> unit
    // substring match: panic message must contain "out of range"
    panic("index 42 is out of range")
```

`#test` functions are **excluded from non-test builds** — `sysl compile` and `sysl run` strip them, so they don't contaminate normal execution and aren't emitted to `.asm` / `.tof` / `.ll` output.

## `sysl test` — running tests

```bash
sysl test <path>                      # file or directory (recursive)
sysl test --filter <pattern> <path>   # substring match on test / display name
sysl test --backend interpreter|trisc|all <path>
sysl test --fail-fast <path>
sysl test --verbose <path>
```

Output groups tests by source file with pass / fail markers and timings:

```text
running 6 tests
std/mem/mem.lsysl
  ✓ test_copy_basic              (0.2ms)
  ✗ test_cmp_prefix              (0.1ms)
      panic: expected -1, got 1
  ✓ test_index_byte_found        (0.1ms)
...
5 passed, 1 failed, 0 skipped — 0.6ms
```

Exit code is 0 iff all tests pass. Failing tests print the source file and line of the `#test` attribute (`at file:line`).

### Builtins useful in tests

- `panic(msg: string) -> unit` — halts with the given message. Primary failure signal.
- `assert(cond: bool, msg: string) -> unit` — panics with `msg` if `cond` is false.
- `expect(actual: i64, expected: i64, msg: string) -> unit` — panics with `"msg: expected N, got M"`. Better diagnostics than `assert(a == b, ...)`.

**Test output capture.** Any output from `print`, `println`, `puts`, or `puti` inside a test is captured and displayed below the failure message if the test fails — useful for debugging intermediate values.

## `#pure` — side-effect-free functions

A function marked `#pure` is checked at compile time to have no observable side effects. Pure functions are a discipline-enforcement tool: any violation is an analysis error, never a warning.

```sysl
#pure
square(x: int) -> int = x * x

#pure
fact(n: int) -> int
    if n <= 1 then return 1
    return n * fact(n - 1)

#pure
gcd(a: int, b: int) -> int
    if b == 0 then return a
    return gcd(b, a % b)
```

**Allowed inside `#pure`:**

- Read parameters and module-level `const`s.
- Declare and mutate **local** variables (they cannot escape).
- Call other `#pure` functions, including recursion and mutual recursion.
- Arithmetic, comparison, casts, and all control flow (`if` / `while` / `for` / `match` / `break` / `continue`).
- Call `assert(cond, msg)` — termination is the only effect, consistent with Ada's `pragma Assert` policy.

**Forbidden inside `#pure`:**

- Calling any non-`#pure` user function (cross-function purity does not transit through impure callees).
- Calling IO builtins: `puts`, `print`, `println`, `putchar`, `puti`.
- Calling allocation builtins: `malloc`, `free`, `calloc`, `realloc`, `sbrk`.
- Calling side-effecting traps with observable output: `panic`, `abort`, `expect`.
- Writing to a module-level `var`.
- Writing through a pointer (`*p = v`), an indexed slot (`arr[i] = v`), or a struct field (`p.f = v`) — the caller might see the write.
- Increment / decrement of struct fields (`p.f++`).
- Heap allocation (`new`), `append` to a slice, closure construction.
- Indirect (function-pointer) calls and interface-dispatch calls.
- `asm` blocks.

```sysl
var counter = 0

#pure
bad() -> int
    puts("hi")               // ERROR: IO inside #pure
    counter = counter + 1    // ERROR: writes module-level var
    return counter
```

**Cross-module propagation.** `#pure` is carried through `.smeta` files (wire format: `FUNCP` / `DEFFUNCP` for pure, `FUNC` / `DEFFUNC` for impure; old smeta files without the `P` suffix round-trip as impure — the safe default for untrusted input). A `#pure` function in module A can freely call a `#pure` function in module B. Imported functions without `#pure` are still impure, so annotate library functions you intend to call from pure code.

**Interaction with `--no-contracts`.** `#pure` is a *static* check and always runs. The `--no-contracts` flag only elides *runtime* contract verification — it has no effect on `#pure` enforcement.

**Future work.** Allow `#pure` calls inside `const` initializers and as default-parameter expressions, so `const TABLE = build_table(16)` becomes legal at compile time.

## `#deprecated` — warn on use

Marks a function as deprecated. Calls emit a warning to stderr during analysis (once per callee per compilation):

```sysl
#deprecated("use foo2 instead")
foo() -> int = 1

#deprecated
old_api() -> int = 2
```

Warnings:

```text
warning: 'foo' is deprecated: use foo2 instead
warning: 'old_api' is deprecated
```

Calls still compile and run normally — `#deprecated` only reports usage.

## See also

- [Builtins and Runtime Semantics](/reference/builtins-and-runtime/)
- [Statements and Control Flow](/reference/statements-and-control-flow/)
- [Testing Strategy](/implementation/testing/)
