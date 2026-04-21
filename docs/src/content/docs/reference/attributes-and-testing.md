---
title: Attributes and Tests
description: Attribute syntax, `#test`, `#deprecated`, and the `sysl test` runner.
---

Attributes are annotations prefixed with `#` that attach to the following declaration. They appear on their own line(s) immediately before the declaration:

```
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

```
#test
test_trivial() -> unit
    assert(1 + 1 == 2, "math is broken")

#test("descriptive name shown in output")
test_with_display_name() -> unit
    0
```

### `should_panic` — tests that must panic

```
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

```
sysl test <path>                      # file or directory (recursive)
sysl test --filter <pattern> <path>   # substring match on test / display name
sysl test --backend interpreter|trisc|all <path>
sysl test --fail-fast <path>
sysl test --verbose <path>
```

Output groups tests by source file with pass / fail markers and timings:

```
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

## `#deprecated` — warn on use

Marks a function as deprecated. Calls emit a warning to stderr during analysis (once per callee per compilation):

```
#deprecated("use foo2 instead")
foo() -> int = 1

#deprecated
old_api() -> int = 2
```

Warnings:

```
warning: 'foo' is deprecated: use foo2 instead
warning: 'old_api' is deprecated
```

Calls still compile and run normally — `#deprecated` only reports usage.

## See also

- [Builtins and Runtime Semantics](/reference/builtins-and-runtime/)
- [Statements and Control Flow](/reference/statements-and-control-flow/)
- [Testing Strategy](/implementation/testing/)
