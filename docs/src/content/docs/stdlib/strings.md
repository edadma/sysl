---
title: std.strings
description: Byte-oriented text operations on string values — searching, trimming, splitting, joining, case conversion.
---

import { Aside } from '@astrojs/starlight/components';

`std.strings` provides byte-oriented operations on `string` values: searching, trimming, case
conversion, replacement, splitting and joining. It is the text-layer counterpart to
[`std.bytes`](/stdlib/overview/#stdbytes), which provides the same API over mutable `[]byte`.

<Aside type="caution" title="ASCII for now">
  All operations treat strings as sequences of bytes, not runes. Case conversion (`to_upper`,
  `to_lower`) and whitespace detection (`trim_space`) handle ASCII only; Unicode-aware
  variants will live in a future `std.unicode` module.
</Aside>

Every function that produces a `string` allocates a fresh buffer — Sysl strings are immutable
and there is no zero-copy substring view today. Operations that don't change the string (like
`contains`) don't allocate.

```sysl
import std.strings.*
```

## Searching

| Function | Signature | Behaviour |
|---|---|---|
| `has_prefix(s, prefix)` | `(string, string) -> bool` | Empty string is a prefix of every string |
| `has_suffix(s, suffix)` | `(string, string) -> bool` | Empty string is a suffix of every string |
| `index(s, sub)` | `(string, string) -> int` | Byte offset of first occurrence, or `-1` |
| `last_index(s, sub)` | `(string, string) -> int` | Byte offset of last occurrence, or `-1` |
| `contains(s, sub)` | `(string, string) -> bool` | Convenience around `index >= 0` |
| `count(s, sub)` | `(string, string) -> int` | Non-overlapping occurrences; empty `sub` returns `len(s) + 1` (Go parity) |
| `index_byte(s, b)` | `(string, byte) -> int` | First byte offset where `s[i] == b` |
| `last_index_byte(s, b)` | `(string, byte) -> int` | Last byte offset where `s[i] == b` |

## Slicing

| Function | Signature | Behaviour |
|---|---|---|
| `substring(s, start, end)` | `(string, int, int) -> string` | Half-open range `[start, end)`, traps on out-of-bounds |

## Trimming

| Function | Signature | Behaviour |
|---|---|---|
| `trim_space(s)` | `(string) -> string` | Strip ASCII whitespace from both ends |
| `trim_left(s, cutset)` | `(string, string) -> string` | Strip any leading byte that appears in `cutset` |
| `trim_right(s, cutset)` | `(string, string) -> string` | Strip any trailing byte that appears in `cutset` |
| `trim(s, cutset)` | `(string, string) -> string` | `trim_left` then `trim_right` |

## Case conversion (ASCII)

| Function | Signature |
|---|---|
| `to_upper(s)` | `(string) -> string` |
| `to_lower(s)` | `(string) -> string` |

## Replacement and repetition

| Function | Signature | Behaviour |
|---|---|---|
| `replace_all(s, old, new)` | `(string, string, string) -> string` | Every non-overlapping occurrence |
| `repeat(s, count)` | `(string, int) -> string` | Traps on negative `count` |

## Splitting and joining

| Function | Signature | Behaviour |
|---|---|---|
| `split(s, sep)` | `(string, string) -> []string` | Splits on every occurrence of `sep` |
| `fields(s)` | `(string) -> []string` | Splits on runs of ASCII whitespace |
| `join(xs, sep)` | `([]string, string) -> string` | Joins slice with `sep` between elements |

## Worked example

```sysl
import std.strings.*

main() -> int
    val line = "  user=alice   role=admin  "
    val kvs  = fields(trim_space(line))

    for kv in kvs
        val eq = index_byte(kv, '=')
        if eq >= 0
            val k = substring(kv, 0, eq)
            val v = substring(kv, eq + 1, len(kv))
            puts(s"$k -> $v")

    0
```

Output:

```
user -> alice
role -> admin
```

## See also

- [`std.builder`](/stdlib/overview/#stdbuilder) — growable string builder used to assemble
  result strings inside the module.
- [`std.strconv`](/stdlib/overview/#stdstrconv) — parse integers, format numbers, quote
  strings.
- [`std.regex`](/stdlib/regex/) — when you need real pattern matching instead of substring
  search.
