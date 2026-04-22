---
title: std.regex
description: POSIX regular expressions via a Pike VM — O(n·m), no catastrophic backtracking.
---

import { Aside } from '@astrojs/starlight/components';

`std.regex` is a POSIX-conformant regular expression engine implemented as a
[Pike VM](https://swtch.com/~rsc/regexp/regexp2.html) — an extension of Thompson's NFA
construction that tracks capture-group positions. Match time is O(n·m) for input length `n`
and pattern size `m`. There is no exponential backtracking.

<Aside type="tip" title="Why a Pike VM?">
  PCRE-style backtracking engines can take exponential time on adversarial inputs. A Pike VM
  is bounded — a regex-of-death DoS is not a concern. The tradeoff is no backreferences and
  no lookaround, both of which would break the complexity guarantee. POSIX doesn't require
  them.
</Aside>

## Features

The engine supports the full POSIX regex grammar:

- **Alternation** — `a|b`
- **Grouping and captures** — `(` and `)`
- **Quantifiers** — `*`, `+`, `?`, `{n}`, `{n,m}`, `{n,}`
- **Character classes** — `[abc]`, `[a-z]`, `[^...]`
- **POSIX named classes** — `[[:alpha:]]`, `[[:digit:]]`, `[[:alnum:]]`, `[[:upper:]]`,
  `[[:lower:]]`, `[[:space:]]`, `[[:blank:]]`, `[[:print:]]`, `[[:graph:]]`, `[[:cntrl:]]`,
  `[[:punct:]]`, `[[:xdigit:]]`
- **Anchors** — `^` and `$`
- **Dot** — `.` matches any character
- **Escaping** — `\` for literal special characters
- **POSIX leftmost-longest** match semantics

A separate BRE parser (`parse_bre`) is available for tools like `expr` and `sed` that require
BRE escaping conventions.

## Pipeline

```
pattern string ──→ [Parser] ──→ AST ──→ [Compiler] ──→ VM bytecode ──→ [VM] ──→ match result
                                │                          │
                          flat array of              flat array of
                          Node structs               Inst structs
```

The AST and VM instructions are stored in flat arrays with integer indices for child
references, sidestepping the (currently missing) language support for recursive types.

## Types

```sysl
struct Regex
    insts: []Inst
    n_groups: int

struct Match
    starts: []int
    ends:   []int
    n_groups: int

Match.group(i: int) -> (int, int) = (self.starts[i], self.ends[i])
Match.matched() -> bool = self.starts[0] >= 0

struct RegexError
    pos: int
    msg: string
```

Group 0 is always the full match. `matched()` returns false if the regex didn't match.

## API

| Function | Signature | Notes |
|---|---|---|
| `parse_regex(pattern)` | `(string) -> Result[(int, []Node, int), RegexError]` | AST root index, node array, capture count |
| `compile(root, nodes, n_groups)` | `(int, []Node, int) -> Regex` | AST to bytecode |
| `exec(insts, classes, input, n_groups)` | `([]Inst, []CharClass, string, int) -> Match` | Run compiled regex |
| `match_regex(pattern, input)` | `(string, string) -> Result[Match, RegexError]` | One-shot — parse, compile, exec |
| `parse_bre(pattern)` | `(string) -> Result[...]` | Basic Regular Expression grammar |
| `match_bre(pattern, input)` | `(string, string) -> Result[Match, RegexError]` | One-shot for BRE |

## Example

```sysl
import std.regex.*

main() -> int
    val pattern = "([a-z]+)@([a-z]+\\.[a-z]+)"
    val input   = "contact: ada@example.com"

    val r = match_regex(pattern, input)

    r match
        Ok(m) ->
            if m.matched()
                val (s0, e0) = m.group(0)
                val (s1, e1) = m.group(1)
                val (s2, e2) = m.group(2)

                puts(s"matched [${s0}..${e0}]: ${substring(input, s0, e0)}")
                puts(s"  user:   ${substring(input, s1, e1)}")
                puts(s"  domain: ${substring(input, s2, e2)}")
            else
                puts("no match")

        Err(e) ->
            puts(s"regex error at ${e.pos}: ${e.msg}")

    0
```

Output:

```
matched [9..25]: ada@example.com
  user:   ada
  domain: example.com
```

## See also

- The [regex source](https://github.com/edadma/trisc/tree/dev/std/regex) is a good
  read — every stage (parser, compiler, VM) is under 300 lines and well-commented.
- [`std.strings`](/stdlib/strings/) for substring-level operations that don't need a regex.
