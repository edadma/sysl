---
title: Standard library
description: The Sysl standard library — written in literate Sysl, read it like a book.
---

import { CardGrid, LinkCard, Card, Aside } from '@astrojs/starlight/components';

The Sysl standard library lives at [`trisc/std/`](https://github.com/edadma/trisc/tree/dev/std).
Every module is a [`.lsysl`](/reference/literate-sysl/) literate file — Markdown prose
interleaved with indented Sysl code blocks. The prose is real documentation; the code
compiles identically to a `.sysl` file.

If you have the repo checked out, the files themselves are the reference. The pages here are
a guide to what exists and what each module is for.

<Aside type="note" title="Status">
  The standard library is a growing, opinionated set of modules — not frozen. Expect the
  surface to expand as Sysl matures. Unstable APIs are noted on individual module pages.
</Aside>

## Text and strings

<CardGrid>
  <LinkCard title="std.strings" href="/stdlib/strings/" description="Byte-oriented operations on string values — prefix / suffix, trim, case, replace, split, join." />
  <LinkCard title="std.bytes" href="/stdlib/overview/#stdbytes" description="The byte-slice counterpart to std.strings. Mutable buffer operations." />
  <LinkCard title="std.strconv" href="/stdlib/overview/#stdstrconv" description="Parse integers, format numbers, quote / unquote string literals." />
  <LinkCard title="std.regex" href="/stdlib/regex/" description="POSIX regex via a Pike VM. O(n·m), no catastrophic backtracking." />
  <LinkCard title="std.text" href="/stdlib/overview/#stdtext" description="Higher-level text utilities — wrapping, tabular formatting." />
  <LinkCard title="std.unicode" href="/stdlib/overview/#stdunicode" description="Unicode codepoint classification — work in progress." />
  <LinkCard title="std.utf8" href="/stdlib/overview/#stdutf8" description="UTF-8 encode / decode for strings and byte slices." />
</CardGrid>

## Data structures

<CardGrid>
  <LinkCard title="std.heap" href="/stdlib/containers/#stdheap" description="Generic binary min-heap. O(log n) push/pop, O(1) peek." />
  <LinkCard title="std.container.list" href="/stdlib/containers/#stdcontainerlist" description="Doubly-linked list." />
  <LinkCard title="std.container.ring" href="/stdlib/containers/#stdcontainerring" description="Fixed-capacity ring buffer." />
  <LinkCard title="std.hash" href="/stdlib/containers/#stdhash" description="Hashable trait and hash-table primitives." />
  <LinkCard title="std.slices" href="/stdlib/containers/#stdslices" description="Generic slice algorithms — find, contains, reverse, equal." />
  <LinkCard title="std.sort" href="/stdlib/containers/#stdsort" description="Stable sort, in-place sort, sort by key, binary search." />
</CardGrid>

## Numerics

<CardGrid>
  <LinkCard title="std.math" href="/stdlib/overview/#stdmath" description="Constants, sin / cos / tan, exp / log, abs, pow, sqrt." />
  <LinkCard title="std.math.bits" href="/stdlib/overview/#stdmathbits" description="Bit-twiddling — leading/trailing zeros, popcount, rotate, reverse." />
  <LinkCard title="std.math.float" href="/stdlib/overview/#stdmathfloat" description="IEEE-754 inspection — sign, exponent, is_nan, is_inf." />
  <LinkCard title="std.rand" href="/stdlib/overview/#stdrand" description="Deterministic PRNG with seedable state." />
</CardGrid>

## I/O and formats

<CardGrid>
  <LinkCard title="std.io" href="/stdlib/io/" description="Reader / Writer / Closer interfaces. ByteReader and ByteWriter for testing." />
  <LinkCard title="std.bufio" href="/stdlib/io/#stdbufio" description="Buffered reader and writer wrappers around std.io." />
  <LinkCard title="std.fs" href="/stdlib/overview/#stdfs" description="Filesystem access — open, read, write, stat (POSIX-backed)." />
  <LinkCard title="std.path" href="/stdlib/overview/#stdpath" description="Filesystem path parsing — join, clean, dir, base, ext." />
  <LinkCard title="std.encoding.hex" href="/stdlib/overview/#stdencodinghex" description="Hex encoding and decoding." />
  <LinkCard title="std.encoding.base64" href="/stdlib/overview/#stdencodingbase64" description="Standard and URL-safe base64." />
  <LinkCard title="std.encoding.binary" href="/stdlib/overview/#stdencodingbinary" description="Read and write fixed-width integers in big- or little-endian byte order." />
  <LinkCard title="std.encoding.csv" href="/stdlib/overview/#stdencodingcsv" description="Quoting-aware CSV reader and writer." />
</CardGrid>

## Cryptography

<CardGrid>
  <LinkCard title="std.crypto.sha256" href="/stdlib/crypto/#sha256" description="FIPS-180-4 SHA-256 — streaming and one-shot." />
  <LinkCard title="std.crypto.hmac" href="/stdlib/crypto/#hmac" description="HMAC with any hash (currently SHA-256)." />
  <LinkCard title="std.crypto.pbkdf2" href="/stdlib/crypto/#pbkdf2" description="RFC 2898 PBKDF2 password-key derivation." />
  <LinkCard title="std.crypto.chacha20" href="/stdlib/crypto/#chacha20" description="RFC 8439 ChaCha20 stream cipher." />
  <LinkCard title="std.crypto.aead" href="/stdlib/crypto/#aead" description="ChaCha20-Poly1305 authenticated encryption." />
</CardGrid>

## System and utilities

<CardGrid>
  <LinkCard title="std.alloc" href="/stdlib/overview/#stdalloc" description="Heap allocator used by the kernel and by any no-POSIX target." />
  <LinkCard title="std.mem" href="/stdlib/overview/#stdmem" description="Low-level memory operations — copy, set, cmp. The target of most backend intrinsics." />
  <LinkCard title="std.cmp" href="/stdlib/overview/#stdcmp" description="Ordering trait, generic min / max / clamp." />
  <LinkCard title="std.option" href="/stdlib/overview/#stdoption" description="The Option[T] enum and helpers." />
  <LinkCard title="std.result" href="/stdlib/overview/#stdresult" description="The Result[T, E] enum and helpers, ?-operator integration." />
  <LinkCard title="std.errors" href="/stdlib/overview/#stderrors" description="Error-wrapping conventions used across the library." />
  <LinkCard title="std.debug" href="/stdlib/overview/#stddebug" description="assert, panic, and related debug primitives." />
  <LinkCard title="std.log" href="/stdlib/overview/#stdlog" description="Level-tagged logging to any std.io.Writer." />
  <LinkCard title="std.time" href="/stdlib/overview/#stdtime" description="Monotonic and wall-clock time, durations, simple formatting." />
  <LinkCard title="std.flag" href="/stdlib/overview/#stdflag" description="Command-line flag parsing." />
  <LinkCard title="std.process" href="/stdlib/overview/#stdprocess" description="Process environment — args, env, exit." />
  <LinkCard title="std.term" href="/stdlib/overview/#stdterm" description="Terminal primitives — ANSI colours, raw mode." />
  <LinkCard title="std.builder" href="/stdlib/overview/#stdbuilder" description="Growable string builder. Used internally by most text-producing APIs." />
  <LinkCard title="std.testing" href="/stdlib/overview/#stdtesting" description="Test helpers beyond the #test attribute — table-driven tests, fixtures." />
</CardGrid>

## Reading the library

Every standard library module is a literate file. Open it in your editor and you get the
implementation; render it with `sysl doc` and you get a documentation page. The prose is
the spec.

```bash
# Render one file to HTML.
sysl doc std/regex/regex.lsysl

# Render an entire module directory.
sysl doc std/crypto/
```

That's exactly how this site is rendered, too — the documentation you're reading is the
[`docs/`](https://github.com/edadma/trisc/tree/dev/sysl/docs) sibling to the library source.

## What's coming next

The next few modules on the roadmap: `std.tls` (TLS 1.3 record layer), `std.net` (socket
primitives on top of SLIX's `tcp`/`udp` servers), and `std.json` (a streaming JSON
parser/generator).
