---
title: std.io and std.bufio
description: Reader / Writer interfaces, in-memory adapters, and buffered wrappers.
---

import { Aside } from '@astrojs/starlight/components';

`std.io` defines the small set of I/O interfaces that everything else builds on — inspired
directly by Go's [`io`](https://pkg.go.dev/io) package. `std.bufio` layers buffering on top.

## Interfaces

```sysl
interface Reader
    read(buf: []byte) -> int            // 0 at EOF

interface Writer
    write(data: []byte) -> int

interface Closer
    close()

interface ReadWriter    { Reader; Writer }
interface ReadCloser    { Reader; Closer }
interface WriteCloser   { Writer; Closer }
```

These are pure interfaces — any struct that implements the right methods satisfies them.
There's no runtime dispatch penalty; the analyser checks the shape at the call site and emits
a direct call.

## In-memory adapters

### ByteReader

```sysl
struct ByteReader
    data: []byte
    pos:  int

ByteReader.read(buf: []byte) -> int     // advances pos, returns 0 at EOF
```

### ByteWriter

```sysl
struct ByteWriter
    buf: []byte

new_writer() -> ByteWriter              // empty, growable

ByteWriter.write(data: []byte) -> int   // append, return len(data)
ByteWriter.bytes() -> []byte
ByteWriter.to_string() -> string
ByteWriter.len() -> int
ByteWriter.reset()
```

Both types satisfy `Reader` / `Writer` and are the default way to wire up tests that exercise
streaming code without touching the filesystem.

## Utilities

| Function | Signature | Behaviour |
|---|---|---|
| `read_all(r)` | `(Reader) -> []byte` | Reads to EOF, returns everything |
| `copy(dst, src)` | `(Writer, Reader) -> int` | Streams `src` into `dst` until EOF, returns total bytes |

## `std.bufio` {#stdbufio}

`std.bufio` wraps a `Reader` or `Writer` with a buffer to amortise system-call cost and add
line-oriented helpers.

```sysl
struct BufReader
    source: Reader
    buf:    []byte
    r, w:   int

new_bufreader(r: Reader) -> BufReader
new_bufreader_size(r: Reader, size: int) -> BufReader

BufReader.read(buf: []byte) -> int
BufReader.read_line() -> Option[string]
BufReader.read_byte() -> Option[byte]
```

```sysl
struct BufWriter
    sink: Writer
    buf:  []byte
    n:    int

new_bufwriter(w: Writer) -> BufWriter
new_bufwriter_size(w: Writer, size: int) -> BufWriter

BufWriter.write(data: []byte) -> int
BufWriter.write_string(s: string) -> int
BufWriter.write_byte(b: byte)
BufWriter.flush() -> int
```

`flush()` on a `BufWriter` is mandatory before the writer is dropped — the compiler can't
enforce this, so wire it into a `defer` or the writer's `close`.

## Example: line-oriented processing

```sysl
import std.io.*
import std.bufio.*

count_non_empty(r: Reader) -> int
    var br = new_bufreader(r)
    var count = 0

    var done = false
    while !done
        br.read_line() match
            Some(line) ->
                if len(line) > 0 then count += 1
            None -> done = true

    count
```

## See also

- [`std.fs`](/stdlib/overview/#stdfs) — the POSIX-backed filesystem layer implements
  `Reader`, `Writer`, and `Closer`.
- [`std.encoding.*`](/stdlib/overview/#stdencodinghex) — codecs that take `Reader`s and
  `Writer`s.
