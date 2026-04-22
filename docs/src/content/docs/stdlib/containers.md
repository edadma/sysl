---
title: Containers, sorting, and slices
description: Heaps, lists, ring buffers, hash tables, and slice algorithms — all generic, all monomorphised.
---

import { Aside } from '@astrojs/starlight/components';

Sysl's standard library keeps data-structure implementations small and concrete. There is no
inheritance hierarchy, no abstract collection trait, no "universal container" type. You pick
the structure that matches your access pattern, and the compiler monomorphises it for your
element type.

## `std.heap` {#stdheap}

A generic binary min-heap.

```sysl
struct MinHeap[T]
    data: []T
    less: (T, T) -> bool

new_min_heap[T](less: (T, T) -> bool) -> MinHeap[T]
new_min_heap_cap[T](less: (T, T) -> bool, cap: int) -> MinHeap[T]

MinHeap[T].push(v: T)             // O(log n) amortised
MinHeap[T].pop() -> Option[T]     // O(log n)
MinHeap[T].peek() -> Option[T]    // O(1)
MinHeap[T].len() -> int           // O(1)
MinHeap[T].empty() -> bool
MinHeap[T].remove_at(i: int) -> Option[T]   // O(log n)
```

```sysl
import std.heap.*

main() -> int
    val cmp = (a: int, b: int) -> a < b
    var h = new_min_heap(cmp)

    for v in [5, 1, 3, 8, 2] do h.push(v)

    while !h.empty()
        h.pop() match
            Some(v) -> print(v)     // 1 2 3 5 8
            None    -> { }
    0
```

## `std.container.list` {#stdcontainerlist}

A doubly-linked list. Use when you need O(1) insertion and removal at either end and in the
middle with an iterator.

```sysl
struct List[T]
    head: *Node[T]
    tail: *Node[T]
    size: int

List[T].push_back(v: T)
List[T].push_front(v: T)
List[T].pop_back() -> Option[T]
List[T].pop_front() -> Option[T]
List[T].len() -> int
```

## `std.container.ring` {#stdcontainerring}

A fixed-capacity ring buffer. Constant-time push and pop at both ends, no allocation after
construction.

```sysl
struct Ring[T]
    data: []T
    head: int
    tail: int
    size: int

new_ring[T](cap: int) -> Ring[T]

Ring[T].push(v: T) -> bool        // false if full
Ring[T].pop() -> Option[T]
Ring[T].len() -> int
Ring[T].cap() -> int
Ring[T].full() -> bool
```

## `std.hash` {#stdhash}

A `Hashable` trait and hash-table primitives for building up generic hash maps and sets.

```sysl
trait Hashable[T]
    hash(v: T) -> u64
    equal(a: T, b: T) -> bool
```

The [FNV-1a](https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function)
default implementations cover `int`, `string`, and byte slices.

## `std.slices` {#stdslices}

Generic slice algorithms.

| Function | Signature |
|---|---|
| `contains[T: Eq]` | `([]T, T) -> bool` |
| `index_of[T: Eq]` | `([]T, T) -> int` |
| `reverse[T]` | `([]T)` (in place) |
| `equal[T: Eq]` | `([]T, []T) -> bool` |
| `copy_slice[T]` | `([]T, []T) -> int` (returns bytes copied) |

## `std.sort` {#stdsort}

```sysl
sort[T](xs: []T, less: (T, T) -> bool)             // in-place, O(n log n)
sort_by_key[T, K: Ord](xs: []T, key: (T) -> K)
is_sorted[T](xs: []T, less: (T, T) -> bool) -> bool
binary_search[T](xs: []T, target: T, cmp: (T, T) -> int) -> int
```

Default sort is an in-place introsort — quicksort with a heapsort fallback and insertion-sort
for small runs. Stable when you need it: pass an index-based `less` that breaks ties on
position.

## See also

- [`std.cmp`](/stdlib/overview/#stdcmp) — generic `min`, `max`, `clamp`, and the `Ord`
  trait that most algorithms here accept.
- [`std.heap`'s literate source](https://github.com/edadma/trisc/blob/dev/std/heap/heap.lsysl)
  — a readable 300-line sample of the library style.
