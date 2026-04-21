---
title: Arrays, Slices, and Pointers
description: Fixed arrays, dynamic arrays, slicing, append, pointer arithmetic, and decay.
---

Sysl distinguishes fixed-size arrays (`[n]T`, value type), slices (`[]T`, fat pointer), heap arrays (`&[]T`, ref-counted), and raw pointers (`*T`). Each has a distinct role — no hidden indirection.

## Fixed arrays

```sysl
var arr: [5]int                  // zero-initialised
arr[0] = 42
arr: [3]int = [10, 20, 30]       // array literal

// Byte arrays from string and char literals
var buf: [5]byte = "hello"           // copies string bytes into array
var msg: [3]byte = ['H', 'i', '!']   // char literals coerce to bytes

// Array decays to pointer when passed to *T parameter
sum(arr: *int, n: int) -> int = ...
sum(myArr, 5)                    // myArr decays to *int
```

## Chained indexing

Indexing is a repeatable postfix operator — `arr[i][j]` works on arrays of arrays, slices of slices, etc.:

```sysl
val grid = new [3][]int          // array of int slices
grid[0] = row0[:]
val v = grid[1][2]               // chain: grid[1] returns []int, then [2] indexes it

// Address-of with chained index
val p = &stacks[slot][0]         // address of first element of stacks[slot]
```

## Dynamic arrays (heap)

```sysl
a = new [5]int                   // type: &[]int, ref-counted
a[0] = 42
len(a)                           // 5 (from heap header)
cap(a)                           // 5
// automatically freed when refcount reaches 0
```

## Slices

Slices are fat pointers `{ptr, len, cap}` (16 bytes). They share the backing storage of the array they were taken from.

```sysl
a = new [5]int
s = a[1:4]                       // type: []int, shares backing array
s = a[:3]                        // = a[0:3]
s = a[2:]                        // = a[2:len]
s = a[:]                         // = a[0:len]
len(s)                           // hi - lo
cap(s)                           // original_cap - lo
```

## Append

```sysl
s = a[:0]                        // empty slice with capacity
s = append(s, 42)                // returns new slice value
s = append(s, 99)                // Go semantics: may grow if len == cap
```

## Pointers

```sysl
x = 42
p = &x                           // p: *int
*p = 100                         // dereference and assign
val y = *p                       // dereference and read

// Expression lvalues (C-style)
(*p).field = 10                  // deref pointer, assign field
(*p)[i] = 42                     // deref pointer, index, assign
(arr + 2)[0] = 99                // pointer arithmetic, index, assign

// Pointer arithmetic (scaled by element size)
p = &arr[0]
val second = *(p + 1)            // pointer + offset
p++                              // advance by one element
p += 3                           // advance by 3 elements
p--                              // retreat by one element
p -= 2                           // retreat by 2 elements

// Array + offset decays to pointer
q = arr + 2                      // q: *int (not [n]int)
```

## Pointer / ref / value conversions

- `value → ptr`: `&v`
- `ptr → value`: `*p` (explicit; see below)
- `ref → ptr`: `&r` (unsafe — no refcount change)
- `ptr → ref`: **always an error** (cannot manufacture a refcount)

### Pointer dereference is explicit

Passing `*T` to a function parameter of type `T` is a **type error**. Implicit deref-and-copy was removed because it hides cost: a pointer-passing site that *looks* like pass-by-reference silently becomes a `memcpy` of the entire pointee. For a small struct that's free; for a 4 KB packet it isn't. Write the deref:

```sysl
struct Point
    x: int
    y: int

sum(p: Point) -> int = p.x + p.y

main() -> int
    var p = Point(20, 22)
    val ptr: *Point = &p
    sum(*ptr)                    // explicit: sum receives a copy of *ptr
```

The reverse direction (`T → *T`) is also not implicit — it would create a dangling pointer to a temporary.

### Exception: `self` in methods

Inside a method body `self` has type `*StructName`. Passing `self` to a function that expects the value type auto-derefs, because the method-call sugar already hides the pointer:

```sysl
Point.total() -> int = sum(self)   // self is *Point; sum gets a copy of *self
```

This is the **only** implicit `*T → T` allowed. Local variables of pointer type, function parameters, struct fields — all require explicit `*ptr`.

## Array / pointer decay rules

- `[n]T` → `*T` (array decays to pointer)
- `string` → `*u8` or `*i8`
- `&T` → `*U` (ref decays to raw pointer)
- Any `*T` → any `*U` (permissive pointer casting)

## See also

- [Type System](/reference/types/)
- [Builtins and Runtime Semantics](/reference/builtins-and-runtime/)
- [Strings](/reference/strings/)
- [Functions](/reference/functions/)
