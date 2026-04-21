---
title: Strings
description: String model, interpolation, format strings, and byte-buffer construction.
---

Strings are fat pointers: `{ptr: *u8, len: i64}` with a ref-counted heap buffer. `string` is a first-class built-in type.

## String operations

```sysl
s = "hello"
len(s)                           // 5
s[0]                             // 104 ('h' as byte value)
t = s + " world"                 // concatenation → new string
s == t                           // structural equality
s != t                           // structural inequality

// String decays to *u8 / *i8
puts(s: *byte)                   // can pass string directly
```

## Interpolation (`s"..."`)

Prefix a string literal with `s` to enable interpolation. Use `$name` for variables and `${expr}` for expressions:

```sysl
x = 42
s = s"value is $x"               // "value is 42"
puts(s"${x + 1}")                // prints "43"
name = "world"
puts(s"hello $name")             // prints "hello world"
puts(s"cost is $$5")             // prints "cost is $5" ($$ = literal $)
```

Plain strings (`"..."`) are never interpolated — `$` is a regular character.

Non-string expressions are automatically converted via `str()`. Integer, boolean, and float (`f32` / `f64`) types are supported.

## Format strings (`f"..."`)

Prefix a string with `f` for printf-style format specifiers. Each interpolation can be followed by `%` and a format spec:

```sysl
val n = 255
puts(f"hex: $n%x")               // "hex: ff"
puts(f"HEX: $n%X")               // "HEX: FF"
puts(f"padded: $n%08x")          // "padded: 000000ff"
puts(f"decimal: $n%d")           // "decimal: 255"
puts(f"binary: ${26}%b")         // "binary: 11010"
puts(f"octal: ${511}%o")         // "octal: 777"
```

### Format specifiers

| Spec | Meaning |
|------|---------|
| `%d` | Decimal integer |
| `%x` | Hexadecimal (lowercase) |
| `%X` | Hexadecimal (uppercase) |
| `%b` | Binary |
| `%o` | Octal |
| `%s` | String (default if no spec) |
| `%+d` | Decimal with explicit sign |
| `%%` | Literal `%` |

### Width and padding

```sysl
val n = 42
puts(f"$n%08d")                   // "00000042" (zero-padded, width 8)
puts(f"0x${10}%04x")              // "0x000a"  (literal needs braces)
val s = "hi"
puts(f"[$s%10s]")                 // "[        hi]"  (right-aligned, width 10)
puts(f"[$s%-10s]")                // "[hi        ]"  (left-aligned, width 10)
```

Without a format spec, `f"..."` works like `s"..."` — values are converted via `str()`.

## `str()` builtin

Converts a value to its string representation:

```sysl
str(42)                          // "42"
str(-5)                          // "-5"
str(0)                           // "0"
str("hello")                     // "hello" (identity for strings)
str(3.14)                        // "3.140000" (codegen: fixed 6-digit)
str(Circle(5))                   // "Circle" (variant name of a data-enum value)
```

Float formatting uses fixed 6-digit fractional precision in TRISC codegen (`3.14 → "3.140000"`). The interpreter uses the host's default float formatting (`3.14 → "3.14"`).

`str()` on a data-enum (tagged union) value returns the variant name, regardless of field contents. Each enum type gets one synthesized `__str_<EnumName>` helper the first time it is referenced. Simple integer enums and struct values are not yet supported — format fields manually.

## Construction from bytes

```sysl
// From pointer + length (copies the bytes)
var buf: [5]byte
buf[0] = 'h'
buf[1] = 'e'
buf[2] = 'l'
buf[3] = 'l'
buf[4] = 'o'
s = string(&buf[0], 5)           // s = "hello"

// From byte slice (copies the bytes)
data = new [10]byte
// ... fill data ...
s = string(data[:5])             // string from []byte slice
```

## Escape sequences

```
\n    newline
\t    tab
\r    carriage return
\0    null
\\    backslash
\'    single quote
\"    double quote
\xNN  hex byte (e.g., \x1b for ESC)
```

## See also

- [Expressions and Operators](/reference/expressions/)
- [Builtins and Runtime Semantics](/reference/builtins-and-runtime/)
- [Arrays, Slices, and Pointers](/reference/arrays-slices-pointers/)
