---
title: Modules and Imports
description: Module declarations, imports, visibility, and name mangling.
---

Sysl organises code into modules. Each source file declares one module; other files import from it by path.

## Module declarations

```sysl
module oskit.kernel
```

The declared module path must match the file's directory path. The driver validates this at compile time. Files without a `module` declaration are treated as anonymous and participate in no name mangling.

## Imports

```sysl
import posix.stdlib.{malloc, free}      // named imports
import posix.string.*                    // wildcard import
import posix.io.{open => fopen}         // aliased import
```

Imports are resolved through module metadata (`.smeta`) produced by dependent modules and merged into analysis scope.

## Visibility

Declarations are public by default. Prefix a declaration with `private` to keep it file-local:

```sysl
private myHelper() -> int = 42        // not exported
myPublicFunc() -> int = 0             // public
```

## Name mangling

Functions and global variables defined in a module are mangled with the module path to prevent cross-module collisions:

```
module std.strings  → trim_space  → std_strings__trim_space
module mylib        → helper      → mylib__helper
```

Module parts are joined by `_`, separated from the symbol name by `__`. Source code always uses the short name; the compiler resolves it to the mangled form.

**Never mangled:**

- `main`
- `extern` functions
- builtin functions
- functions in files without a `module` declaration
- any function whose name matches an `extern` declaration in the current compilation

## See also

- [Language Reference Scope](/reference/scope/)
- [Type System](/reference/types/)
- [Attributes and Tests](/reference/attributes-and-testing/)
- [Compiler Pipeline](/implementation/pipeline/)
