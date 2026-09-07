<p align="center">
  <img src="https://sysl.sh/sysl-wordmark.svg" alt="sysl" width="360">
</p>

<p align="center">
  <a href="https://central.sonatype.com/artifact/sh.sysl/sysl_3"><img alt="Maven Central" src="https://img.shields.io/maven-central/v/sh.sysl/sysl_3"></a>
  <a href="https://github.com/sysl-lang/homebrew-tap"><img alt="Homebrew" src="https://img.shields.io/badge/dynamic/regex?url=https%3A%2F%2Fraw.githubusercontent.com%2Fsysl-lang%2Fhomebrew-tap%2Fmain%2FFormula%2Fsysl.rb&search=version%20%22(%5B%5E%22%5D%2B)%22&replace=%241&label=homebrew&color=fbb040"></a>
  <a href="https://github.com/sysl-lang/sysl/commits"><img alt="Last Commit" src="https://img.shields.io/github/last-commit/sysl-lang/sysl"></a>
  <img alt="License" src="https://img.shields.io/github/license/sysl-lang/sysl">
  <img alt="Scala Version" src="https://img.shields.io/badge/Scala-3.8.4-blue.svg">
  <img alt="Scala.js Version" src="https://img.shields.io/badge/Scala.js-1.22.0-blue.svg">
  <img alt="Scala Native Version" src="https://img.shields.io/badge/Scala_Native-0.5.12-blue.svg">
</p>

A modern, ref-counted, general-purpose systems language.

> **Status: specified in writing, and it runs.** This repository is a clean reimplementation of the
> sysl language — not a port of the earlier prototype, which survives only as a source of lessons.
> Every rule is written down before it is implemented, one feature at a time, and the compiler builds
> programs to native binaries through LLVM today: see the [tour](https://sysl.sh/tour/) to learn the
> language, and [`library/`](library/) for the standard library it ships with.

## What sysl is

sysl is a general-purpose systems language that aims to keep the control a systems language is used
for while being meaningfully easier to learn and work with than other languages. It is ref-counted
rather than borrow-checked: memory is managed through explicit modes written on the type — `T`
(value/stack), `&T` (ARC reference-counted heap), and `*T` (raw pointer) — with no garbage
collector, plus `ref`, a local binding that gives a second name to storage something else already
owns.

## Documentation

- **[The tour](https://sysl.sh/tour/)** — the language and its standard library in one
  pass, from `print` to a program that reads its input. Every program on those pages is compiled and
  run by a test suite, so a page that has drifted from the compiler fails.
- **[The reference](https://sysl.sh/reference/)** — every construct written down once, in its own
  place, with the rules complete rather than the ones a beginner needs first. This is the
  specification: where a rule has an edge, the edge is shown as a program that the suite runs.
- **[`library/`](library/)** — the standard library, written in sysl, with its own `@test` functions
  beside what they test. `sysl.regex` is written as a literate module, which is the largest worked
  example of the format in the tree.

The site lives in **[sysl-lang/sysl.sh](https://github.com/sysl-lang/sysl.sh)**, which drives this
compiler as a published dependency. That is deliberate rather than incidental: a website documents
the *released* language, so its pages are checked against whatever version is on Central, and a page
demonstrating something only `dev` can do would be wrong for the person reading it.

**There used to be a `design/` directory here as well** — twenty-nine chapters carrying each
rule's argument and the alternatives that were rejected. It was removed on 2026-08-21, once an audit
established that the reference documents every language feature and every piece of syntax. Two
documents saying the same thing is one that goes stale, and the reference is the one with a test
suite behind it. The chapters are in the history: `git show b12e60c3:design/13-modules.md`.

**And there used to be a `guide/` directory** — programs written to *force a language decision*
rather than to demonstrate a finished one. Every finding they made is discharged, and the set was
retired on 2026-09-02 once the last two had somewhere better to be: `ring` is
[`sysl.container.ring`](library/sysl/container/ring/) in the standard library and `slab` is the
[`slab`](https://github.com/sysl-lang/slab) package. What the programs decided is in the
[reference](https://sysl.sh/reference/); `git show 2de44a5f:guide/README.md` is the set's own account
of itself.

## Building

This is a Scala 3 cross-project (the compiler is written in Scala). During bring-up:

```bash
sbt syslJVM/compile     # compile
sbt syslJVM/test        # run the test suite
sbt syslJVM/run         # run the CLI
```

To see the language run end to end — source, through the compiler, to a native binary — point the
CLI at any project directory:

```bash
sbt "syslJVM/run run path/to/project"
```

Anything after a `--` goes to the program rather than to sysl, which is what a `main(args: []string)`
reads — `sysl run <path> -- <args>`.

That needs a `clang` on the PATH: sysl emits textual LLVM IR and links it with clang.

**Bindings to real C libraries are packages, and none of them lives in this tree** — see the list at
[github.com/sysl-lang](https://github.com/sysl-lang). Each is a library rather than a single file:
its sysl, and the `.c` shims that read whatever only a header knows. `--lib` takes either the tree
itself or an artifact built from it, and both roads carry the shims, because a `.c` anywhere in a
tree is compiled with it ([a library may carry C](https://sysl.sh/reference/ffi/)) — whether that
tree is a library, a package a `dependencies` block brought in, or the project itself:

```bash
sysl run prog.sysl --lib /path/to/some/package     # the source tree
sysl build-lib /path/to/some/package -o /tmp/it.syslib
sysl run prog.sysl --lib /tmp/it.syslib            # or an artifact
```

Ordinarily a program names one in `package.hocon` instead and `sysl build` fetches it, which is what
[packages](https://sysl.sh/reference/packages/) is about.

**The boundary runs both ways.** `@export` publishes a definition under a plain, unmangled C symbol,
and `sysl build-c` writes a static archive and a C header for an existing C project to link — so
sysl can sit underneath a C program as readily as it sits on top of a C library
([the foreign interface](https://sysl.sh/reference/ffi/)):

```bash
sysl build-c mylib -o libmylib.a   # writes libmylib.a and libmylib.a.h
clang main.c libmylib.a -o app
```

That archive is self-contained: whatever of the standard library the module reaches is compiled into
it, because a `.syslib` is not something a C link line can be handed. Libraries you chose — `libm`,
anything `@link` named — are still yours to pass, and `build-c` prints which.

**Binding a library your package manager installed takes two flags**, because a toolchain searches
its own directories and nothing else — on a Mac that means `/opt/homebrew` is invisible to it:

```bash
sysl run prog.sysl --include-path /opt/homebrew/include --link-path /opt/homebrew/lib
```

`--include-path` is where a header the shim `#include`s is looked for and `--link-path` is where a
library a `link` directive named is; both are repeatable and searched in order. A binding needs both,
since it has to compile against the headers before there is anything to link. Neither is guessed at
for you, and neither belongs in a source file — where a prefix lives is a fact about your machine,
not about the code. `LIBRARY_PATH` and `CPATH` work too, since clang reads them, and
are the better answer for a machine where the setting is always the same.

A **package** whose C includes headers it does not carry can name them, and then a build that forgot
the flag is refused by name rather than by a header you have never heard of — `requires { headers
{ lwip = "…" } }` in its `package.hocon`, answered with `--include-path lwip=<dir>` —
see [headers a package needs and does not carry](https://sysl.sh/reference/packages/).

**For an ordinary installed library, neither flag is needed at all** — a package names the library and
this machine is asked where it is:

```hocon
requires {
  pkg_config { sdl3 = "SDL3 — brew install sdl3, or Debian's libsdl3-dev" }
}
```

`sysl run .` then works with nothing on the command line: `pkg-config --cflags` reaches every C
compilation and `--libs` reaches the link line, including the `-Wl,-rpath` a hand-written
`--link-path` leaves out. The package still names a library and never a path — what supplies the path
is the machine rather than a person copying its layout. Asked only for the **host**, since a cross
build's headers are the target's, and overridden by `--include-path <name>=<dir>` whenever you would
rather say it yourself.

**Both bindings that used to live here have repositories of their own now, and `bindings/` is gone.**
SQLite went first — [sysl-lang/sqlite3](https://github.com/sysl-lang/sqlite3), the first sysl package
outside this tree — because a binding to a library nobody is obliged to have
installed is a *package*, not an example, and keeping it here made the compiler's own suite depend on
SQLite being present.

POSIX regex followed to [sysl-lang/regex](https://github.com/sysl-lang/regex), and its reason is the
weaker one, which is the point: nothing had to be installed for it and the suite it added was green
everywhere, so the argument was only that a library is not part of the language and does not belong
in the language's repository. It is the organisation's worked example of binding a C library the
machine already has — a shim for what only a header knows, no `@link` for what the driver already
passes, and a `requires` clause naming what it needs of the target. What sysl still owns is the
*mechanism*: carrying the C and resolving the externs are both pinned on fixtures in
`LibraryBuildCliTests`, where the inputs
can be chosen to be discriminating rather than being whatever one real library happens to do.

A program's own unit tests are `@test` functions written beside what they test, and `sysl test` is
what runs them ([`@test`](https://sysl.sh/reference/attributes/)):

```bash
sbt "syslJVM/run test path/to/project"                  # every @test under a directory
sbt "syslJVM/run test path/to/project --filter empty"   # the ones whose name holds this
```

A test passes by returning; `@test(should_trap)` is for the ones whose subject is a check that
should fire, and passes only if the run does not come back. Every other build drops the tests, so
they cost a program nothing.

Every program is compiled against the standard module, and **there is no step to run for it**. The
library's own source ships with the compiler and is read off disk — `share/sysl/library` under the
install prefix, found from the binary's own location, or `library/` in a checkout. The artifact built
from it is a derived file — object code for one machine — so a clone, a fresh worktree, or a stale
container after the encoding moved all have the same answer, and the compiler gives it in well under
a second, announced on stderr. It goes in your cache directory, keyed by a fingerprint of the library
it was built from, so every project on the machine shares one and an upgrade needs no invalidating.
`sysl build-lib library --std` writes one explicitly if you want to.

That rebuild is not a silent fallback. What the design refuses is compiling against a *different*
standard module than the one asked for, so an artifact named with `--std-lib` is never rebuilt and a
compilation stops when it will not read: somebody who wrote down which artifact to use is owed the
truth about that one. `--no-std-lib` is the third way — it compiles the library's source into the
program with no toolchain at all, which is what the test suite takes and what makes the first build
possible.

Building a library also needs an **`llvm-ar`**, because a `.syslib` is an `ar` archive whose members
are objects for the machine it was built *for*: a platform archiver indexes only its own format and
silently drops the rest. On a Mac, Homebrew's LLVM is deliberately off the `PATH`, so sysl looks in
`/opt/homebrew/opt/llvm/bin` as well; `--ar` names one anywhere else.

The JS and Native cross-targets exist in the build but the JVM target is the working one during
development.

## Using the compiler as a library

The command line takes paths, because a program on disk is what it is for. A tool that *generated*
the source it wants compiled — a documentation harness holding a page's code block, an editor with an
unsaved buffer, a test with an inline program — has no file and no reason to make one. The same
compiler is on Maven Central and takes a string:

```scala
libraryDependencies += "sh.sysl" %% "sysl" % "0.0.109"   // %%% in a cross-project
```

The Maven Central badge at the top of this page is the published version, so it is what to check this
line against.

```scala
import sh.sysl.api.Sysl

Sysl.run("main()\n    print(1 + 2)")     // Right(Sysl.Run(0, "3\n"))
Sysl.runBody("print(1 + 2)")             // the same, with the `main` supplied
Sysl.compile("main()\n    print(x)")     // Left("… undefined name 'x' …")
```

`compile` and `run` take a whole program. `compileBody` and `runBody` take a **body** — the statements
a program runs, with no `main` written around them, which is what a documentation page shows and what
a test writes inline. Supplying that wrapper is structural rather than textual, which is why it lives
in the compiler rather than in each caller: at a file's top level `f()` is a call and `f() -> int = x`
is a declaration, and nothing about the characters separates them.

Each has a `…Files` form taking several `Sysl.File(name, text, dir)`, where `dir` names the module a
file belongs to when there is no filesystem to read it from.

`check` is the one that answers the mistakes as **data** rather than as the paragraph the driver would
have printed — for an editor, which cannot underline a paragraph, and for anything else that wants to
do something with an error other than show it. Empty means it compiled, and nothing is truncated:

```scala
Sysl.check("var x = 1\nprint(nope)\n", "t.sysl")
// List(Problem("undefined name 'nope'", Some(Span("t.sysl", 2, 7, 2, 11))))
```

A `Span` is 1-based lines and columns with an **exclusive** end, so its width on one line is
`endCol - col`, and it covers the token the diagnostic points at.

**No signature here mentions a syntax tree.** Everything is `String`, `Int`, `List` and `Either`, so a
node added to the AST is not a breaking change to anything compiled against this. `Sysl.canRun` says
whether this machine has the clang that linking needs; compiling to LLVM IR needs nothing installed.

### The IR, for a back end that is not LLVM

`sh.sysl.ir` is the same compilation as **data** — types, globals, declarations, and functions made of
basic blocks made of instructions. `Codegen.module` answers with an `ir.Module` and `ir.Printer` is
what turns one into LLVM's textual form, so a tool targeting a machine LLVM does not build for reads
the shapes the compiler decided instead of parsing them back out of what it printed.

It is a sibling of `sh.sysl.api` rather than part of it, precisely because the paragraph above is a
commitment: an IR *is* a tree. What it offers in exchange is a version to pin — **before 0.1.0
anything in it may change in any release**.

## Changelog

[CHANGELOG.md](CHANGELOG.md) carries every release, newest first. It is generated from the GitHub
release bodies by `changelog.py`, so the releases page and the file say the same thing and the file
is the copy that works offline and shows up in a diff.

## License

ISC — see [LICENSE](LICENSE).
