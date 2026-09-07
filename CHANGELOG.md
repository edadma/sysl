# Changelog

Every release of sysl, newest first.

This file is **generated** by `changelog.py` from the GitHub release bodies, which are the canonical
copy -- correct a mistake there and regenerate, rather than editing this file. Versions are
`MAJOR.MINOR.PATCH`; while the leading zero stands the language is still moving, and a release may
change what an existing program means. Where it does, the release says so.

## Unreleased

#### Analysing a large program no longer costs the tables once per question

The analyzer asks speculative questions everywhere -- whether a receiver has a member of some name,
which of several overloads the arguments fit, whether a bare name resolves at all -- and each one is
a real walk whose registrations have to be dropped again afterwards. It used to drop them by copying
the six whole-program instantiation tables before the walk and putting them back after, which costs
the size of the tables once per question: a program's instantiations multiplied by its call sites.

The tables now keep an undo log instead, so a walk is taken back by what it *wrote* rather than by
what the tables *hold*, and a question that registers nothing costs nothing. Measured on a generated
program of 14,005 lines, analysis allocated 13,966 MB before and 862 MB after -- and, more to the
point, the cost per line used to grow with the program (636 KB per line at 3,505 lines, 1,046 KB at
14,005) and now falls (78 KB, then 65 KB). Nothing about what is compiled changes; large programs
simply stop needing a heap proportional to their own square.

## 0.0.108 — 2026-09-07

#### The compiler gives its own GC a heap ceiling by default

Scala Native's Immix collector reads `GC_MAXIMUM_HEAP_SIZE` from the environment and defaults to
**unlimited** when it is absent, so a large program compiled by someone who had never heard of the
variable grew until it met physical memory rather than collecting — on a memory-constrained host,
`Out of heap space grow heap` followed by a hang rather than a clean failure.

The compiler now sets `GC_MAXIMUM_HEAP_SIZE=128g` on itself before it allocates anything of size,
unless the caller already set one — a caller's own value is always left exactly as given. There is
no link-time hook for the GC's own default, so the mechanism is the compiler's own entry point:
`setenv` the default, then re-exec the same binary with the same arguments. `execv` replaces the
process image outright, so stdin, stdout, stderr and the eventual exit status all carry through
unchanged, and the re-exec cannot loop — the variable it sets is what stops it running a second time.

128 GB was picked by measurement against `slate`, the largest sysl program there is: capped runs at
2, 4, 8, 16 and 32 GB each either ran out of heap in seconds or grew to the cap and made no further
progress, 36 GB was the smallest that completed (92 s, 36.9 GB peak, confirmed at 40 and 48 GB too),
and the default is twice that floor, rounded up to the nearest power of two.

## 0.0.107 — 2026-09-07

#### A parse holds one memo table per token, not one per step

`PackratReader.rest` answers a **new** reader on every call, and every reader carries its own memo
table. A rule consumes each token many times over a backtracking parse and every memoized result
keeps the reader it ended at, so those tables — allocated, immediately shadowed, never read — were
retained for the whole file. Measured over a 4,000-line program: 129 MB still live at the end of a
parse whose tree is 5 MB, or about 33 KB for every line of source.

The reader is now one instance per token position, built the first time the position is reached and
reused after. The memo is sound at that granularity because it is only ever consulted at the
reader's own position — the same entry is found by the same lookup — and the single
`(parser, position)` map that used to hold the whole file at once, whose `growTable` is where a
large build ran out of heap, is now one small map per token. The same program leaves **236 bytes a
line**, and a hello-world compiles under `GC_MAXIMUM_HEAP_SIZE=512m`, which used to run out of heap
at four times that.

Three smaller allocations went with it: the grammar's `at`, `describe`, `maybe`, `asOneToken` and
`repeatedly` bound their by-name argument once instead of rebuilding the whole sub-parser on every
application, and a terminal's `… expected` is built when the terminal is, not on every token it
declines.

Nothing about the grammar moved, so every program parses to exactly the tree it did.

## 0.0.106 — 2026-09-07

A language and library release. `sysl test` gains setup and teardown hooks, and `sysl.unicode.fold` fixes an ordering bug in casefolding.

### `sysl test` gains `@setup`, `@teardown`, `@setup_all` and `@teardown_all`

A module may now write `@setup` and `@teardown`, which run before and after each `@test` it declares, and `@setup_all` and `@teardown_all`, which run once around the module's whole run. At most one of each per module — a second is refused, and the message names the first.

The scope is the module and the process is the point. `@setup` runs inside the test's own process, so what it leaves in module storage is what the test finds. `@teardown` runs in that process where the test returned, and in a process of its own where it did not — a trap takes the process with it, so what a teardown can release there is what outlives a process. The `_all` hooks are their own invocations for the same reason, and share with a test only what outlives one: a file, a pid, a path.

A fault anywhere in one process leaves the same exit status, so the dispatcher writes a byte to standard error as it passes each boundary and the runner reads how far the run reached off what arrived. A `_all` hook that does not come back has no test to hang a failure on and gets a row of its own, and a module whose `@setup_all` failed runs none of its tests.

See [`reference/attributes.md § The hooks a module may write`](https://sysl.sh/reference/attributes/#the-hooks-a-module-may-write).

### `sysl.unicode.fold` decomposes before casefolding

`utf8proc` applies its casefold mapping per code point ahead of canonical reordering, so folding straight to `Casefold | Compose` could leave a mark the casefold mapping introduces sitting where the original character was rather than where its combining class puts it — `U+1FA4` is the case this was found on. Unicode caseless matching is NFD, then fold, then NFD, so `fold` now decomposes first and lets the second pass reorder and recompose:

```
fold(s) = mapped(mapped(s, Stable | Decompose), Stable | Compose | Casefold)
```

## 0.0.105 — 2026-09-05

**a start offset on every search, and Horspool under the ones over bytes**

A library release: every search in `sysl.text` and `sysl.slices` now takes a place to start, and the
ones over bytes stopped being the naive scan. Nothing about the compiler moved, so a program that
does not search compiles to exactly what it did.

### `from` names a place to start, and the answer is an offset in the whole receiver

Walking every occurrence of something meant cutting the haystack up and adding the offset back on,
at every call site that did it. `sysl.text.Search` and `sysl.slices` now carry a `_from` form of
every search instead:

```sysl
import sysl.text.Search

val s = "abcabcabc"
var at = s.index_of("abc")

while at.is_some()
    val k = at.unwrap()

    print(k)
    at = s.index_of_from("abc", k + 1)
```

```
0
3
6
```

The new members are `index_of_from`, `last_index_of_from`, `index_of_byte_from` and
`last_index_of_byte_from` on `sysl.text.Search` — so they are there for a `string` and for a
`[]const u8` alike, since they are trait defaults over `view()` — and `index_of_from` and
`last_index_of_from` in `sysl.slices`. They read as **one rule**: the search is over `self[from..]`,
and what comes back is an offset into the receiver rather than into the suffix.

**A backward search given a `from` looks at that same suffix, and answers with the *last* occurrence
in it.** It does not run downwards from `from`. That is the half worth reading twice, because the
other convention is the one some libraries pick — and it is what makes the forward and backward forms
say the same thing about which elements are being looked at.

**A `from` past the end answers `None` rather than trapping**, which is the whole reason `k + 1` above
is safe to write at the last occurrence with no bound test of its own. The length itself *is* a
position, which is where the empty needle is still found.

`sysl.text`'s own `split` and `replace_all` are written on it now, which is what the form is for.

### The searches over bytes are Boyer-Moore-Horspool

`index_of`, `last_index_of` and `count_of` were the naive O(n·m) scan, with a line in the file saying
a sub-linear one might replace them one day. It has. A needle of two bytes or more is looked for by
aligning its **last** byte and, on a mismatch, shifting the window by however far that byte's own last
occurrence in the needle is from the end — so the common case reads a fraction of the haystack.

Over a megabyte in which the needle's first byte occurs four thousand times:

| | before | after |
|---|---|---|
| `index_of` | 478 µs | **35 µs** |
| `count_of` | 491 µs | **34 µs** |

**Nothing observable changed.** The offsets, the empty-needle conventions and the non-overlapping
count are exactly what they were. What says so is a differential test in `library/sysl/text/tests.sysl`:
two thousand random haystacks over a three-letter alphabet, at lengths that fall on both sides of the
threshold, compared against a scan written out longhand in the test file — sharing no helper with the
implementation, so it can fail for the interesting reason — plus a counter asserting that both paths
were actually reached, since a differential test that only ever ran one of them would be agreeing with
itself.

**The shift table is 256 bytes on the stack, so `sysl.text.find` still allocates nothing** and is
still reachable under `@no_alloc`. A shift is capped at 255 rather than widened to a machine word: a
shift shorter than the ideal one is still correct, so the cap costs a needle longer than 255 bytes a
few extra windows and costs every other needle nothing.

**Below 32 bytes of haystack the naive scan is still the faster answer and is still what runs.** That
crossing was measured rather than reasoned about, and it is a good deal lower than the table's fixed
cost suggests — 256 adjacent byte writes are a handful of vector stores rather than 256 of anything,
and a window the shift skips costs nothing where the scan's costs a bounds-checked load and a branch.
A one-byte needle never builds a table at all.

### Upgrading

`brew upgrade sysl`. There is nothing to change in a program: every existing call means what it meant,
and the `_from` members are additions.

## 0.0.104 — 2026-09-05

**a sanitizer run stops being served from cache, and --link-path carries an rpath**

Two toolchain fixes, both found by writing bindings against the compiler rather than by testing it —
and the first is a **behaviour change** that anybody checking a package with a sanitizer wants to
know about before they read another green run.

### Behaviour change: a sanitizer run rebuilds, because the build's environment is now part of the cache key

**`sysl run` and `sysl test` cache the executable they built, and the key did not include the
environment.** Everything in it was settled by the command line and the source tree; the one input
that is neither was missing. So this, which is the recipe for checking a binding:

```
sysl test .                                          # an ordinary run, cached
SYSL_EXTRA_CFLAGS="-fsanitize=address" sysl test .   # served the SAME binary
```

replayed the **uninstrumented** executable the first run had left in the slot, ran it, and reported
green having looked at nothing. Touching a file did not help — the key is over content — and neither
did anything else short of clearing the cache, which nobody had a reason to do.

**A sanitizer is the worst possible place for a stale answer, because its whole output is an
absence.** A wrong number gets noticed. A clean run looks exactly like a clean run.

The key now carries `SYSL_EXTRA_CFLAGS` — which also decides the `sanitize_*` attributes the IR is
marked with, so it changes what is *compiled* and not only the command line — plus the five variables
that name a cross toolchain (`WASI_SDK_PATH`, `ANDROID_NDK_ROOT`, `ANDROID_NDK_HOME`, `ANDROID_HOME`,
`ANDROID_SDK_ROOT`), each of which selects a different compiler and sysroot.

**What this costs is one rebuild the first time you change the variable**, and nothing after that:
the key is over the value, so the second sanitizer run is free exactly as the second ordinary run is.
A variable that is not set contributes nothing, so no cache entry anybody already has is invalidated
by upgrading.

**`SYSL_LIB` is deliberately not in it.** It names where the library source *is*, and the key already
carries that library's fingerprint — its contents. Adding the path would make two identical trees at
two paths miss each other's entry for no gain.

**If you have run a sanitizer over a package under an earlier release, that result is worth nothing
and should be taken again.** `nm -u <binary> | grep -c asan` was the only way to tell, and it is
still worth running.

### `--link-path` now emits an rpath, so a dylib with an `@rpath` install name loads

A shared library whose install name is `@rpath/libfoo.dylib` — which is what CMake writes for any
library given a `SOVERSION`, so it is the ordinary case rather than an exotic one — linked cleanly
and then died at startup:

```
dyld: Library not loaded: @rpath/libwebview.0.12.dylib
      Reason: no LC_RPATH's found
```

which reads as a missing library sitting exactly where it was said to be. `--link-path DIR` emitted
`-L DIR` and nothing else, and a sysl program cannot pass a raw linker flag, so there was no way to
say the other half from inside sysl at all. `sysl-lang/webview` had to work around it in its Homebrew
formula by overriding the install name.

`--link-path DIR` now emits `-Wl,-rpath,DIR` beside the `-L`, with the directory made absolute —
an rpath is read by the loader in whatever directory the program is eventually *run* from, so a
relative one would name a place nobody meant.

**This is a place sysl deliberately does something clang and rustc do not, and that was measured
rather than assumed.** A `libfoo.dylib` at an `@rpath` install name, linked with the search-path flag
alone, dies at startup under both, with zero `LC_RPATH` in either binary; rustc has an opt-in
(`-C rpath`, off by default) and clang has none. The reason sysl answers differently is that
`--link-path` is not `-L`: clang's `-L` sits on a line the writer is composing, next to the
`-Wl,-rpath` they would add themselves, while sysl composes the whole line and the writer never sees
it. A directory named to sysl is the only thing anybody gets to say about where the library is.

**Two things it deliberately does not do.** A directory a `pkg-config` probe answered with gets no
rpath — a `.pc` file supplies its own where the library wants one, and a second guessed one would
override a decision the library had already made. And a **freestanding** target gets none: `wasm-ld`
and `ld.lld` both accept `-rpath` for a bare-metal link without complaint, and what they would write
is a run-time search path for a dynamic loader that does not exist on such a machine.

### Not in this release, because it was already in 0.0.103

The three standard-library gates that say *"BSD libc, not glibc"* — `__error` against
`__errno_location`, `ENOTEMPTY` at 66 against 39, and a 16-bit `mode_t` — were switched to a derived
`bsd` symbol in **0.0.103**, along with `Os.bsd` and `bsd`'s place in the closed `#if` vocabulary.
This is stated because the work was queued again for this release and turned out to be done; there is
no FreeBSD target and this release does not claim one.

### The one thing worth knowing that is not a change at all

**A method can already name the box it was called through, and the receiver is `&self`.** It has been
in the language for a long time and neither of the two org bindings that needed it used it — both
invented a two-type split instead, a private owning handle carrying the destructor plus a copyable
value holding a `&` to it. `&self` hands the method the box, so a value it constructs takes a *share*
rather than a second owner: one type, one `impl Drop`, and the resource outliving everything that
kept a share of it.

`reference/declarations.md` now says so with a worked example, and `reference/memory.md`'s destructor
section points at it — the person who needs it is reading about destructors rather than about
receivers, which is most of why it went unread.

## 0.0.103 — 2026-09-04

Three cards, and two behaviour changes to read before the features.

### An array and a slice gained traits, and a `Uuid`'s hash value changed with them

**This changes what an existing program computes.** Both halves are widenings — nothing that compiled
stops compiling — and they are here rather than under the features because a widening is invisible to
the suite that produced it and to a consumer's suite alike, unless that consumer wrote a test pinning
the old answer as a decision.

- `[N]T` implements `Ord` and `Hash`, and `[]T` implements `Ord`. A package that had written its own
  blanket implementation for either now has two, and is refused rather than shadowing — `override
  impl` is what says the replacement was meant.
- **`sysl.encoding.Uuid` hashes to a different number.** It derives `Hash` now, and the derived mixer
  folds the elements' hashes where the block it replaces folded the raw bytes. A hash value has never
  been a promise here — nothing persists one across versions — but a program that stored one will not
  find it again. Its **ordering is unchanged**, which its own tests pin, including the one asserting
  that a v7 sorts by time.

### A `const` of an aggregate type is refused with different words

The rule has not moved: a constant is a scalar and an aggregate is a `val`. What changed is that the
refusal now says so and names the keyword to write, where it used to make a claim about constancy
that the neighbouring `val` contradicts. A program grepping for the old text will not find it.

### An array orders and hashes, and a slice orders

`impl[T: Ord] Ord for []T`, `impl[const N: usize, T: Ord] Ord for [N]T` and
`impl[const N: usize, T: Hash] Hash for [N]T` join the `Eq` rows they belong beside, so a struct
wrapping an array can derive all three where it could derive only equality.

Ordering is **lexicographic, first element first**, with a shorter sequence ordering before a longer
one it is a prefix of — the rule a `string` already follows and the one a tuple's `Ord` follows over
its parts. Hashing folds the elements' hashes in order, so `[1, 2]` and `[2, 1]` are different keys.

**A slice deliberately has no `Hash`.** A hash is a promise about a key and a slice is a view: putting
one in a table leaves the storage reachable and writable by whoever owns it, so the key can change
under the table and the entry becomes unfindable. An array is a value and copies, so it has no such
owner. A caller holding a slice it means as a key hashes something it owns.

`sysl.encoding.Uuid` now derives `Eq`, `Ord` and `Hash` instead of spelling two of them out.
`sysl.math.bigint.BigInt` keeps its hand-written blocks and now says why: its fields are a sign and a
little-endian magnitude, so a structural comparison would order every positive before every negative
and then decide on the least significant limb.

### A `const` of an aggregate type says which rule it is applying

`const zeros: [16]u8 = [0; 16]` was refused with *"the value of 'zeros' is not a constant
expression"* — a claim about an expression that a `val` one keyword away lays into the object file as
constant data. It now reads:

    a constant is a scalar, and [16]u8 is storage — write 'zeros' as a 'val', which is laid into
    the object file as constant data

`const` is narrower than a static `val` on purpose and that has not changed: a constant is a folded
value that can stand in a type, answered before any type table exists, where a `val` is storage. The
declared type is simply resolved before the value is folded now, so the rule that was always the right
answer is the one that gets to speak. A scalar `const` whose value is genuinely not constant still
gets the old message, which is the case it was written for.

### A module's headline comes from the file named for it

The generated API index described twenty of its thirty-five modules with nothing at all, and several
of the rest with a sibling file's sentence — `sysl` by `check.sysl`'s account of assertions,
`sysl.unicode` by the grapheme walker, `sysl.fs` by its error type. `sysl-doc` now takes a module's
summary from the file named for the module's last path segment and from no other, and a module whose
headline file has none gets a blank row rather than a borrowed sentence.

Eleven modules gained a file holding their headline and nothing else. All thirty-five now describe
themselves.

## 0.0.102 — 2026-09-04

Five standard-library modules, and one behaviour change worth reading first.

### `sysl.text`'s `trim` on a `string` is Unicode now

**This changes what an existing program does.** `trim`, `trim_start` and `trim_end` on a **`string`**
receiver decode and test the Unicode whitespace property, where they walked bytes with
`Ascii.is_space` before — so a no-break space, an ideographic space and the next-line control are
removed where they used to survive.

Every ASCII input answers exactly as it did; the set only widened. What changes is a program that was
deliberately *not* trimming a `U+00A0` because it wanted to keep it.

The **`[]const u8`** receiver is untouched and stays ASCII, byte for byte. A byte is not a character
— a no-break space is `0xc2 0xa0`, and neither of those is a space on its own — and a wire format
wants the ASCII answer. A caller who wants the old behaviour on text has `s.bytes.trim()`.

`columns` and `char_columns` are **unchanged**, so nothing that lays out a diagnostic moves. The
grapheme-cluster walk is a new function beside them rather than a replacement.

### `sysl.text` — comparing and measuring

- `eq_fold`, `starts_with_fold`, `ends_with_fold`, `contains_fold` compare through
  `sysl.unicode.fold`, which is the operation `to_lower(a) == to_lower(b)` gets wrong: `"STRASSE"`
  and `"straße"` are the same word and lowercase to two different strings.
- `cluster_columns` and `grapheme_columns` measure a grapheme cluster at a time, so a joined family
  emoji is two columns rather than the eight its code points come to.

### `sysl.math.bigint` and `sysl.math.decimal`

Integers with no width — sign and magnitude over 32-bit limbs, schoolbook multiply, Knuth's algorithm
D, `pow`, and text in any base from 2 to 36 — and exact decimal arithmetic over them.

A `Decimal` is a coefficient and a scale, so `0.1 + 0.2` is `0.3` and stays `0.3` over a million
rows. The scale is part of the value: `1.50` and `1.5` compare equal and render differently, which is
what a price quoted to the cent needs. Division takes a scale and a `Rounding`, `HalfEven` by
default.

### `sysl.log`

A `Record`, a `Sink` trait of one method, `text` and `json` renderings, and the four level names over
a threshold. `log_at` takes the instant and `log` reads the clock, so the module is usable on a board
with no clock at all; the threshold is an atomic so a filtered `debug` costs a relaxed load rather
than a lock.

### `sysl.encoding` — UUIDs

`v4` and `v7`, RFC 9562. A v7 puts a millisecond timestamp in the high 48 bits, so ids made in
sequence sort in sequence — which is what a database key should be. `v4_of` is the constructor for a
value that must be **unguessable**, since `sysl.rand` is PCG32 and a v4 built from it is not.

### `sysl.path` — globs

`matches(pattern, p)` is Go's `filepath.Match` plus a globstar, over bytes and allocating nothing.
A leading `.` is hidden from every wildcard, which is what keeps a tool driven by `**` out of
somebody's `.git`. `sysl.fs.walk(root).matching(pattern)` filters a walk by one.

### A variant that carries nothing is constant data

The one compiler change, and it is what `sysl.log` needed. `Option[T] = None` in module storage was a
*computed* initializer — filled by the program's prologue — which is invisible on a hosted target and
fatal on a bare one, where an `@export` reaching such a slot was refused outright. A module-level slot
may now start out unset and be laid into the object file, so a module holding one is reachable from a
freestanding `build-c` archive.

A dataless variant of a *simple* enum was never affected: that enum lowers to its discriminant. What
did not fold was the empty variant of an enum whose other variants carry something, which is exactly
`Option`'s `None`.

## 0.0.101 — 2026-09-04

One card, and it is the one that makes a `string` in sysl mean the same thing
in storage and in operations.

### `sysl.unicode` — the Unicode Character Database, in the standard library

```sysl
import sysl.text.{to_upper, to_lower}

print(to_upper("héllo"), to_lower("HÉLLO"))
```

```
HÉLLO héllo
```

That was `HéLLO hÉllo` in 0.0.100. A sysl `string` has always been UTF-8, and
until this release nothing in the library knew what the bytes *meant*:
`to_upper` mapped `a`–`z` and left everything else alone, so a name with an
accent in it came back exactly as it went in and a program that wanted
otherwise had to go and find a binding.

`sysl.unicode` is that data, in the library, answering out of **utf8proc
2.11.0** — Unicode 17.0.0 — vendored beside the module. It carries case
mapping, case folding, the four normal forms, grapheme segmentation and
`General_Category`, and `sysl.text.to_upper` and `to_lower` are walks over it.

```sysl
import sysl.unicode.{fold, normalize, Form, grapheme_count, category, Category}

print(fold("Straße"), fold("ﬁt"))
print(normalize(Form.Nfd, "é").len, normalize(Form.Nfc, "é").len)
print(grapheme_count("e\u{301}x".bytes), grapheme_count("\u{1f469}\u{200d}\u{1f4bb}".bytes))
print(category('ǲ'))
```

```
strasse fit
3 2
2 1
Lt
```

**`fold` is the operation a caller comparing two strings actually wanted**, and
it is not lowercasing: it may change a string's length, which is how `STRASSE`
and `straße` come to be the same text. **`normalize` is what makes `==` mean
what a reader thinks it means** — the same text typed on two machines is
routinely two different sequences of code points. **A grapheme cluster is the
third answer to how long text is**, and the one a person would give.

#### It costs nothing to a program that does not call it

That is the claim the argument rests on, and the reason a Unicode table had been
refused three times before. `sysl.text` is not optional — it is what places a
diagnostic's caret — so the reasoning was that every freestanding program would
link 330 KB of tables.

What a program links is decided by the functions it calls, not by the modules it
names. The library is one archive with an object per C file, an archive member
is pulled in only to resolve a symbol something already referenced, and the link
dead-strips. `StdArtifactTests` pins both halves: a program printing an int
carries no utf8proc symbol and stays under 200 KB, and one calling `to_upper`
carries `utf8proc_toupper` and goes over it.

#### And it reaches a board

The vendored copy carries five added lines that make its malloc-using surface
optional, so it compiles for a freestanding target with no libc at all — at
`thumbv6m-none-eabi` the object's only undefined symbol is `__aeabi_uidivmod`.
Five emulated boards in the suite now uppercase `é` out of the database and read
the answer back off a UART.

The sixth is excluded, and the exclusion is the most informative thing there:
330 KB of tables do not go into the micro:bit's 256 KB of flash, which is the
honest answer to what this costs where the answer is a hard limit.

#### What is deliberately not here

**Display width.** `sysl.text.char_columns` has answered it from 499 UCD ranges
since before this module existed, and two answers to one question in one
standard library is worse than either. Which one stays is decided by who pays:
`columns` is reached by programs that never asked for anything Unicode, so its
cost is inherited; `to_upper` is reached by deciding to uppercase a string, so
its cost is opted into.

**ASCII case mapping is still `sysl.text.Ascii`, unchanged, and is still the
right tool for a protocol identifier.** Unicode case mapping carries characters
*into* the ASCII range — the Kelvin sign lowercases to a plain `k` — which is
the shape a spoofing check exists to prevent.

#### One behaviour change to know about

**`to_upper` and `to_lower` answer differently for text outside ASCII, which is
the point of the release** — `to_upper("héllo")` was `HéLLO` and is `HÉLLO`. If a
program depended on the old answer it was depending on a limitation.

`to_upper("straße")` is `"STRAẞE"`, not `"STRASSE"`. The mapping is the *simple*
one, character for character, which is what a `char -> char` signature can
promise; the capital sharp s is what the database gives, and it round-trips
where `SS` could not. A caller who wanted `SS` was comparing two strings, and
`fold` is that operation.

**For a protocol identifier, reach for `sysl.text.Ascii`, which is unchanged.**
An HTTP header name, a scheme and a hostname label are ASCII by definition, and
Unicode case mapping carries characters *into* that range — the Kelvin sign
lowercases to a plain `k` — which is the shape a spoofing check exists to
prevent.

#### There is no compiler change in this release

Not one line under `shared/src/main/`. The whole of it is the standard library
and the tests: the compiler is what it was in 0.0.100.

## 0.0.100 — 2026-09-03

Two cards, both additive, and both about a program saying what it means rather
than what the machine it happens to be on is called.

### A program may read a clock without naming one

```sysl
import sysl.time.{now, monotonic, since, whole_micros, Instant}

val y2020 = Instant(1577836800000000)

print(whole_micros(since(now(), y2020)) > 0, whole_micros(monotonic()) >= 0)
```

```
true true
```

That program names no capability, no chip and no package, and it is the same
source on a laptop and on a board. `sysl.time` declares `sysl_wall_us` and
`sysl_monotonic_us` and defines neither, so what supplies the numbers is
whatever the program is linked against: `sysl.posix.time` on a host, over
`clock_gettime`, and a package binding an RTC chip over I²C on a board. Moving
between the two is a change to `package.hocon` rather than to any line of the
program.

`sysl.time` still asks for no capability — an `extern` is a name and a signature
— so the calendar goes on running on a bare machine, which is the whole reason
the clock could not simply be added to it.

**Both spellings are right and neither is deprecated.** A program that is only
ever going to run on a host imports `sysl.posix.time` and calls `now` directly,
which needs no linker cooperation at all.

#### The compiler had to learn one thing for that to work

An `@export` is a reachability root only where the program reaches its module,
which is the rule that stops a package's own test application from putting a
second `main` in every consumer. A seam is exactly the case that rule excludes:
the consumer imports the module that *declares* the symbol and never names the
supplier, because naming one is naming the chip. So the definition was pruned
and the program then failed at the link, against a name no line of it contains.

**A function supplying a symbol an `extern` the program actually calls is now a
root.** It is the accounting an archive member already gets, arrived at from the
other side — the definition is taken because something is going to ask for that
name, and left out otherwise.

The narrowness is what leaves the old rule intact. Nothing declares
`extern "main"`, so no live symbol answers to it and a package carrying its own
program still cannot reach a consumer. A supplier of a symbol your program never
calls is still dropped. And two modules claiming one symbol is refused naming
both, rather than left to the linker to report against a name that appears in no
sysl file.

#### Where the declaration is compiled is where the seam is decided

On a hosted target the standard library arrives prebuilt and was compiled whole,
so its own supplier is already bound and a package cannot displace it — you get
the host clock without asking, which is what a host program wants. Where a board
is served there is no such artifact: `sysl build-c` and every freestanding target
compile the library from source into your program, so the declaration and the
supplier meet in one compilation and the package's definition answers.
**Overriding a clock is something a board does and not something a host does.**

### `bsd`, which is what five of the library's OS gates always meant

`library/sysl/fs` had five `#if macos` gates and not one of them was about macOS.
Each is a place where a BSD libc and glibc disagree: `errno` reached through
`__error` against `__errno_location`, `ENOTEMPTY` at 66 against 39,
`ENAMETOOLONG` at 63 against 36, and `mode_t` at sixteen bits against thirty-two
for both `mkdir` and `chmod`. The symbol had been lying since it was written, and
nothing noticed because macOS was the only BSD sysl targets.

`bsd` joins `hosted` and `posix` as a derived symbol, and a `__bsd__` directory
selects on it as well. It is deliberately **not** a capability: a BSD and a glibc
system are both POSIX, and every one of these gates is about the libc rather than
about what the machine can do.

**It costs nothing today and is the whole of the fix later.** `#if macos` and
`#if bsd` select identically on the targets that exist, so this release changes
no behaviour anywhere. What it changes is what a *second* BSD would do: added as
a bare operating system, it would route into the glibc branch at all five sites,
and exactly one of them says anything — the `errno` accessor fails at the link,
while a wrong `ENOTEMPTY` merely stops matching and a `mode_t` at the wrong width
appears to work on most ABIs.

`platform.sysl`'s `os()` ladder still says `macos`, and is right to: that one is
about the operating system.

## 0.0.99 — 2026-09-02

Three cards, all additive, and all three came out of writing `sysl-lang/curl` —
a libcurl binding, which is the first thing in the org to register a sysl
callback through a variadic C function and the first whose suite needs a server
that deliberately does not answer.

### A variadic tail carries the address of a function, in both directions

```sysl
private on_write(p: *u8, size: usize, nmemb: usize, user: *u8) -> usize = size * nmemb

c.setopt(h, c.OPT_WRITEFUNCTION, &on_write)
```

`curl_easy_setopt(CURL *, CURLoption, ...)` is how all twenty-one of libcurl's
`CURLOPT_*FUNCTION` options are set, and until now `&on_write` could not be
written there:

```
error: a *extern(*byte, usize, usize, *byte) -> usize cannot be passed to '...' — a variadic
argument must be an integer, a float, a char, or a raw pointer
```

**The bytes were already right; only the check refused.** A code pointer is the
width of a data pointer and goes in the same register, and C's own variadic
rules take one exactly as they take a `void *` — so the workaround was to throw
the type away and pass the address as a `*u8`, which compiled, linked, and
fetched a page through the real library.

That workaround is what makes this worth fixing rather than documenting. A
`ptr_cast` to `*u8` erases the signature, so a callback registered with the
wrong arity or the wrong parameter types stops being a compile error anywhere —
on the one option class where getting it wrong corrupts the stack at run time
instead of failing to link.

`usize(&f)` is accepted for the same reason and was the same omission wearing a
different hat: `ptr_cast` already answered an address for a function, and the
conversion table simply had no row for one.

**And what may be written into a tail is what may be read back out of one**, so
a callee names the signature rather than reading a number and casting it:

```sysl
doubler(n: int) -> int = n * 2

apply(x: int, ...) -> int
    var ap: va_list

    va_start(ap)

    var f: *extern(int) -> int = va_arg(ap)

    va_end(ap)

    f(x)

print(apply(21, &doubler))
```

That half was found by grepping for other places pinning the refusal text this
change moved: the write rule and the read rule are one rule in two directions
and had gone out of step. Both messages now say the same thing, and a test
asserts the wording so they cannot drift apart again quietly.

### `sleep` and `nanosleep`

```sysl
import sysl.posix.time.{monotonic, sleep}
import sysl.time.*

val t0 = monotonic()

sleep(50.ms)

print(whole_millis(monotonic() - t0) >= 50)
```

**Nothing in the library made a thread wait for a length of time**, which
`sysl.time.units`' own opening docstring had been advertising with `sleep(5.ms)`
since it was written. What the library had instead was a spin: `Mutex` says in a
comment that a contended lock spins *because there is no wait queue*, `Channel`
inherits the same, and curl's suite was burning a core on an `Atomic[int]` to
hold a server open.

It is in **`sysl.posix.time`**, beside `now` and `monotonic`, for the reason
that module exists at all: a capability requirement is module-wide, so a `sleep`
written beside `Duration` would take the whole calendar away from every
freestanding program that only wanted to add two durations. `sysl-lang/pico2`
has a `sleep(d: Duration)` of its own over the SDK's timer, so the same line
reads the same on a board and on a host.

**A signal cuts a wait short, and `sleep` carries on.** `nanosleep(2)` returns
early with `EINTR` and the time still owed, so a wait written as one call is
quietly shorter than it asked for whenever anything arrives — which is exactly
when a test that depends on it starts looking flaky. `sleep` re-enters with the
remainder until nothing is left, and answers nothing.

`nanosleep` is the other half and is two differences rather than one. It counts
**nanoseconds**, where a `Duration` counts microseconds and so cannot name a
wait shorter than one; and it **answers what was still owed**, which is what
lets a program with a handler installed tell "the wait finished" from "something
happened".

```sysl
var left = nanosleep(2_000_000)

while left > 0
    left = nanosleep(left)
```

## 0.0.98 — 2026-09-01

Four cards, all additive. Two are refusals that were drawn wider than their
reasons; two are the standard library growing what a protocol asks for by name.

### A temporary array moves to the heap, like the array literal that already did

```sysl
import sysl.crypto.sha256

static var fingerprint: []const u8 = []

record(data: []const u8)
    fingerprint = sha256(data)[..]

record("abc".bytes)

print(fingerprint.len, fingerprint[0], fingerprint[31])
```

A view of a local array that gets out of its frame moves the array to the heap
— that has always been so, and it is what lets a function return a slice of
something it declared. An array with **no declaration** had nowhere to move,
so a view of one that escaped was refused. Since 0.0.86 an array *literal* was
promoted too; this is the rest of it.

What makes something a temporary is having no address of its own: a call's
returned `[N]u8`, a field of one, an array literal. It is nowhere yet, so it
is nobody's to move, so it is this frame's.

**Binding it to a name first always worked**, which is what says the refusal
bought nothing:

```sysl
val d = sha256(data)

collected(d[..])          // accepted, and always was
collected(sha256(data)[..])   // refused until now — one frame slot, two spellings
```

What stays refused is storage that really is somebody else's: an array a
caller passed **by value**, and a field of a **named** struct on the frame,
where moving the field alone or the struct with it is a choice the reference
leaves unspecified.

`--explain-escapes` and the `@no_alloc` refusal now name *which* temporary
moved — the call whose answer was sliced — rather than calling everything an
array literal.

### An associated function may carry a default

```sysl
trait Sized
    width() -> usize
    doubled() -> usize = Self.width() * 2

struct Word
end Word

impl Sized for Word
    width() -> usize = 4

show[T: Sized]() = print(T.width(), T.doubled())

show[Word]()
```

A trait member with no receiver was refused a default body, on the reading
that a body with no receiver has nothing to work on. That is false of a
constant and false of the body above, and the reason underneath it — that
every implementation would otherwise inherit one value — is what a default
*is* rather than an objection to one. It was not what told the two cases
apart either: a defaulted method **with** a receiver hands every
implementation one constant just as surely, and always compiled.

What it cost is the property defaults exist for: **a trait can grow**. Four
traits in the standard library are made entirely of receiverless members, so
none could gain one without editing every implementation.

Inheritance and replacement are the ordinary rules — leave the member out to
take the trait's body, write `override` to supply your own.

A **static property** is still refused, and its reason is its own rather than
anything about receivers: it has no parameter list and no receiver, so nothing
is available to vary the value it would hand every implementation.

### `sysl.crypto` grows MD5

```sysl
import sysl.crypto.md5
import sysl.encoding.hex_string

print(hex_string(md5("abc".bytes)))
```

**MD5 is broken and has been since 2004**, and a chosen-prefix collision has
been buildable since 2007 — a pair of colliding X.509 certificates was
constructed to show it. Nothing may be signed with it and no content address
may be one.

It is here because a client does not always get to choose. PostgreSQL's `md5`
authentication answers a salt with `md5(md5(password + user) + salt)`, and a
client that cannot compute one cannot log in to an older server at all. HTTP
Digest, a `Content-MD5` header and an S3 `ETag` are the same shape. `sha1` has
been here on the same terms, for the WebSocket handshake.

`new_md5` streams and `hmacmd5` keys, like the five digests beside it. It
shares every line of the buffering and the padding with them; what it does
differently is write its length field and its digest least significant byte
first, which is the member `Compression` gained — using the defaults this
release allows, so the three compressions already answering the common way
needed no edit.

### …and PBKDF2 and HKDF

```sysl
import sysl.crypto.pbkdf2_hmac256
import sysl.encoding.hex_string

var key: []u8 = [0; 32]

pbkdf2_hmac256("password".bytes, "salt".bytes, 4096, key).expect("a key this can produce")

print(hex_string(key))
```

**They answer different questions and are not alternatives.** PBKDF2 is for a
**password** — little entropy, so the only thing to be done is make each guess
expensive, which is what the iteration count buys and the only thing it buys.
HKDF is for a secret already unguessable but not uniform, such as a
Diffie-Hellman result: it condenses that once and spreads it into as many keys
as a protocol names.

`pbkdf2_hmac1` through `pbkdf2_hmac512`, and `hkdf1` through `hkdf512` with an
`_expand` beside each. The output goes into storage the caller owns, like
everything else in the module, so nothing here needs an allocator.

**HKDF-Extract has no function of its own because it already has one**: it
*is* HMAC with the salt as the key, so `hmac256(salt, ikm)` is it, and only
the expansion has a name — which is the half TLS 1.3 leans on. An absent salt
needs no special case either: HMAC pads a short key with zeroes, so an empty
salt is the zero key the standard names.

The vectors are RFC 6070's, RFC 7914 §11's and RFC 5869's appendix, and MD5's
are RFC 1321's own seven plus RFC 2202's six.

## 0.0.97 — 2026-08-31

Three cards, all additive. Two of them are ways of writing something the
language had no spelling for; the third is a flag a build no longer needs.

### A bound may be written out of line, with `where`

```sysl
show[T](x: T) -> string where T: Display = str(x)

print(show(42))
```

The form Rust, Swift and Kotlin all have. A bracket list is one place to put a
bound and it is not always the readable one: a declaration with several
parameters and several bounds pushes its parameter list, its result and its
body off to the right, and what the reader came for — what the function
*takes* — is the part that moves.

**It is pure syntax over what the brackets already express.** A clause is
folded into the same bound map `[T: Display]` fills, so inference, coherence
and every diagnostic are untouched — nothing downstream learns a clause was
written. It goes at the end of the header line, on a function, a member, a
struct, an enum, a trait and an `impl`.

The two may be mixed, which is what makes the choice per-bound rather than
per-declaration — the short obvious constraint stays in the brackets, and the
long one that would crowd the signature goes below it:

```sysl
same[T: Eq](a: T, b: T) -> string where T: Display =
    if a == b then str(a) else "different"
```

`where` was **already** a contextual word — it introduces a constrained
subtype's predicate, `type Even = new int where value % 2 == 0` — so no keyword
was added and a program may still name a function `where`. The two cannot
collide: one follows a type inside a `type` declaration, the other follows a
signature.

A clause may only bound a parameter the declaration actually declares, and a
value parameter is the case that reads as though it ought to work and cannot —
`[const N: usize]` already states a type, and a value implements no trait:

```
error: this declaration has no type parameter 'U', so a 'where' clause has
nothing to bound — it takes 'T'

error: 'N' stands for a value rather than a type, and a value implements no
trait — its 'const' declaration is where the type its argument must have is
written
```

### A property may belong to the type

```sysl
struct Temp
    n: int

    static freezing -> int = 32

print(Temp.freezing)
```

**A property had nowhere to say `self`.** A member's receiver is written in its
parameter list, and a property is a method with that list left off — so the one
thing separating an instance member from an associated one could not be
spelled, and every property was an instance member by construction. `static` is
what says otherwise, and it is read on the type with no parentheses.

**What the form is for is a bound.** A type parameter is not a value, so a fact
*about the type* — a zero, a width, a limit — has nowhere to arrive from unless
a bound can carry it:

```sysl
trait Bounded
    static lowest -> int

struct Age
    n: int

impl Bounded for Age
    static lowest -> int = 18

lowest_of[T: Bounded]() -> int = T.lowest

print(lowest_of[Age]())
```

`lowest_of` never mentions `Age`, and there is no value of it anywhere in the
program.

Reading one on a value is refused, in the words the mirror case uses. A trait's
static property carries no body — the rule every receiverless trait member
already has, since a default would give every implementation the same constant —
and it gets a refusal of its own, because a property cannot take the advice to
add a `self` parameter.

`static` is not a new word either: it is the same one that makes a binding in an
entry file belong to the module rather than to that file's body, and the two are
told apart by what follows it.

### A `headers` requirement may name its environment variable

```
requires {
  headers {
    pico_sdk = { note = "the pico-sdk's headers", env = "PICO_SDK_PATH" }
  }
}
```

A consumer with that variable set needs no `--include-path` flag.

**The manifest still may not carry a path, and this does not weaken that.** A
variable's *name* is the same string on every machine in the world — it is what
the pico-sdk's own CMake reads — so it is a property of the library's ecosystem
rather than of whoever happens to be building.

An `env` is consulted **only** where no flag named that requirement, so it can
turn a refusal into a success and never one success into another. An empty
variable counts as unset, since `PICO_SDK_PATH=` is somebody clearing it rather
than naming a root. It folds in a dependency's needs as well — a program
depending on `pico2` never writes that requirement down itself, which is the
case it exists for.

### Nothing else changes

No keyword was added, nothing was renamed, and nothing was removed. Every
program that compiled under 0.0.96 compiles here.

## 0.0.96 — 2026-08-31

Two cards, and together they are the other half of 0.0.95's headline. A scalar may sit on the left
of an operator — unless the type on the right is generic, which is every type a library writes, and
unless you want one block to cover every integer width at once.

### A scalar goes on the left of a generic subject too

0.0.95 widened the orphan rule to count a trait's **arguments**, so `impl Mul[Point, Point] for real`
has a home in the module declaring `Point`. The very next check took it back whenever the block was
generic. It asked whether a written trait argument *mentioned* the subject's name rather than whether
it *was* the subject, so this was refused:

```sysl
struct Box[T]
    v: T

struct Pair[T]
    a: T

impl[T] Mul[Box[Pair[T]], Box[Pair[T]]] for Pair[T]
    mul(self, b: Box[Pair[T]]) -> Box[Pair[T]] = b
```

```
error: 'Box[Pair[T]]' is a Pair, and a 'sysl.Mul' whose arguments default names the type it is
written for — so at one Pair this block and a defaulted one would promise the same thing
```

`Box[Pair[T]]` is a `Box`. A defaulted argument list on this subject is `Pair[T]`, and there is no
instantiation at which the two are one type — so there was nothing to choose between and nothing to
refuse. Containment is not overlap.

The check now asks the argument for its own owner key. `impl[T] Mul[Box[int]] for Box[T]` — the case
the rule exists for, which really does collide with a defaulted `Box[T]` at `T = int` — is refused
exactly as before.

**What it cost was the packages, which is where a generic type actually lives.** `linalg`'s elements
are `real`, `f32`, `int` and `Complex[F]`; only the fourth is generic, so three quarters of a scalar
on the left was writable and the quarter the package is generic *for* was not.

### A count goes on either side of a duration

The sweep 0.0.95 owed its own library. `sysl.time` carried a docstring saying `2 * d` could never be
written, which had stopped being true one release earlier.

```sysl
import sysl.time.*

var d = seconds(3i64)

print(whole_seconds(d * 3), whole_seconds(2 * d))
```

**The two sides are two blocks, because they do not type a literal the same way.** `d * 3` reads its
`3` off the parameter it is passed to, so a count on the right arrives as a `long` whatever the
reader wrote. `2 * d` has nothing to read it off — the literal is an `int` before the operator is
looked at — so the left-hand side is a block of its own.

### A blanket over the integers covers every width, and hides none of their own

That left-hand block is written once, over the whole family, rather than once per width:

```sysl
impl[N: Integer] Mul[Duration, Duration] for N
    mul(self, d: Duration) -> Duration = d * long(self)
```

`Integer` is a bound the compiler closes — every `iN` and `uN` is in it, an open family of arbitrary
widths, and nothing a program declares can join — which is what lets one block stand for all of it.

**Writing that was accepted before this release and it was not safe.** A blanket is filed under its
bound's key rather than under any type's, so the question every other block is asked — *does the
compiler already provide this member?* — was put to a subject that is not a type, and answered no.
The block then took the plain `mul`, and an integer's own multiplication reached by name stopped
compiling in **every** program, whether or not it mentioned the block:

```
error: 'd' of 'bound.sysl$Integer.mul.int' is Duration, but int was given
```

The operator was never affected. Only `5.mul(2)` was, which is the spelling nothing in a working
program uses and every core-trait test does.

The family is what gets asked now, so a blanket over the integers is the second implementation it
always was and takes a suffix exactly as `impl Mul[Point, Point] for real` does. What it buys over a
block per width is the narrow count on the left:

```sysl
import sysl.time.*

var d = seconds(3i64)
var w: u8 = 4

print(whole_seconds(w * d))
```

```
12
```

A blanket writing **no** arguments is refused, which is the same predicate the other way round:
`impl[N: Integer] Mul for N` is the implementation the compiler already provides. That one is
reachable only from inside the library — a program's own attempt has no home and is refused a step
earlier — and its refusal used to name the block's type *parameter*, which is the one thing a reader
cannot act on. It names the family.

### Nothing else changes

No syntax moved, nothing was removed, and every program that compiled under 0.0.95 compiles here.
This release only accepts more.

## 0.0.95 — 2026-08-31

Five cards, and four of them are one feature seen from four sides: an **associated type** is a thing
the compiler had never been asked much of, and writing `sysl.crypto` over one properly asked.

### A scalar may sit on the left of an operator

`z * 2.0` compiled and `2.0 * z` did not. The orphan rule looked only at an `impl`'s **subject**, so
`impl Mul[Point, Point] for real` — which names `Point` in the trait's argument list and `real` in
the `for` — had no home in any module, including the one declaring `Point`. It now counts the
trait's arguments too, which is the shape Rust widened to in RFC 2451.

```sysl
struct Point
    x: real
    y: real

impl Mul[real] for Point
    mul(self, k: real) -> Point = Point(self.x * k, self.y * k)

impl Mul[Point, Point] for real
    mul(self, p: Point) -> Point = Point(p.x * self, p.y * self)

val p = Point(1.0, 2.0)

print(p * 2.0, 2.0 * p)
```

`sysl.math.complex` takes the eight blocks this makes writable — `Mul`, `Add`, `Sub` and `Div` at
`real` and at `f32` — so `2.0 * z`, `1.0 - z` and `2.0 / z` all work. An `impl` may not be written
for a bare type parameter, which is why it is two blocks per operator rather than one generic block.

**The built-in keeps everything it had.** `real` is a member of `Mul` whatever anybody writes, and
only the argument list tells the two apart: `2.0 * 3.0` is still the machine's multiply and
`5.0.mul(2.0)` still reaches the member the compiler provides. A block that names nothing local
anywhere — `impl Mul[real, real] for real` — is refused exactly as it was.

**And a derived subtype may now be given an operator at an argument list its base has not.** That
follows from the same rule and had been a documented ceiling: `type Stamp = new i64` with an
`impl Add[Span] for Stamp` is the timeline shape a struct has been able to describe for a while. What
the base *has* still cannot be replaced — `impl Add for Stamp`, writing no arguments, is refused as
before.

Card `0385`.

### Three fixes to associated types

**A bound on an associated type is about that type, not about the implementer.** `type W: Add` asked
whether the *supplied* type could be added to the *implementing* one, because `Add`'s `Rhs = Self`
and `Out = Self` defaults were filled from the wrong side. It went unseen because it needs a bound
that is both on an associated type and parameterised, and the library had none. Card `0383`.

**A struct field may name an associated type of a type whose `impl` appears later.** A non-generic
type is instantiated before any `impl` is hoisted, so `struct Sha256 { inner: Sha[Sha2Narrow] }` —
whose parameter's own field is `[8]C::W` — was told `Sha2Narrow` implemented no trait declaring a
`W`, three lines above the block that plainly supplies it. A generic holder was unaffected. Card
`0384`.

**And such a bound is answered once every `impl` is registered.** It was checked at the block that
supplied it, so an `impl` writing `type W = u32` under a `type W: Word` was refused by a module whose
next file writes `impl Word for u32`. Reordering the two files compiled the same program. Card
`0386`.

### `sysl.crypto` is one hash

SHA-1 and the four SHA-2 digests differ in their compression function and in nothing a caller can
see: they agree on the buffering, on the padding, and on how the digest is published. That agreement
was written twice, because SHA-1's five 32-bit words did not fit a generic whose parameter is the
width. It is now written once, over a trait whose `W` is an **associated** type — which is what lets
two hashes at one width be two implementations.

**The public surface is byte-for-byte what it was**: `sha1` through `sha512`, `hmac1` through
`hmac512`, the five hasher structs and `verify`. Nothing outside the module could name the shared
half before and nothing can now.

Card `0382`.

### Also

The installation page and the org profile now say `brew trust sysl-lang/tap`, which Homebrew
requires and checks *after* the download verifies — so an install without it fails at the end with a
message that reads like a defect in the formula.

## 0.0.94 — 2026-08-30

### A pkg-config module's name is not yours to choose, so the rule stopped pretending it was

A `pkg_config` requirement was held to the same name rule as a `headers` one — letters, digits, `_`
and `-` — and the two are not the same question. A `headers` name is invented by whoever writes the
manifest, so a plain word costs nobody anything. A `.pc` name is what `pkg-config` answers to, and a
great many of the world's libraries file under one with a version in it.

```hocon
requires {
  pkg_config { "yaml-0.1" = "libyaml — brew install libyaml, or Debian's libyaml-dev" }
}
```

libyaml is `yaml-0.1` on macOS, Debian and Arch alike; GLib is `glib-2.0`; GTK 3 is `gtk+-3.0`. So
the refusal was never "pick a better name" — it was "this library cannot be declared", and a binding
to one had to declare a `headers` requirement instead and hand every consumer two flags on a command
line, where every other installed-library binding builds with the bare command.

A `.pc` name may now hold a `.` and a `+`. What still bounds it is the flag rather than taste: the
name is written back to you as `--include-path <name>=<dir>`, so it may hold neither a separator nor
an `=`, and it may not begin with a dot — `./x=y` and `../x=y` are directories exactly as they were.
`--include-path` reads the wider of the two spellings, because the refusal a build stops on tells you
to type one and the flag has to be able to take it.

A name with a `.` in it has to be **quoted** in the manifest, since an unquoted dot is HOCON's path
separator.

### A `for` that is refused now names the member the type does have

`for x in b` over a `Buf` is refused, and the refusal named `sysl.Iterate` and stopped — sending you
to look for a missing implementation when the road was a method on the type in front of you.

```
'for' iterates an integer range, an array, a slice, or a type that implements 'sysl.Iterate', and
sysl.buf.Buf[int] is none of those — what it does have is 'view()', which answers with something a
'for' walks, so 'for x in b.view()' is the loop
```

A container stays deliberately not a cursor, which this does not reverse: a `view()` is valid only
while nothing pushes, and `for x in b.view()` says at the call that a view was taken where an
`impl Iterate` would hide it. `Buf` has no cursor position to advance either, and giving every buffer
one so that the obvious line works would be a word on the type that means nothing outside a loop.

The member is found structurally rather than from a list of names: a nullary, visible member with no
type arguments of its own whose declared result a `for` walks — a slice or an array syntactically, or
anything whose type implements `Iterate`. So it answers for `Buf`'s `view()`, for `walk()` on `Map`,
`List`, `Deque` and `Set`, and for a type you write tomorrow under a name nobody here chose. The
worked line is offered only where the subject is a plain name; a spelling you did not write would be
worse than the sentence alone.

### Every `llvm.` name the compiler emits, declared in one place

The names were literals in nine files, so the only way to find out what sysl asks of LLVM was to grep
and hope. That is exactly what a port to a back end which is not LLVM has to know, and what a target
whose LLVM is missing an intrinsic asks.

`Llvm` is the single declaration site now, and it is documentation that happens to be executable:
each entry carries the prose a standalone page would have held, and every emitter builds its names by
asking for an entry — so a name that is not declared cannot be emitted. A page would have gone stale
the first time an emitter reached for a name, silently; this cannot. The reserved globals are entries
too: `llvm.global_ctors`, `llvm.used`, `llvm.compiler.used` and the `llvm.metadata` section are
equally things a port has to answer.

`LlvmRegistryTests` makes the claim a census rather than a spelling: one program per family, every
`@llvm.` name read out of the IR, each asserted to be one the registry accounts for — plus the
complement, that no callable entry is declared and never emitted.

## 0.0.93 — 2026-08-30

### `dev_dependencies` — a package your tests need and your consumers do not

A manifest could only say a dependency one way, so a package needed solely by a project's own suite
was declared as an ordinary one and fetched and built by everybody who depended on it.

```hocon
dependencies {
  parsing { git = "github.com/sysl-lang/parsing", version = "0.5.0" }
}

dev_dependencies {
  quickjs { git = "github.com/sysl-lang/quickjs", version = "0.1.0" }
}
```

The block has Cargo's `[dev-dependencies]` semantics. `sysl test` on this project resolves them;
`build`, `build-lib` and `build-c` do not; and **anything that depends on this package does not
resolve them at any command** — not fetched, not built, not on the link line. For a binding to a
system library that is the difference between a consumer having to install it first and not knowing
it exists.

A dev dependency may only be imported from a `@tests` file or a `@test` function, checked against
the tree an ordinary build already strips. Without that the mistake is invisible to the person
making it: the suite passes and every consumer is refused at a module that was never fetched for
them. `sysl deps` lists them marked `(dev)`, and a package named by both blocks is refused.

**Scope, stated plainly**: this is ergonomics rather than correctness. Test source never reached a
consumer's compilation — `Tests.stripSource` removes a `@tests` file whole and each `@test` function
before anything else runs. What was wrong is that the *package* was still fetched and built.

### ThreadSanitizer over a sysl program now looks at something

`SYSL_EXTRA_CFLAGS="-fsanitize=thread"` linked TSan's runtime and instrumented nothing, so a run was
green whatever the program did. It is the hole ASan had one release ago, from the same cause: LLVM
instruments only functions carrying the attribute, and sysl emitted `sanitize_address` alone.

The attributes are a table now — `address`, `hwaddress`, `memory`, `thread`. On the two-thread
reduction through `sysl.posix.threads.spawn`, `nm -u` goes from **0** `__tsan_read`/`__tsan_write`
hooks to **4**, and the race is reported and named.

**`-fsanitize=undefined` is deliberately not in the table, and that is worth knowing before you
trust a clean run.** UBSan has no such attribute: clang emits its checks and their
`__ubsan_handle_*` calls during codegen, so there is nothing to switch on afterwards. A UBSan run
over a sysl build covers the C a package vendors and none of the sysl, and no change here could
alter that.

Two smaller fixes came with it: one flag may name several sanitizers (`-fsanitize=address,undefined`
is split rather than matched whole), and the prefix is anchored, so `-fno-sanitize=address` is no
longer read as a request to instrument for it.

### `spawn`'s docstring said sysl could not check what a closure captures

It can, and it could four days before that sentence was written. `sysl.posix.threads.spawn` takes a
`*extern` because the module is `@no_alloc` and a boxed closure needs an allocator — a limit of that
module, not of the language. A package with an allocator may take a `&sync Fn`, whose captures the
compiler checks one at a time. Corrected in place rather than deleted, so a reader who designed
around it can see why.

### The gate: one suite wanted the whole heap

A chunk of nine Native suites had been producing no verdict, and the reading taken at the time was
that nine suites accumulate about 2 GB each and never give it back. Measured one at a time on an
idle machine, `StdCacheBoundTests` alone peaks at **24.0 GB** against a 24 GB ceiling and the other
eight sit between **4.7 and 6.1 GB**.

It runs alone now. The chunk size is unchanged at nine — at 24.0 GB against a 24 GB ceiling, any
chunk holding that suite and anything else is over, so shrinking every chunk in the gate would have
cost an invocation apiece and not fixed it.

## 0.0.92 — 2026-08-30

One card, and it undoes something 0.0.91 should not have shipped.

### A variant re-points a name; it never conjures one

0.0.91 fixed a real problem — a module declaring its own `Ok` could not use `Result` — and fixed it
too widely. The rule consulted the expected type *instead of* asking whether the name resolved at
all, and that turned out to do two things rather than one.

**The first is benign.** A bare variant became reachable with no import, which cannot change the
meaning of anything, since a name that was an error was not meaning something else:

```sysl
import sysl.sync.Atomic          // Ordering is NOT imported

var a = Atomic(0)

print(a.load(Relaxed))
```

`0.0.90` refused that. `0.0.91` compiled it. `0.0.92` refuses it again — sysl already has the leading
dot for a variant the context knows, and the bare form was never meant to work.

**The second is not benign, and is why this release exists.** A name that *already resolved to
something else* was taken:

```sysl
module app
import pal.{Shape, describe}     // pal has enum Shape { Segment(n: int), Dot }

Segment(n: int) -> Shape = Shape.Dot

go() -> string = describe(Segment(1))
```

`0.0.90` prints **`DOT`** — the module's own function. `0.0.91` prints **`VARIANT 1`**, having
constructed a distant enum's variant instead, with the function never reached and **no diagnostic
either way**. That is 0.0.68's *"a name in call position loses to an enum variant"* reintroduced
across modules rather than within one.

**The rule now keeps two questions apart: reachability is the import system's and ownership is the
expected type's.** The expected type is consulted only where the name already resolves as a variant —
which is exactly the case 0.0.91 was written for, where a module's own `Status.Ok` makes `Ok`
resolvable and only the *owner* was wrong. That case still works:

```sysl
enum Status
    Ok
    Bad

f() -> Result[int, string] = Ok(1)
s() -> Status = Ok
```

**Everything else 0.0.91 shipped is unaffected** — `sysl.slices.copy`, the `Drop` warning,
`entropy_from_os` and the overload tie-break are all unchanged, and no program that compiled under
0.0.90 or 0.0.91 compiles differently now except the one shape above, which was wrong.

#### How it was caught

`sysl.sh`'s `library/sync.md` asserts the `Relaxed` refusal, so `DocsTests` reported *"the page says
the compiler refuses this, and it compiled"* — an unrefused refusal, found by the only suite that
could find it and one release too late, since the site is pinned after the tag by design.

The call-site case was found by probing a question rather than answering it: the first report of this
said the widening was purely additive, on the strength of probes where the shadowed name was a
**local** — which a top-level `val` in an entry file is, and which was never at risk. A function is
not a local, and nine lines settled it the other way.

## 0.0.91 — 2026-08-29

Six cards. One is a library function every caller in the org had been writing by hand, two are what
writing it turned up, and three came out of writing packages against 0.0.90 — including a leak that
was costing 26 GB in a package that had already shipped.

### `sysl.slices` copies a slice, and it is right where the two overlap

```sysl
import sysl.slices.{copy, copy_exact}

val moved = copy(dst, src)          // as much as fits, answering how much
if !copy_exact(dst, src) then ...   // all or nothing
```

Every caller was writing `for i in 0..<src.len do dst[i] = src[i]`, the standard library included —
`Buf.extend`, HMAC's key padding, `cstring`, both of `parse_real`'s scratch fills, the test harness's
name buffer. C has `memcpy`, Rust has `copy_from_slice` and Go has `copy` because the loop is noise at
the call site and slower than the platform's own move.

`copy` answers how much moved, which is what a caller streaming into a buffer wants. `copy_exact`
writes the whole of the source or nothing and answers whether it fitted — a destination *larger* than
the source is a fit, since a buffer sized once and written into in runs is the ordinary caller.

**Both are right where the two slices are views of one array**, which is `memmove`'s guarantee rather
than `memcpy`'s. Two views into one array is an ordinary thing to have and a silently wrong answer is
not worth the nanoseconds.

#### There are two declarations of each, and that is what makes the generic one correct

A call over `[]u8` reaches a declaration that hands the move to `memmove`; every other element type
reaches a generic one that assigns element by element.

That is not an optimization detail. An element carrying a reference count needs the retain and the
release an assignment does — a bitwise move over a `[]string` leaks what the destination held and
hands the source's boxes out twice, and it **prints the right answer while doing so**. The fault
arrives later, if it arrives anywhere anybody is looking. So the fast path is exactly as wide as the
type where it is sound, and it was measured rather than assumed: at the default `-O1` the generic
loop compiles to one `ldrb` and one `strb` per element, because the destination's bounds check
survives inside the body and stops the optimizer recognising the idiom.

**A freestanding target walks the elements instead**, because a bare board is linked with no C
library and there is nothing to call. The declaration and what it promises are the same on every
target; only the cost differs, which is what keeps `sysl.slices` usable where the operating system is
not.

### The third overload tie-break, where nothing is exact

`reference/declarations.md § Overloading`'s third tie-break — a candidate that named its parameters
beats one that was solved for them — was asked only of the candidates the second tie-break found
*exact*. Where the argument reached every candidate by a conversion that set is empty, so the rule was
skipped in precisely the case it exists for:

```sysl
g(x: []u8) -> string = "plain"
g[T](x: []T) -> string = "generic"

var a: [3]u8 = [1, 2, 3]

print(g(a))     // was: 'g' is ambiguous here
```

Two declarations, one fitted signature, and an array that had to be viewed to reach either. It is
what `copy` needed to exist in the shape above, and it is the shape any binding wanting a fast path
for one element type will reach for.

The widening is bounded by the rule the page is emphatic about: candidates that all fitted at
**identical** parameter types took identical routes, so choosing between them ranks nothing but the
declarations. Two fitted at different types stay ambiguous, since telling them apart would be ranking
the conversions. **No call that already resolved can resolve differently** — the widening only ever
turns an ambiguity into an answer.

### A module may name a variant the prelude also names

A module declaring an enum with an `Ok` in it could not use `Result` in that module at all:

```sysl
enum Status
    Ok
    Bad

f() -> Result[int, string] = Ok(1)
```

```
error: variant 'Ok' carries nothing, so it is written as a name on its own — drop the
parentheses and the 1 argument inside them
```

The expected type is on the same line and nothing asked it. `Err`, `Some` and `None` are the same
shape, so the four names every package returns through were four a module could not afford to
declare — found writing `sysl-lang/libpq`, where `PQstatus` answers `CONNECTION_OK`/`CONNECTION_BAD`
and the enum over it wants the name every other language gives it.

**The diagnosis is one layer out from where it reads.** A variant key says which *module*, and a
bare name resolves to this file's own module before the library — so the local key was chosen and the
expected type was never consulted. It is now consulted across modules as well as within one, which is
the rule a variant already followed wherever two enums in one module shared a name.

A bare name still means the module's own where the module's own type is expected, a genuine ambiguity
is still refused naming both enums, and `Status.Ok` still says which for a site whose expected type
is a `Result` and whose meaning is not. Patterns were never affected — the scrutinee says which enum
before a name is looked at.

### A destructor that can never run is now told so

`impl Drop for T` is dead code unless something hands back a `&T`: a destructor fires when a *box's*
strong count reaches zero, so a constructor declared `handle() -> Result[Handle, Error]` leaks the
resource on every call — with every test green and every answer correct.

**It was leaking three shipped packages at once.** Measured over 50,000 iterations: `sysl-lang/brotli`
v0.1.0 leaked a `BrotliEncoderState` per `compress` — **26 GB**, against 4.8 MB once fixed —
`hiredis` a `redisReader` per reader, and `libpq` a `PGconn` per failed connect. No test anywhere
could have caught any of them; the compression, the parsing and the queries are all correct while it
happens.

```
warning: 'make' hands back 'Thing' by value, and 'Thing' has a 'Drop' — a destructor runs when a
box's count reaches zero, so nothing here will ever call it. Return '&Thing' instead, or take the
resource apart before returning it
```

`&T`, `*T`, a slice of them and the `drop` member itself are all silent, each for its own reason.

**This is the compiler's first warning that carries a position**, so a diagnostic now has a
`severity`. Two values and no ordering between them — a warning is not a lesser error, it is a
different claim — and deliberately no *level*: an argument about which warnings are errors is one
this compiler has no reason to have while it has one warning. `reference/memory.md` states the
consequence beside the rule now, which is where somebody writing a binding will meet it.

### `sysl.posix.rand` can be asked for key material

`getentropy(2)` was bound and eight bytes of it given away. `seed_from_os` feeds PCG32, and nothing
in sysl could ask the kernel for a salt, a nonce, an IV, a session id or a token — so a program
wanting sixteen fresh bytes opened `/dev/urandom` by hand, which works, and is a descriptor, an open,
a read and a close for one kernel call.

```sysl
import sysl.posix.rand.entropy_from_os

var salt: [16]u8 = [0; 16]

print(entropy_from_os(salt[..]))
```

A slice longer than 256 bytes is filled by looping — that is `getentropy`'s limit per call, not the
caller's — and it answers a `bool` rather than an `Option`, which is `seed_from_os`'s reasoning turned
round: a caller who cannot seed has a fallback, and one who cannot get key material has none and must
stop.

### A positional variant payload says what to write instead

`Url([]const u8)` is what somebody porting an enum from Rust, Swift, OCaml or Haskell writes first,
and sysl refused it with `')' expected` and the caret on the `[` — which reads as a complaint about
the *type*. The next two moves, `Url([]u8)` and `Url(*u8)`, failed the same way, and a pointer
payload got `'self' expected`, which names the one word that would not have helped.

```
a variant's payload names its fields, as 'Circle(r: real)' — write a name before the type,
as 'name: []const u8'
```

The suggestion is built from the type that was actually read rather than recited, so all seven forms
give one message with their own spelling in it. Not a language change: the reference had already
chosen the named form, and this is one sentence at the point of refusal.

## 0.0.90 — 2026-08-29

Eight cards. One was a heap-corruption bug that had blocked another, and two more were cards
filed against gaps that turned out not to exist.

### A `return` out of a `for` gave the element back twice

An iterating loop keeps what `next` handed over in a temporary region that both edges out of the test
release for themselves — the body on the way in, the exhausted path on the way out — so the region
stays populated across both. `releaseAll`, which a `return` emits, walks *every* region, and so gave
that one back a second time.

It is an over-release, so a program that hit it printed the right answer until the storage was reused
by something else. That made it intermittent and made it look like a defect in whatever happened to
be looping. `break` was never affected.

**What found it in one run is the other half of the fix**: `SYSL_EXTRA_CFLAGS` is spliced into every
clang a build drives, and the emitted IR is marked `sanitize_address` when those flags ask for it.
The attribute is what makes the answer mean something — LLVM instruments only functions carrying it,
and a *frontend* is what normally adds one, so IR handed to clang as a `.ll` links the sanitizer's
runtime and is instrumented nowhere. The first ASan run of the program that provoked this came back
green with the bug live.

### `sysl.fs` walks a directory tree, and copies one

```sysl
for item in walk(root)
    val e = item?

    if e.is_dir() then print(e.path)
```

Five programs across this org and slate had each hand-written a recursion through a directory, and
they differed on three axes: what they did at a leaf, what they did about a directory they could not
read, and whether they descended into everything. An iterator answers all three — the leaf action is
the loop body, the error policy is the caller's because each item is a `Result`, and pruning is
`skip_dir`.

A symbolic link is reported and never followed. `bottom_up()` reverses the order for a caller that
needs a directory after its contents.

`copy_dir_all` is the walk with a leaf action: a directory is merged into, a file is never
overwritten, permissions are carried, and a link is copied as a link. `remove_dir_all` is now the
same walk asked the other way round rather than a recursion of its own.

### A trait may promise to borrow what it is handed

```sysl
trait Sink
    @borrows(bytes)
    put(*self, bytes: []const u8)
```

A call through a trait object is opaque, so escape analysis assumed every argument was kept and a
local array passed through one was promoted to the heap. `sysl.Writer` was the one exception and it
was hardcoded twice. `@borrows` is that promise written down, checked against every implementation —
and `Writer` now declares it, which is the test that the feature is real rather than a second way of
spelling what was already there.

### `sysl add` and `sysl vendor`

```
sysl add github.com/sysl-lang/sdl3
sysl vendor .
```

The package manager could resolve, fetch and report a graph, and could not add to one. `add` writes
the entry at the repository's newest tag or at one you pin, asking `git ls-remote` rather than a
forge's API — so it works for a self-hosted server or a mirror. **The manifest is rewritten one run
of bytes at a time**, so your comments and layout survive it, and the result is read back before it
is written.

`vendor` puts what a project depends on into `vendor/` beside the manifest, which is the machine's
package cache moved into the project: same layout, same resolution, same `sysl.sum`. A project that
has one builds with the network off.

### `wasm32-wasi`, and the first cross target that runs on this machine

sysl built for `wasm32-unknown-unknown` and nothing else in the family — the bare target, with no
libc and no convention for what the host supplies. WASI is a standardised table of imports, wasi-libc
is a real libc built on them, and the new row reaches both through wasi-sdk's clang, found from
`WASI_SDK_PATH`.

```
export WASI_SDK_PATH=~/wasi-sdk-34.0-arm64-macos
sysl build --target wasm32-wasi hello.sysl -o hello.wasm
wasmtime hello.wasm
```

It needs no new capability: a preview1 module has files, a clock, randomness and exit and has no
fork, sockets or threads, which is hosted-but-not-POSIX — the rung Windows already stood on.

Three things had to be right and each would have produced a module that linked and did not run: the
bare row's `-nostdlib` and `--entry=main` are keyed on the *operating system* now rather than the
processor; the entry symbol on WASI is `__main_argc_argv`, because wasm cannot overload on arity and
IR handed over as a `.ll` never went through the frontend that renames it; and `fseek`/`ftell` take
C's `long`, which is pointer-width — 32 bits on wasm32 — where sysl's `long` is 64 everywhere.

`sysl.process` moves to `@requires(posix)`, a correction the new row made visible: it is `fork` and
`execvp` underneath.

### A type's own width is a member of `Bits`

```sysl
low[T: Bits + Shr](v: T, n: u32) -> T = v.reverse_bits() >> T(T.width() - n)
```

`width()` is answered from the receiver's own type the way `zero()` and `one()` are, so a bit
expression can be written in a body that does not know its width. `sysl.crypto.Word` requires `Bits`
and `word_bits()` is gone.

### `raw` means raw

`raw` was an interpolator following Scala's -- escapes off, `${...}` still read. That left one
combination nobody could write: a plain string reads no `${...}` and *does* decode escapes, and
Scala's `raw` does the opposite, so *leave all of it alone* had no spelling.

That combination is the only one a literal carrying another language's source can use, and `${` is
not exotic: shell, Make, Kotlin, Groovy and JavaScript template literals all spell interpolation with
it. So `raw` leaves the interpolator family and becomes a prefix like `c`'s -- no escapes decoded, no
holes read, and a raw block still strips its incidental indentation like any other block.

```sysl
val program = raw"""
    fn main() {
        let re = "\d+\.\d+";
        println!("{}", re);
    }
    """
```

Byte for byte what was written. On one line a `raw"..."` cannot hold a `"` -- the first one ends it,
because there is no escape left to write one with -- which is what the tripled form is for.

Nothing in the org used a `raw` literal, so this breaks nobody.

### A changelog

`CHANGELOG.md` at the repository root, generated from every release body by `./changelog.py`.

## 0.0.89 — 2026-08-29

### An identifier may be written in any script

A name begins with `_` or anything Unicode calls a letter, and continues with that or a digit — so
`año`, `área`, `Círculo`, `μ`, `Москва` and `名前` are ordinary names. It is a readability change
rather than an internationalization one: a language whose identifiers are ASCII asks everybody who
does not think in English to transliterate their own vocabulary, and the words that suffer are the
domain ones. Go, Java, Scala and C# all draw the line here.

Two edges, both stated on `reference/lexical.md § Identifiers` rather than left to be found:

- **No normalization.** A precomposed `é` and an `e` with a combining acute are two names. That is
  UAX #31's rule deliberately refused — a declaration is what a caller has to spell, and folding the
  two would make a name that looks right refuse to resolve for a reason nothing on the screen shows.
- **BMP only.** A letter above U+FFFF is refused. Every living script's letters are inside it, CJK
  Unified Ideographs included; what is outside is the historic scripts and the CJK extension planes.

A bare name and its backtick-quoted form are one name at one symbol, so what quoting is still *for*
narrows to punctuation, spaces and the words the grammar has taken for itself.

### Three fixes that were live in 0.0.88, and none of them needed an accent

Making the grammar Unicode found them; every one is reachable in 0.0.88 through a quoted name.

- A **quoted method name** emitted IR that does not parse — `define i32 @Point.is ok(...)`.
- A **quoted enum variant** crashed the compiler outright, `NoSuchElementException` out of the
  analyzer.
- A **module segment** was carried into a symbol unescaped, which only the new grammar could reach.

All three are one mistake: the name encoding was applied where a *key* is built rather than where IR
text is written, and a key is read back as a name in several places. It is now two functions with two
jobs — one marks the module separator, the other makes a name legal at the emitter — and every symbol
this compiler has ever emitted keeps the spelling it had.

### RISC-V follows LLVM 23, and the ABI oracle asks one compiler

A small aggregate returned or passed in one register is named by its own width — `i8`, `i24`, `i32`,
`i40` — where it was named by the register's. An indirect argument states its alignment. Both are
what clang 23 does; **neither changes where a value travels**, so a caller built by either generation
interoperates with a callee built by the other.

The suite that checks this against clang now asks **one chosen compiler** for every target rather
than the first one that happens to carry each back end, and refuses to run against an LLVM older than
the convention sysl lowers to. A vendor clang and a Homebrew clang answering for different halves of
the registry is how a toolchain upgrade came to read as a one-architecture defect.

### A `c const` can read a float from LLVM 23

`c const` asks clang for the value of a macro and reads it back out of the IR, and LLVM 23 changed
how it spells one: a float constant is now `f0x3FF0000000000000` rather than `0x…`, and the
non-finite ones are `+inf`, `-inf` and `+qnan`. A binding that read a floating-point constant out of
a header got nothing on a machine with the new toolchain. Found by CI rather than by a test, which is
the sort of thing CI is for.

### The syntax grammar knows what a word is

sysl's TextMate grammar is matched by Oniguruma, whose `\b` derived its word set from ASCII — so
`síif` highlighted `if` as a keyword and `Árbol` had no boundary in front of it to match at. Fixed
upstream (`oniguruma` 0.0.5, `highlighter` 0.0.11, `juicer` 0.4.2), and the grammar's own character
classes widened to match the language: an identifier is `\p{L}` and `\p{N}` now, not `[A-Za-z0-9]`.

A grammar whose patterns fail to compile is *dropped* by the loader and reported only in a warnings
list, so a grammar that highlights nothing loads as cleanly as one that works. The tests assert that
list is empty in both repositories.

## 0.0.88 — 2026-08-28

Five cards, all in the standard library — the compiler itself is unchanged since 0.0.87.

### Sockets — `sysl.posix.net`

Blocking TCP, and the names a host and a port resolve to. `resolve`, `resolve_passive`, `socket`,
and on a socket: `bind`, `listen`, `accept`, `connect`, `send`, `send_all`, `recv`, `shutdown`,
`close`, `local`, `read_timeout`, `write_timeout`, `reuse_address`.

Files in, subprocesses in, clock in, threads in, network out was a line no language in sysl's
position draws. Sockets are a syscall surface like `open` and `fork`, not a third-party library like
SQLite, and every other syscall surface was already in the library.

**It mirrors POSIX and is deliberately not a `TcpStream`.** `sysl.posix.*` is where an API keeps its
own shape — `sysl.posix.tty` is `termios` presented as `termios` — and top level is where sysl
invents a portable abstraction. A portable `sysl.net` covering Windows comes later, over this. That
order is the point rather than a compromise: POSIX made every decision this module needs forty years
ago, so mirroring it cannot guess wrong, while what a *portable* address or error should be are
guesses that want consumers before a freeze.

Blocking only, which is the line Rust's `std::net` draws and for the same reason — the event loop is
`sysl-lang/libuv`, outside. A timeout is what keeps "returns when the work is done" from meaning
never, and `timed_out(e)` recognises one.

UDP, multicast, unix sockets, non-blocking mode and every socket option beyond the timeouts and
`reuse_address` are deliberately out. All additive, so leaving them out costs a later release
nothing.

### The platform in expression position — `os()` and `cpu()`

`#if macos` gates *lines*, before the lexer, so a program could already define a different `val` per
platform — and could not compare a platform, pass one to a function, or `match` on one. `os()` and
`cpu()` are that half: simple enums, folded, so a program compiled for one target carries one
constant and a freestanding target pays nothing.

```sysl
val leaf = os() match
    MacOS -> "Library/Caches"
    Windows -> "AppData"
    _ -> ".cache"
```

`Os` is `MacOS`, `Linux`, `Windows`, `Freestanding`, `Android`; `Cpu` is `Aarch64`, `X86_64`,
`Riscv64`, `Riscv32`, `Thumb`, `Wasm32`, `X86`, `Craft`. Both derive `Display`. The two vocabularies
are the same words on purpose, and a new operating system in the registry with no branch fails to
build for its own target rather than answering something else.

### Where a directory belongs — `sysl.fs`

`home_dir`, `cache_dir`, `config_dir` and `data_dir`, each `Option[string]`:
`~/Library/Caches` on macOS, `%LOCALAPPDATA%` on Windows, `$XDG_CACHE_HOME` or `~/.cache` elsewhere,
and so on for the other three.

They are `Option` because a machine may genuinely not say where its user lives; they name a
directory and do not make one; and a program appends its own leaf, because a library that guessed it
would be guessing the program's identity.

### A capture may collect standard error — `sysl.process`

`capture(..., stderr = true)` puts the child's other stream in `Output.err`. Without it a tool that
shells out learns that a child failed and not why — the reason went to the terminal, where a person
may not be looking and a program cannot read it at all.

`err` is an `Option[string]`: `None` is "not collected" and `Some("")` is "collected and empty", and
a tool reporting a failure has to tell those apart. The second stream goes through a second file, on
the same mechanism, for the same reason the first does — two pipes is where a deadlock gets *easier*
to reach.

### A heap on a machine with no libc

`requires { heap = true }` on a freestanding target is refused by sysl with a sentence, rather than
by the linker with an undefined `malloc`.

### Also

`sysl.time.checked_date` refuses a date the calendar does not have, where `date_at` keeps its
arithmetic. `display_real_shortest` renders a `real` at the shortest precision that reads back
equal — `sysl-lang/json` v0.1.2 drops its own copy of that loop in favour of it.

## 0.0.87 — 2026-08-28

**a closure in a val, a tests file's own capabilities, and three limits that were never there**

Nine cards. One is a language change, four are diagnostics, and three of the nine turned out to be
reports about limits that were never there.

### A closure that only reads its captures may be bound with `val`

A closure captures **by value**, so it carries its own copy and one that writes a capture writes
through the closure itself. That is why `Fn::call` takes `*self` — and it was being charged to every
closure whether or not it wrote anything, so `val inc = (x: int) -> x + 1` could not be called.

```sysl
val inc = (x: int) -> x + 1
val double = (x: int) -> x * 2

print(inc(41), double(21))
```

A closure that *does* write a capture is refused exactly as before, with the same message. The
compiler asks the typed tree once, at lowering, whether a closure's body reaches its own environment.

**What decided it was the freestanding case.** `val g: &Fn(int) -> int` was the existing way to bind
one to an immutable name, and a `&T` is a **box** — refused under `capabilities { heap = false }`. So
on a target with no allocator there had been no way to do this at all.

### A `@tests` file's own capability clause governs its tests

`@requires`/`@no_os` on a `@tests` file may take back what the module gave up, because a module's
clause is a promise about what **ships** and scaffolding does not ship. `@needs(...)` was reading the
*module's* clause for every declaration, so a module that gave up `os` could not test itself against a
real filesystem. It now reads the tests' clause for scaffolding, exactly as the allocator check
already did, and the diagnostic names the file that declared the narrowing rather than always naming
the module.

Only the module's half moves. A test reaching `@needs(os)` is still refused on a target that has no
OS, whatever its file says.

### `sysl emit-llvm` and `sysl prove` read headers like every other command

Both failed with `'uv.h' file not found` on a project `sysl build` compiled. The guard around the
pkg-config probe asked *"does this command link?"* where the include half needs *"does this command
read a header?"* — a `c const` block is evaluated by running the C compiler over the file's
`@include`s during **analysis**, which both do.

`prove` was the worse half: it branched into a function that took no search paths at all, so
`--include-path` was *ignored* rather than insufficient. It is handed them now.

This matters most on `emit-llvm`, which is what somebody reaches for to *diagnose* a build.

### A `.c` that nothing will compile is named

A directory holding C and no sysl is not a module, so its files belong to no compilation. That rule
is deliberate — otherwise a `build/` directory or a vendored library would be swept in — and it said
nothing at all: no line at the skip, and then a link error naming a symbol. It is a warning now, with
the one-file remedy in the sentence.

### `main() -> int` says how a program does choose a status

The refusal said what a signature may not do and stopped, which sent somebody looking for a language
feature that has been built all along. `exit(code)` is in the core module, prints nothing, and is now
named where the question is asked.

### `sysl.text.to_upper` says that it is ASCII

`to_upper` and `to_lower` on a `string` are general names over a per-character call into `Ascii`, so
`to_upper("héllo")` is `HéLLO`. Total rather than wrong — text outside the range comes back
unchanged — but the caller who did nothing and got it deserves the sentence, and now has it at the
function and on the page.

A Unicode case table stays out of the standard library, measured rather than estimated: the *simple*
mapping alone is about 1,357 range entries, roughly 16KB, against the 499 entries `sysl.text.width`
already carries — in a module every freestanding program links, because it is what places a
diagnostic's caret. It belongs in a package, which is the line `sysl.math.complex` already sits on.

### Two things about building sysl itself

`release-linux.yml` reads the declared version **whole**. It ran it through a numeric-only pattern
that strips a pre-release suffix, so a `-alpha` tag was compared against a truncation of itself, could
never match, and both jobs failed with no Linux tarball attached at all.

The gate learns a third kind of group. `ConditionalTests`' chunk announced an out-of-memory and was
retried alone on all three full gates of 0.0.86 — the evidence a single retry does not give. It was
put in `HEAVY` first and that was wrong: `HEAVY` isolates *and* serializes, and at one agent this
suite runs past fourteen minutes, where inside an ordinary chunk it finishes because its tests spread
across three. Its cost is retention in a shared agent, not a need to run alone in time. So there is
an `ALONE` set now — a chunk of one, at the ordinary chunk settings — and `run-gate.sh` gives each
`HEAVY` suite a group of its own, which is what `gate-groups.py` has always said they need and not
what it did.

## 0.0.86 — 2026-08-28

**ten cards: become, sysl.path, the fs working set, a build cache, and a zero fill that stopped costing its own size**

Ten cards, and two of them are new to the language.

### `become` — a call that replaces the frame

`@tailrec` recognizes a function's calls to **itself** and lowers them as a jump. A chain of calls
between *different* functions cannot be an optimization: it is a loop only if every call in it is
eliminated, so one that is not is an immediate stack overflow rather than a slowdown.

`become f(…)` is `return f(…)` with the jump guaranteed — LLVM's `musttail`, which LLVM verifies. Ten
million mutual calls at `-O0`, which is the level where nothing is eliminated by luck. The callee may
be chosen while the program runs, which is what threaded dispatch needs.

It is a **soft word**: `become` goes on being an ordinary variable and an ordinary function name.

### `sysl.path` — path handling that requires nothing

`join`, `join_all`, `parent`, `file_name`, `extension`, `stem`, `is_absolute`, `normalize`,
`relative_to`, `components`. A module of its own rather than part of `sysl.fs`, because a requirement
is module-wide and one `getcwd` beside `join` would cost every OS-free program the whole of paths.

`normalize` is lexical and `sysl.fs.canonicalize` asks the filesystem; each says what the other does
and why confusing them is a security bug.

### `sysl.fs` gains the working set it was missing

`metadata` and `link_metadata` over a `Meta`, `set_permissions`, symbolic and hard links,
`canonicalize`, `make_dir_all`, `remove_dir_all`, `copy_file`, `truncate` on a path and on an open
file, `current_dir` / `set_current_dir`, and `make_temp_dir`.

### `sysl run` and `sysl test` keep what they built

The second run of an unchanged program builds nothing. Measured on a 9,600-line module with three git
dependencies: **9.98s to 2.81s**, and most of what is left is the JVM starting. `SYSL_NO_CACHE` turns
it off for work on the compiler itself.

### A package may carry an example

`examples/` at a project root is not part of the tree, and `sysl run <pkg>/examples/demo` compiles
against the package with nothing on the command line.

### A zero-filled array no longer costs its own size to compile

`[0; N]` on module storage was written out element by element, where `zeroinitializer` says the same
thing in one word. The binary was never affected — LLVM folds an all-zero global straight into a
zerofill section — so the whole of the cost was in compiling: at 16 MiB the module was **100 MB of
LLVM IR and is now 53 KB**, and the build went from 12.6 s to the 2 s everything else takes.

Reported from `slate`, whose collected heap is exactly that declaration and which had been capped at
16 MiB rather than 64 to keep its build tolerable.

### And four smaller things

- **An empty `Map` holds no table until the first insert.** A profile of an interpreter that gave
  every block its own scope put 27% of its running time in constructing and destroying maps nothing
  was ever read out of.
- **A member may carry the annotations that are about a parameter** — `@crossing`, `@reads`,
  `@writes`. `Channel[T]`'s four transfers are methods now.
- **An escaping array literal is given storage of its own**, so a `...T` trait member works through a
  trait object.
- A file-private helper declared in two `@tests` files of one module is scaffolding in both.

### Compatibility

**No package in the org is affected.** The one API change — `Channel[T]`'s transfers becoming methods
— has no consumer outside the standard library, checked; no package has a root `examples/`; and
`Map.capacity()` answering 0 for a map nothing has been put in is asserted nowhere.

**`sysl.fs.remove_dir_all` is POSIX-only**, which is new in the sense that the function is new. It
needs to know whether an entry is a symbolic link without following it, and that reading is POSIX;
`make_dir_all` beside it is portable. Nothing is losing a call it had.

## 0.0.85 — 2026-08-27

**a shift's count, a declaration's capabilities, the checked variadic, and `?` through `From`**

### sysl 0.0.85

Stabilization batch two: six compiler cards, three findings fixed on the way, and twenty design
cards closed. Everything here is a **widening** — every program that compiled against 0.0.84
compiles against this.

### A shift takes a count

`x << n` asks for `x` shifted `n` places, so `n`'s width has nothing to do with `x`'s. The right
operand may be any integer type and the result is the shifted value's own. That is C's reading,
Rust's, Java's, Go's and Scala's; the cast a program used to write for the compiler's benefit was
noise in exactly the code — a hash, a bitset, a decoder — where widths are already being juggled.

```sysl
var x: u32 = 1
var n: u8 = 5

print(x << n)
```

A count wider than the value is clamped before it is narrowed, so 256 places on a byte is a full
shift rather than a shift by nothing. Six casts in `library/` were noise the moment this landed.

### `@needs(...)` — a declaration may name what reaching it needs

A module is a coarse unit for a capability: `@requires(heap)` on `sysl` would say something false
about most of it. `@needs(heap)`, `@needs(os, posix)` goes above one **declaration**, and the cost is
charged to whoever reaches it — checked at the call, in the caller's module.

**The declaration it exists for is `extern`.** Every other declaration has a body the compiler reads,
so an `extern` was the one route by which a module that had given up an environment capability could
reach `open()`.

### The checked variadic

`xs: ...T` collects a call's trailing arguments into a `[]const T`; `...&Display` is the same
parameter at a trait object, which is what a checked `print`-alike takes. `xs...` at a call hands an
existing slice through.

```sysl
show(xs: ...&Display) -> unit
    for x in xs
        print(x)

show(1, "hi", true)
```

The array a call packs is laid out **where the call is written**, so a variadic call costs no
allocator and a module that gave one up may still make one. The other side of that is the one
limitation worth knowing: a **trait member** declared `...T` is ordinary under static dispatch and is
refused through a trait object, since a trait object may hold on to what it is given and the array is
the caller's frame.

### `?` converts through `From`

`trait From[T]` joins the core library, and a `?` across two error types finds the conversion instead
of refusing:

```sysl
impl From[Io] for Fault
    from(value: Io) -> Fault = Fault.Disk(value)

read(ok: bool) -> Result[int, Fault]
    val n = open(ok)?         // open answers Result[int, Io]

    Ok(n * 2)
```

The parameter is the source and `Self` is the destination, which is the direction that lets a type
accept conversions from types it does not own.

### Running out of stack now says so

A program that recursed without bound died with status 139 and **zero bytes** of output — not even
the lines it had already printed. A hosted program now installs a `sigaltstack` and a handler that
tells an overflow from a wild pointer, and flushes what the program had written:

```
sysl: this program has overflowed its stack -- a recursion with no base case, or a walk over a
structure that contains itself
```

A freestanding target installs nothing, having no signals to catch.

### `Channel[T]`

A bounded queue two threads hand values across, in `sysl.posix.threads`, over storage the caller
supplies — so it needs no allocator. It is the first thing in the library whose **element** type is
held to the rule about what may leave a concurrency domain.

### An empty `Buf` no longer allocates

**A consequence rather than a feature, and the better half of the change below.** `buf()` is
`Buf([], 0)`, and an empty view is now the zero value of what a view is made of — no elements, no
length, nobody owning them. So building an empty `Buf` reaches no allocator at all, and an
allocator-free module may hold one, which is what a function answering "no results" wants.

Growing one still needs an allocator and always will. What moved is where the refusal lands: it now
names the `push` rather than the construction, because the construction is honestly free.

### Also

- **An empty slice literal needs no allocator.** `val e: []const int = []` is `{null, null, 0}` and
  is ordinary in a `@no_alloc` module, where it used to be refused.
- **A sliced array literal takes its element type from the view asked for**, so
  `[1, "hi", true][..]` at a `[]const &Display` is three erasures.

## 0.0.84 — 2026-08-27

### sysl 0.0.84

Eight cards from one decided batch: a flag that was advertised and did not exist, a
namespace rule that defeated the modifier it belonged to, a manifest that ignored
what it did not understand, and the reading and error surfaces the standard library
was missing.

### The language

**A file-private name is scoped to its file.** `private` restricted the *reach* of a name
without restricting the *namespace*, so two files of one module could not each declare a
`Limit` — which defeats what file-privacy is for, since the reason to keep a helper to its
file is that its name is a local matter. Rust, C and Go all scope the name as well as the
reach. A private name against a sibling's **public** one of that spelling is still a
duplicate, and so is a second declaration inside one file.

It is a widening: nothing that compiled before stops compiling.

### The library

**`Option` and `Result` gained the transforming combinators**, decided as a set rather than
grown one at a time: `map`, `and_then`, `or_else` and `unwrap_or_else` on both, `filter` and
`ok_or` on the option, `map_err`, `ok` and `err` on the result. `and_then` is `?` written as
an expression, for the places `?` cannot go; `ok_or` and `ok`/`err` are the crossings between
the two enums.

`?` still does not convert between error types. `map_err` is how two layers are joined.

**`sysl.io` gained the whole-stream reads**: `read_all`, `read_all_text` and `read_exact`. A
`Reader` answers whatever one call could fill, and a short answer is not the end of anything —
so *"read once"* and *"fill this"* were different requests and only the first was expressible.
`Lines` was already the buffered reader; this is what was actually missing.

`read_all_text` has no panicking twin, deliberately: a whole stream is what arrives off a wire
or out of a file somebody else wrote, so the caller is told rather than stopped.

**Two accidental exports are gone.** `floor_div` and `floor_mod` were reachable as library API
while every caller is inside the civil-calendar file that declares them.

### The toolchain

**`--cc` names the clang to build with**, as `--ar` already named the archiver. Three
diagnostics had been telling readers to use it for months while nothing parsed it. It reaches
every place sysl runs clang — the link, a package's carried C, the `c const` probe, and the
standard module's own rebuild, which is the one a flag threaded through the ordinary road would
have silently stopped at.

**An unknown `package.hocon` key is reported and ignored** rather than silently skipped.
Whatever this release's parser does with a key it does not know is the compatibility floor every
later manifest has to clear: refusing would mean no key could ever be added without breaking
released compilers, and ignoring silently is a manifest that looks configured and is not. A
dependency's unknown keys are reported with the package root in front of them, which is the case
a manifest cannot report for itself.

Inside `dependencies` and `allocator` an unknown key is still refused — those are a closed
vocabulary, and somebody who wrote `versoin` believes they have pinned a version.

### The gate

`GATE: GREEN` now names its own scope, and the doc suites run as one more group: sixteen
`DocCliTests` pinning the `sysl doc` exit codes, plus `SlugConformanceTests`. They are discovered
from sbt rather than listed. JS stays out, blocked on a linker that has never linked the bundle,
and the verdict line says so.

### Two claims that were doubted and are now pinned

A linked `.syslib` **is** read by the capability and escape walks: a `no alloc` program calling
into a library that allocates is refused and told what it reached, and an array whose view escapes
through a library function is promoted. Four tests say so, each with a control that fails if the
walk were merely being conservative.

## 0.0.83 — 2026-08-27

Six defects, and four of them are one sentence about **views and large values** seen from four
directions. Nothing here is a new feature; everything here is something that was refused, or crashed,
and should not have been.

### A `[]const T` reaches a block written for `[]T` (card 0301)

`impl[T: Display] Display for []T` supplies `Display` to a `[]const T` — both views share the one
shape key, deliberately — and then could not be **called** on one, because the block's `self` is
spelled `[]T`. A bound that is satisfied and a member that cannot be called is the defect, and it bit
hardest on `[]const u8`, which is what `.bytes` answers and what every text routine takes.

The block is now made real at whichever view the receiver had, so a member that only reads is simply
callable and one that writes through `self` is refused **in its own body** — which is where a reader
can act on it, and what `TraitLookup.shapeOwners` had promised in its own comment all along.

### The two views of one slice meet at the read-only one (card 0303)

`c == a[..]` compiled and `a[..] == c` did not, for the same two values. Wherever a pair of types has
to become one — a pair of operands, an `if`'s branches, a `match`'s arms, a loop's `break` values —
one side's type was taken and the other re-read at it, so which side happened to be read-only decided
whether the form compiled. They meet at the read-only view, which is the one either side converts to.

Reached only where one side is already read-only, and what they meet at may not be written through.

### `?` in a function whose result is large (card 0304)

An early return out of a function returning through an `sret` out-parameter emitted a **direct `ret`**
of the value, out of a function the ABI had already made `void` — so clang refused the compiler's own
IR, naming a temporary file the driver deletes and naming `void`. The analysis passed all the way to
clang, so a package could be type-correct, reviewed and unbuildable.

Where it bites is bindings: a `?` on a status code followed by `Ok(<something big>)` is the shape of
every constructor over caller-placed storage.

### Rendering a value past the indirect boundary (card 0305)

A struct large enough to cross calls through memory was handed to its own `display` as a first-class
value, putting the struct's first word where the callee reads an address. `print(x)` went through an
ordinary call and worked; `str(x)` and an interpolation build the string first and did not, so a type
rendered all through development and died the first time it was put in a message. There was no
workaround at the `impl` site either, so the rule as it stood was that a type over the threshold could
not implement `Display` at all.

### A `string` carries no view out of a frame (card 0266)

A call answering a `string` inherited its arguments' views, because a `string` is a view in the
layout. It is not one to escape analysis: `str_cast` copies into storage of its own, a literal is
static, and nothing else makes one. The refusal landed on a temporary array handed to something taking
a `[]const u8` and answering a `string` — `hex_string(sha3_256(msg))`, so both hashing packages met
it — and read as arbitrary, because the *enclosing* function's result decided it.

### An executable is not written over a directory (card 0300)

A project named for its own module directory is the obvious layout, and `ld` answered `open() failed,
errno=21 (Is a directory)` — an errno, from a tool the reader never invoked, naming neither the
package nor the directory. It is refused by name now, and which of the two names to change depends on
where the path came from. `sysl run` never meets it, so a project could carry the collision until the
first time somebody asked for a binary.

## 0.0.82 — 2026-08-26

Five cards, gated green on Native at **10582 / 0**.

- **0293** — `[]T`, `[N]T` and `Buf[T]` are `Eq` where their elements are, and a `Buf` is `Display`. `assert_eq` takes a sequence now and prints both when they differ, where `sysl.slices.equal` answered a bare `bool` and printed neither.
- **0294** — a payload-free variant of a generic enum takes its sibling branch's type, so `if c then Some(e) else None` needs no annotation on the binding.
- **0295** — a variant of the expected type's enum wins over a same-named **type** in call position, not merely a same-named struct. A `type Eval = Result[…]` beside a `StmtKind.Eval` used to read `Eval(x)` as a cast from an integer.
- **0296** — `from_utf8_unchecked` is an ordinary `sysl.text` function over a raw-tier `str_cast`, so it sits beside `from_utf8` where a reader compares them. An `import` naming any built-in form now says so instead of reporting a name that does not exist.
- **0297** — the derivation clause is `derives`, not `deriving`.

#### Breaking

Two of these change existing source.

- `from_utf8_unchecked` **needs an import** — `import sysl.text.from_utf8_unchecked`. It was in scope everywhere as a compiler form.
- `deriving` no longer parses. Write `derives`.
- A program with its own `impl Eq for []MyType` needs `override impl`, exactly as `Display` has always required — the library supplies a blanket now.

`syslui` 0.1.1, `solder` 0.4.1 and `syslui-sdl` 0.1.1 carry the fixes, and every application in the org is built against this release.

## 0.0.82-alpha — 2026-08-26

**alpha**

An **early, ungated build** so work can continue against the four cards below while 0.0.82's Native gate runs. macOS arm64 only. The real 0.0.82 supersedes it.

It is the tree `5374facc`, which passed the JVM suite at 10582/0, plus a version stamp. It has **not** passed the Native gate.

- **0294** — a payload-free variant of a generic enum takes its sibling branch's type, so `if c then Some(e) else None` needs no annotation
- **0295** — a variant of the expected type's enum wins over a same-named *type* in call position, not merely a same-named struct
- **0296** — `from_utf8_unchecked` is an ordinary `sysl.text` function over a raw-tier `str_cast`; **it now needs an import**
- **0297** — the derivation clause is `derives`, not `deriving`
- **0293** — `[]T`, `[N]T` and `Buf[T]` are `Eq` where their elements are, and a `Buf` is `Display`

The last two are **source-breaking**. `syslui`, `solder` and `slate` are fixed for them.

## 0.0.81 — 2026-08-26

**a pointer is aligned by the library, and an over-width shift has an answer**

### Aligning a pointer is a library call

`sysl.slices` gains `align_up` and `is_aligned`, written in sysl with no C beside them, next to
`as_ptr` and `as_mut_ptr` — the pointer surface a binding already reaches for. The module is
`@no_alloc` and freestanding-reachable, which is what a binding needs.

```
val state: *c.Compressor = ptr_cast(align_up(as_mut_ptr(storage), alignof(c.Compressor)))
```

Every binding that places a C struct in storage the caller supplied has to align that storage itself,
and until now each one wrote the arithmetic in C on the grounds that sysl has no pointer-to-integer
cast. It has had one since v0.0.10: `usize(p)` is an ordinary conversion, `ptr_cast` reads the address
back, and `alignof(T)` supplies the boundary.

### A shift at or past the width is defined

Shifting a value by its own width or more now answers what shifting it all the way answers — zero for
`<<` and for an unsigned `>>`, the sign bit for a signed one. Go's rule and Swift's, and it follows
from a call sysl had already made: raw integer arithmetic is total, and `within` is where checking is
opted into.

What it replaces was not a stable wrong answer. `11 >> 64` printed `0`, then `2`, then `8503132480`
across compilations of one source, because LLVM calls an over-width shift poison. The amount is
clamped rather than the shift emitted and discarded, so no poison is built at all: a constant amount
folds, a variable one is a compare and a select, and vectors are bounded lane-wise.

Not masking, which is C's and Java's and would make `x >> 64` be `x`.

### `@no_alloc` is judged by what a module wrote, not by what the program linked

An allocator-free module rendering into a `*Writer` it was handed was judged against every
`impl Writer` in the program, so the same `display` — unchanged, byte for byte — was legal or refused
according to what the *program* imported. No `Display` in `library/` could carry the clause at all.

A dynamic call site is now answered against the tables the module was seen to erase a value into: a
body that put a value behind a trait object has said which implementation it is reaching. It is a
narrowing rather than a hole — a module that erases the value itself is still answerable for what
that reaches.

`sysl.math.complex` carries `@no_alloc` as a result, and `sysl.display`'s promise that rendering
costs no allocation is now true rather than needing to be weakened.

### Linear algebra and the PNG reader leave the tree

`sysl.math.matrix` was a leaf — nothing in `library/sysl` imported it, and it was the only *domain* in
a library otherwise made of the platform and of what all code touches. It is
[`sysl-lang/linalg`](https://github.com/sysl-lang/linalg) v0.1.0 now. `sysl.math.complex` stays: the
complex *type* is a standard-library thing and the algorithms over it are a package's, which is what D,
R, Racket and Chapel do.

`guide/png` is [`sysl-lang/png`](https://github.com/sysl-lang/png) v0.1.0, and `guide/fft` is
[`sysl-lang/fft`](https://github.com/sysl-lang/fft), generic over `F: Float` rather than fixed at
`Complex[real]`.

Seven more guide programs retire and `examples/` is deleted; the reference and the tour on sysl.sh
cover what they covered. The guide set is **two** — `ring` for the constrained-subtype surface and
`slab` for the raw tier, each owning an axis nothing else does.

### `sysl doc`

`-w, --weight <n>` puts the generated API section where somebody chose rather than where the
unweighted default landed. The generated anchors are now checked against the renderer that serves
them — four separate implementations of GitHub's heading rule across two repositories, and nothing had
ever compared any of them. `--site` has coverage for the first time, which is how a stale juicer pin
rendering every anchor dead was found.

## 0.0.80 — 2026-08-26

**a width is measured, not gathered**

**A width is measured, not gathered — so rendering stops allocating.**

A `Display` whose value has more than one part used to build the text first and pad the finished
bytes, because a specifier describes the field the **whole** value occupies: `%12s` on a `Size` pads
the `Size`, not its first field. That much is right and has not changed. What never followed is that
the parts had to be *gathered* — running the same writes into a `Counting` sink answers the same
question, how wide did it come out, and stores nothing.

That shape now reaches everything that had the old one:

- **a derived `Display`** — the ordinary way to give a type a rendering. `Deriving` emitted a fold of
  `+` over `str` of each part; it now writes a file-private renderer straight through to wherever it
  is pointed, and points it at a `Counting` sink first only when `fmt.width > 0`.
- **a tuple**, one level down, over a pack-generic `render_tuple`.
- **`Complex[F]`**, which built three intermediate strings per value and threw two away.
- **`Vector[T]` and `Matrix[T]`** — a vector of `n` cost `2n + 1` strings, and a matrix built a whole
  fresh `Vector` per row through `row(i)` purely in order to print it. An 8×8 matrix of reals cost on
  the order of 150 allocations and eight vectors for output that went straight out again.

An ordinary print now costs one pass, and the sink is built only where a width was asked for.

**One rendering changes, and it is a fix.** A precision now reaches the parts instead of truncating
the finished text. `%.3s` on `Complex(1.0 / 3.0, -2.0 / 3.0)` answered `0.3` — the first three bytes
of a rendering, which is not a number — and answers `0.333-0.667i`. On a vector it answered `(0.` and
answers `(0.333, -0.667)`. That puts `sysl.math` on one rule: **a width is the aggregate's field, a
precision is each number's own.** The containers divide the other way on purpose — `[]T`, `Option`,
`Result`, a tuple and a derived struct hand their parts the neutral specifier, because their parts are
unrelated values rather than the digits of one number. Every width case is byte-identical to before.

**`Counting` is now public.** It was `private[sysl]`, and that is the one API change here: any
`Display` with more than one part faces this question, and one written outside module `sysl` had
nowhere to measure into but a buffer.

The claim is pinned in **IR**, not in output — the output is identical either way, and only the
emitted symbols say whether a string was built. `@sysl.str.concat`, `@sysl.str.int` and
`@sysl.str.from_bytes` are gone from a program that prints a derived value or a tuple, padded or not.

**A `@tests` file states what its own tests need.**

A module's capability clause described the module; its tests were judged by it too, so a suite that
wanted a buffer forced the module to admit an allocator it never used. A `@tests` file now carries its
own clause, and **seven library modules promise what they were already doing** as a result.

**A walk over unvalidated bytes reads Unicode's table, not the lead byte alone.**

`Chars` decided a sequence's length from the lead byte and trusted it. On input nobody had validated
that walks off the end of a well-formed region; the walk now consults the table. ASCII got ~4% faster
in the process, which was not the point but is welcome.

**`sysl doc` — a generated section can sit beside hand-written prose.**

Two changes, both from putting the generated API section on sysl.sh:

- **`slugStyle` is a per-page frontmatter key** rather than a site-wide switch. A generated page needs
  `github` slugs for its symbol index to resolve; turning that on for a whole site rewrote the anchor
  of every existing heading with punctuation in it — measured at **115 of 921** on sysl.sh, which are
  links people have saved. Needs juicer 0.4.1.
- **`--note`** puts a line of Markdown under the index title, so a generated index can point back at
  the prose that explains it. Optional, and its absence changes the output not at all.

**`guide/json` is retired — JSON is a package.** `sysl-lang/json` v0.1.1 is where it lives, and it
builds freestanding.

**Every reference pointer in the tree resolves**, and fourteen did not. `design/` is gone and the
citations that named its chapters now name the reference; `check-pointers.py` keeps them honest.

---

**Known, and open:** a module doing this still cannot declare `@no_alloc`. A call through `*Writer` is
judged against **every** `impl Writer` linked into the compilation, and `sysl.buf`'s `ByteSink`
allocates — so the same module is legal or refused according to what the *program* imports. Both
halves are pinned in `CapabilityClauseTests` beside each other. The question is one of granularity and
is being decided.

## 0.0.79 — 2026-08-25

**`sysl.crypto` — SHA-2 and HMAC in the standard library.**

`guide/sha2` implemented SHA-224, SHA-256, SHA-384 and SHA-512 with HMAC over all four, generic
over a bound so that two word widths share one compression function. That is a library rather than
a demonstration, and `monocypher` — the org's binding for the rest of cryptography — has no SHA-256
at all, so where it was it filled no hole.

```sysl
import sysl.crypto.{sha256, hmac256, verify}
import sysl.encoding.hex_string

print(hex_string(sha256("abc".bytes)))

val tag = hmac256(key, msg)
print(verify(tag, received))
```

Four hashers — `Sha224`, `Sha256`, `Sha384`, `Sha512` — each `update` as often as you like and then
`finish` into storage the caller owns. Copying a hasher copies the hash in progress, so a common
prefix can be hashed once and finished several ways. Beside them are eight one-shot functions:
`sha224`/`sha256`/`sha384`/`sha512` and `hmac224`/`hmac256`/`hmac384`/`hmac512`.

**Three decisions worth knowing:**

- **A digest is bytes, not text.** The guide ended each of these in `hex_string(…)`, which is right
  for a program whose job is printing test vectors and wrong for a library twice over: it builds a
  string, so it allocates and the module could not be used where there is no allocator; and a caller
  who wants the digest *as a digest* — to compare it, to sign it, to key an HMAC with it — would have
  to parse the text back. Rendering composes instead, `hex_string(sha256(data))`, so the module
  imports nothing to do it. The whole library builds for `thumb-freestanding` with it in place, which
  is what that argument was about.
- **The generic stays inside the module.** A public `Sha[T]` would drag its bound public with it, and
  that bound declares `bits`, `rounds` and `k` — names general enough that the standard library
  claiming them collides with the next program that wants one. So the public surface is four ordinary
  types. That two widths share one algorithm is how SHA-2 is written, not something a caller should
  have to know.
- **`verify` compares a tag; `==` should not.** `==` stops at the first difference, so a wrong first
  byte is rejected fractionally sooner than a right one — measurable, and it turns forging a tag from
  a search over every value into a search over one byte at a time. `verify` reads every byte,
  accumulates the differences and asks once at the end. What the module can honestly claim is written
  into it: the code reads every byte and branches on none of them, and a codegen test pins that shape
  against the compiler's own output. What sysl cannot state is that the *machine* runs it in constant
  time — there is no annotation for it and no check — so the comment says where the guarantee stops
  and names the instruments that go further.

**A cycle through a type argument is finite whichever of its types was declared first.**

`Buf[Node]` reaching `Node` is not containment, and the reference has said so since 0.0.75 — the
edge is the `[]T` inside `Buf`, and whether a generic holds its parameter by value is the generic's
own business. The rule held for a type reaching itself and not for two reaching each other:

```sysl
struct A
    xs: Buf[B]

struct B
    a: A
```

was refused with *"type 'A' contains itself, so it has no finite size"*, and writing `B` first made
the same pair compile. An enum had no such escape, since the enum is the type holding the `Buf` in
either order — which is what a syntax tree looks like, and what stopped one being written.

Two further holes in the same walk came out of the fix, and both let an unbounded type through where
0.0.78 refuses it: it was checking only the **first** in-progress type it reached, so field order
decided the answer; and a raised type-argument depth was excusing everything inside an argument list,
a type's reference to *itself* included. A type reached through an argument list entered after it is
now left alone, and one reaching itself in that same position is condemned. Every new shape is pinned
in both declaration orders, since the order is what used to decide.

**Also in this release**

- Two diagnostics stop naming private traits: one offered a private trait as the bound a type
  parameter was missing, the other advised an import that is refused. Both now filter by visibility,
  and where every candidate is out of reach the message says the member is an implementation detail
  instead of naming it.
- Tuple parts are walked as a by-value edge, in both directions — `Buf[(string, Json)]` is finite and
  `Wrap[(int, B)]` over a `B` holding an `A` by value is not.

**Documentation**: [`sysl.sh/library/crypto/`](https://sysl.sh/library/crypto/).

## 0.0.78 — 2026-08-24

**sysl.process**

**`sysl.process` — starting another program and waiting for it.**

Until now there was no way to run a child process anywhere in sysl. Nothing in the standard library
bound `system`, `popen`, `posix_spawn`, `fork`/`execv` or `waitpid`, so a command-line tool written
in sysl could not drive `git`, a compiler, or anything else. This closes that.

```sysl
import sysl.process.{run, capture}

run("git", ["clone", url, dir])?

val out = capture("git", ["rev-parse", "HEAD"])?
```

Two functions. `run` lets the child share this program's streams, which is what a build or an
install wants; `capture` collects what the child wrote, which is what asking a program a question
wants.

**Three decisions worth knowing:**

- **No shell.** Arguments are a list and are handed over exactly as written, so a filename with a
  space in it is one argument and one with a `;` in it is not a second command. There is no quoting
  to get right because there is nothing to quote for.
- **A missing program is `Err(NotFound)`, not a status.** `Err` means the child could not be
  *started*; a program that ran and exited non-zero is `Ok`, carrying a `Status`. Telling those
  apart is not free — a child that cannot exec has no way to return, so the conventional answer is
  to exit 127, which is indistinguishable from a program that chose to. The child reports the
  failure through a close-on-exec pipe instead.
- **The environment is on the call.** `sysl.env` deliberately has no `set` — it mutates state the
  whole process shares and is not safe against a concurrent read — so variables for a child are
  given at the spawn and applied between the fork and the exec, where the process is
  single-threaded and this program's own environment is untouched.

`Status` has two cases rather than one number, because an exit status is something the program chose
and a signal is something that happened to it. A shell folds them together as `128 + n`, which makes
a program killed by `SIGKILL` indistinguishable from one that deliberately exited 137.

Capture goes through a file rather than a pipe: a parent waiting on a child while the child waits
for the pipe buffer to drain is a deadlock that only appears once the output gets long enough.
Standard error is not captured and goes where this program's does, as a shell's `$(...)` leaves it.

It is `sysl.process` rather than `sysl.posix.process` because starting a child is the same idea on
every hosted system and only the mechanism differs — the POSIX of it is a shim under `__posix__`,
exactly as `sysl.fs` hides `dirent`. `sysl.posix` is for bindings that *are* POSIX and have no
equivalent elsewhere.

**Also in this release**

- `impl Eq for IoError`, which the enum's own documentation asks for: it says the cases are named so
  a caller can match rather than compare a number, and without an `Eq` the only comparison available
  was exactly that. `sysl.fs`'s own test was writing `assert_eq(e.code(), 2)` for want of it.
- A `build-c` archive now fills its computed module storage from a constructor instead of the
  `@export` being refused, so a module linked into a C project may hold a `Buf`, a `Signal` or a
  closure at module scope.

**Documentation**: [`sysl.sh/library/process/`](https://sysl.sh/library/process/).

## 0.0.77 — 2026-08-24

**sysl doc, and a documented standard library**

### `sysl doc` — API reference from the library's own prose

Two releases have been building to this. `0255` made a `/** … */` comment something the compiler
collects and attaches to the declaration below it; this one reads the result.

```
sysl doc                              # the working directory, to docs/api
sysl doc library --out docs/api       # a tree, somewhere else
sysl doc --check                      # write nothing; fail if what is committed is stale
```

**It is a separate binary — `sysl-doc` — reached through the git-style dispatch `0257` added.** Every
major toolchain ships its doc generator that way (scaladoc beside scalac, rustdoc beside rustc), and
the reason is the dependency profile: this links a static site generator, a templating engine, an
asset pipeline and a web server, and none of that belongs inside a systems compiler. The tarball
carries both binaries, so `sysl doc` works as soon as you have installed sysl.

#### It writes Markdown, not a website

Against what scaladoc, javadoc and rustdoc all do, and deliberately.

A `docs/` folder of generated Markdown in a package repository is readable in the GitHub UI with **no
tooling, no hosting and nothing installed**. Rust needs docs.rs — an entire hosted service — to solve
exactly that for crates, and sysl has fifty package repositories and no such service. Generated
Markdown is also diffable, where generated HTML cannot be reviewed in a commit.

**Nothing it emits is HTML, and nothing carries a CSS class.** The `juicerapi` theme that renders
these pages hooks structure and heading ids instead, precisely so the output is not written for one
renderer. A generator reaching for a wrapper `<div>` would produce a file that reads *worse* in the
repository than in a browser, which is backwards.

The unit is a **module**, not a type — scaladoc gives a page to every class because a Java-shaped
program is a tree of them, and sysl's importable thing is a module.

#### It needs nothing analyzed

Every question it answers is answered by the syntax, so a package whose dependencies are missing —
or which does not currently compile — still documents. That matters exactly when somebody is trying
to read their way out of trouble.

### The standard library is documented

**686 doc comments across 86 files.** Every `//` block that sat directly above a declaration is now a
`/** … */`; every other comment stayed `//`, which is the distinction the delimiter was chosen to
make — an implementation note above a declaration stays an implementation note by being written `//`.

Nothing was rewritten. The prose was already in the shape a doc comment wants.

354 of 631 symbols now carry prose, and 15 of the 27 modules carry a summary. The rest are genuinely
undocumented, and mostly right to be: `us_per_milli` and its four siblings are a run of constants
under one shared paragraph, and a sentence each would be noise.

### Also

`juicer` 0.4.0 ships the `juicerapi` theme these pages are rendered with, and publishes
`io.github.edadma:juicer-core` to Maven Central — the site generator without its command line, which
is what `sysl-doc` links.

## 0.0.76 — 2026-08-24

Four cards, all of them found by writing `sysl-lang/parsing` — the first
substantial piece of sysl written against the language in a while, and
therefore the first thing to press on its edges.

### A branch with no type of its own takes its sibling's

`if n == 0 then 1 else n` over a `usize` used to be refused: the `1` fell
to `int` and the two branches disagreed about a width the reader never
wrote. The fix was an annotation on the declaration restating a type the
other branch already gave.

A bare literal now takes the type of the branch beside it — the same
tiering `n + 1` and `0..<xs.len` have always had, reaching the one place
two positions have to agree and did not have it. A `match` is the same
rule over as many arms as the form has.

Two branches that each know what they are still have to agree. That is
what the rule was protecting, and it is untouched: a literal is the
exception because it has no opinion to override.

### A token that opens a block opens one wherever it is written

A bracket pair suspends the off-side rule, which is what lets an argument
list be laid out however reads best. A block opened inside one was the
place that got wrong: the body's own margin is the only thing saying
where the block ends, so a `match` written as an argument was refused
with `newline expected` pointing at its first arm.

```sysl
print(n match
    0 -> "none"
    1 -> "one"
    else "many")

xs.each((x) ->
    val doubled = x * 2

    print(doubled))
```

Both forms work now. `match` and `->` are the two triggers — both,
because one of them would be a rule nobody could state. `then`, `else`,
`do` and the trailing block's `:` are deliberately not among them.

It costs one layout, and this is the whole of it: a function *type*
inside brackets may not be broken immediately after its arrow. Break it
before the arrow or at a comma and it joins as it always did.

### The parse family reads the bytes it is given

`parse_bool`, `parse_int`, `parse_long`, `parse_uint`, `parse_ulong` and
`parse_real` — and the `_base` forms — are each declared twice now, over
a `string` and over a `[]const u8`. What a parser holds is bytes and a
span, and reading a number out of one used to cost a `string` built from
the slice, plus a terminated copy for a float. The digits are ASCII, so
nothing is lost by reading them where they already are.

`parse_real` copies into a buffer on the stack for any text short enough
to fit one — every float anybody writes — and falls back to the heap
only for a longer run.

### `@test` answers for its own argument list

`@test()`, `@test(3)`, `@test("x"` and `@test(should_trap` all reached
one sentence about an annotation marking a function and only a function.
That sentence is about the declaration *below*, and it is true of it, so
a reader was sent to look at a function that was never the problem. The
argument list commits at the `(` now and says what it wanted.

`@test()` stays illegal: an empty argument list is not a shorter way of
saying nothing, it reads as a description that got lost.

### Under it

`io.github.edadma:indentation` 0.0.8 → **0.0.9**, published today. The
block rule lives there: that library had machinery for exactly one such
token, no way to name two, and no test for the feature at all.

## 0.0.75 — 2026-08-24

### A syntax tree can be declared

`reference/memory.md` has always said that a type may reach itself **through an indirection**, and
that the rule is per cycle rather than per field: a cycle is legal as soon as one edge on it is a
pointer, a reference or a slice. A bare `[]Node` field was legal on that rule. The same thing one
generic deep was not:

```sysl
struct NodeSlice
    kids: []NodeSlice        // legal, and always was

struct NodeBuf
    kids: Buf[NodeBuf]       // "type 'NodeBuf' contains itself, so it has no finite size"
```

`Buf[T]` reaches its elements through a `[]T`, so the cycle has an indirection on it and the second
declaration is finite. It was refused because the check fired while resolving the type **argument**,
before anything had looked at how the generic uses its parameter — and that is a question the
argument cannot answer, since `Buf` holds a `[]T` and `struct Wrap[T] { x: T }` holds its `T` by
value, with opposite answers from an identical argument list.

So the question moved to where it can be answered: it stands aside inside a type-argument position,
and every **use** of a substituted parameter asks it instead, at the indirection depth that use is
written at. A generic that holds its parameter by value still contains what it is given —

```sysl
struct Wrap[T]
    x: T

struct Node
    w: Wrap[Node]
```
```
error: type 'Node' contains itself, so it has no finite size
 --> node.sysl:2:8
  |
2 |     x: T
  |        ^
```

— and the caret now lands on the field that holds it rather than on the argument list that mentioned
it, which is the message moving as well as the rule.

**What this unblocks is the shape every syntax tree has**: a node whose children are a growable
sequence of itself, and a data enum whose variant carries a `Buf` of its own enum. Both are ordinary
finite types now, and neither needed a `&` to be written down.

```sysl
enum Json
    Num(v: int)
    Arr(items: Buf[Json])
```

Found writing [sysl-lang/parsing](https://github.com/sysl-lang/parsing), where it was the first
thing the package's own consumer wanted to declare.

## 0.0.74 — 2026-08-24

### Linear algebra is a library module now

`sysl.math.matrix` is 1,289 lines that until this release only a guide program could reach — a
`Vector[T]`, a `Matrix[T]`, Gaussian elimination with partial pivoting, and Bareiss fraction-free
elimination. A program that wants to solve a system copies nothing:

```sysl
import sysl.math.matrix.{Vector, Matrix, solve}

var m = Matrix.of(3, 3, [2.0, 1.0, -1.0, -3.0, -1.0, 2.0, -2.0, 1.0, 2.0])
var b = Vector.of([8.0, -11.0, -3.0])

print(solve(m, b))        // Ok((2, 3, -1))
```

**The one design change on the way in is a second trait, and it closes the guide's sharpest
finding.** `Scalar` requires `Div`, so it requires that division *exists* — it cannot require that
division answers the quotient it was asked for. `int` satisfies it outright and integer `/`
truncates, so Gaussian elimination over the integers reduced nothing and reported a determinant of
60 where the true one is 40, wrapped in `Ok`, with nothing in the bound, the types or the run saying
otherwise. In a guide that wrongness was the exhibit. In a library it is a footgun.

So `Field: Scalar` is a memberless marker: implementing it asserts that `/` yields the true
quotient, exactly or to within rounding. Nothing verifies it and nothing could — exactness is a
property of an operation, not of a type. `real`, `f32` and `Complex[F]` implement it; **`int`
deliberately does not**, and `gauss.sysl` is bounded by it while `bareiss.sysl` is not. A
`Matrix[int]` still adds, multiplies, transposes, prints and compares, and `solve` on one is now a
compile error naming the promise its element type cannot make.

Both types are `Display` and `Eq`, so a `Result[Vector[T], Fail]` prints as a whole — `Ok((2, 3,
-1))`, `Err(the matrix is singular)` — with no formatter written anywhere. The constructors sit on
the types: `Vector.of`, `Vector.zeros`, `Vector.basis`, `Matrix.of`, `Matrix.zeros`,
`Matrix.identity`.

### An arrow may name an associated type, and a call reads its arrows by readiness

Four fixes that add up to bare-arrow parameters working where a boxed `&Fn(…)` already did.

**A projection in an arrow.** `f: S::Item -> N` was refused with a message saying `S` carried no
bound, on a signature where `[S: Walk]` is written and `Walk` declares `type Item`. The same
projection resolved fine as an ordinary parameter type and fine inside `&Fn(S::Item) -> N`, so what
was broken was the arrow's own desugaring.

**One arrow settles the next.** `apply(x: int, first: int -> N, again: N -> N)` could not be called:
`first` takes an `int`, so it is readable, and its result is the whole of what `N` is — but the
second arrow was then told nothing said what it took, by a call whose previous argument had just
said so.

**And order stopped mattering.** Folding each callable's answer forward left to right made the order
two arrow parameters are *declared* in decide whether a call compiles: `f(int -> T, T -> int)`
worked and the same pair reversed did not, against a reference page saying either order answers
alike. Held-back arguments are read by readiness now — each round reads the ones that can be read
and lets what they settle reach the rest — and whatever never becomes readable is still reported
leftmost-first, so the refusal a reader gets is unchanged.

**A `Self` in a member's bounds.** A member inherited from a trait's default keeps the word `Self`
on purpose, and every substitution that fixes its parameters now fixes its bounds alongside them —
which is exactly where bare-arrow sugar puts what the author wrote.

One shape is still refused and says so: a trait's **default body** taking an arrow over
`Self::Item`. Declaring that member on a trait and supplying it from an `impl` both work.

**An explicit type-argument list names what the author declared.** A bare arrow becomes a type
parameter the author never wrote, so it is not one an explicit `f[Int](…)` list has to account for.

### `sysl deps` — the resolved graph, and who asked for each version

Imports became transitive in 0.0.73, so a program can be built against packages its manifest never
names — and nothing said what those were, or why they were at the versions they were at.

```text
sysl deps .
```

prints the resolved graph, marks a coordinate `(raised)` when some claim on it is below what was
selected, and names the claim that raised it. It stops above the target on purpose: a graph is a
property of the manifests rather than of the machine, so a project that cannot be *built* here can
still be inspected. It honours `--lib`, and resolves through the same code a build calls rather than
a copy of it.

The claims come from where resolution holds both halves at once. Recomputing them from the finished
graph can only ever find the claim that *won* — the manifest of a version that was passed over is
dropped on purpose, so the losing floor, which is the whole reason a version is higher than a
manifest says, exists nowhere else.

### The terminal sink flushes one stream, and names it in C

`sysl.posix.tty` flushed *every* stream, because a `#define` is one of the three things only C can
reach and Darwin's `stdout` is a macro rather than a symbol. It is one line of C beside the termios
shim now, so leaving raw mode no longer flushes files the program had nothing to say about.

### Also

The standard library's 94 `NN §M` citations named design chapters that were deleted; every one now
points at the reference page and section a reader can actually open.

## 0.0.73 — 2026-08-24

### Imports are transitive

**A package reached through another is importable.** Naming one dependency brings its own with it,
and theirs, however far down the graph — so a manifest names what a project *takes* rather than
everything it can see:

```hocon
dependencies {
  syslui-sdl { git = "github.com/sysl-lang/syslui-sdl", version = "0.1.0" }
}
```

is now enough to `import sh.sysl.ui`, `import sh.sysl.plutovg` and `import sh.sysl.sdl3`, because the
driver depends on all three. Before this it took four coordinates, three of which said nothing the
build could not work out.

**The forcing case is that a package's public surface is made of its dependencies' types.**
`syslui-sdl` hands out a `&Fn() -> &View` and `View` belongs to the toolkit it is built on, so a
program that could not name the toolkit could not call the one function that package exists for.

**Three levels of precedence, and only a tie inside one of them is an error:**

1. your own modules, and every `--lib` source root's;
2. what your manifest declared;
3. what arrived through something else.

A nearer name wins quietly — a project with its own `json/`, or a dependency it mounted as `json`,
keeps that name however many packages three levels down offer one. **A name nobody asked for never
takes one somebody wrote**, and refusing there would mean a package you have never heard of could
break your own module names. Two packages at the *same* level wanting one name is still refused, as
it always was; naming one of them yourself is what settles an inherited pair.

**A `mount` does not travel.** It is a name its writer chose for their own import lines, so what an
inherited package offers is what its own documentation shows.

The cost is stated rather than hidden: a program may import through a package that never promised to
keep depending on what it depends on, so a library dropping one of its own dependencies can break a
consumer that never named it.

### Two things selection cannot do for you, now said out loud

**Two major versions of one library are named as such:**

```text
'json' and 'json' cannot both be imported — github.com.e.json and github.com.e.json.v2 are two major
versions of one library and both are in this graph, and their modules have the same names
```

Selection cannot fold those together — a major above the first is a different coordinate, which is
the whole point of the suffix — while their module names are identical, because a module's name is
its directory. A `mount` is still the answer where you genuinely want both. It matters more now that
a package reached through another is importable: without this, the message is about two coordinates
you have never typed.

**And a build says so when it selects a version above what your own manifest asked for:**

```text
sysl: note: 'plutovg' is named at 0.2.0 and the build selected 0.2.1, which syslui asks for
```

A note and not a refusal — the higher version is the right answer and the build is correct.
Selection is otherwise silent by design, because it raises floors constantly and a line for each
would be a wall of them about packages nobody typed. What is different here is that the version came
from *your* file, and nothing in it says what actually happened.

## 0.0.72 — 2026-08-23

### `Iterate` names its element with `type Item`

A cursor walks one kind of thing and the choice is the cursor's, so the element is an **associated**
type now rather than a parameter of the trait:

```sysl
trait Iterate
    type Item
    next(*self) -> Option[Self::Item]
```

What that buys is a signature generic over *what* it walks, naming no element at all:

```sysl
count_all[I: Iterate](it: I) -> usize
first_of[I: Iterate](it: I) -> Option[I::Item]
```

`count_all[E, I: Iterate[E]](it: I)` was refused — `E` appeared only in a bound, and a call has
nothing there to solve it from. A body that does want the element reads it off the subject as a
projection, which answers with whatever the implementation chose: an `int` for a range of them, a
`char` for a string's characters.

**This is a breaking change in one direction and one only.** An `impl Iterate[T] for C` becomes
`impl Iterate for C` with `type Item = T` inside it, and every container in the library has moved.
An object type is unchanged in spelling (below). What has no replacement yet is a **bound** naming
the element — `[T: Iterate[int]]` — which is a bound on a projection and is not built.

### An object may fix a trait's associated type

An erased value has forgotten which implementation it came from, so an object over a trait with an
associated type has to say what that type is, or nothing could form one at all:

```sysl
drain(cur: *Iterate[Item = int]) -> usize
count_chars(cur: *Iterate[char]) -> usize
```

The second is the first. A trait with no parameters of its own and exactly one associated type has
only one thing a bare argument could mean, so the two spellings are one type — which is what keeps
`*Iterate[string]` reading the way it always did while the trait underneath it changed shape.

An object still refuses to form where the trait leaves an associated type unfixed, and the refusal
names the ones it left open rather than saying the trait takes no arguments.

### `sysl.math.Magnitude` — how large a value is, when that is not which is greater

```sysl
trait Magnitude
    type Size: Ord
    magnitude(self) -> Self::Size
```

Each type answers in its own terms: `real` and `f32` in their own widths, every integer in itself
through **one blanket block** over `Integer + Zero`, and `Complex[F]` in `F`. Fixing the result to
`real` would be a floating-point commitment made on behalf of element types with no floating point
in them; fixing it to `Self` would leave `Complex` out, which is the one type the trait exists for.

**It is not `Ord` on the values, and that distinction is why it exists.** `Complex` has no `Ord` on
purpose — no order on the plane respects the arithmetic — and yet `|z|` orders complex numbers by
size perfectly well. Gaussian elimination picking its pivot is the worked example: `guide/matrix`
chooses the largest remaining cell in a column, which every element type can answer and `<` cannot.

**The compiler defect it turned up is fixed with it.** A blanket `impl[T: B] Tr for T` whose trait
declares an associated type was refused against the trait's own declaration — conformance resolves
the trait's `Self::Item` with `Self` bound to the subject, and a blanket subject is the block's own
parameter, whose bounds name the family rather than the trait being implemented. So the one block
supplying the associated type was the one place the projection could not be read.

**The name of an associated type is spent for every type the trait covers**: a type has at most one
of any one name, so `Magnitude` declaring `Size` refuses a second `Size` reaching the integers from
anywhere else. That is pinned as a refusal, because the next library trait carrying one is choosing
its name under the same rule.

### Fraction-free elimination, for the element type the bound admits and Gauss is wrong for

`int` meets every requirement `guide/matrix`'s `Scalar` lists, `Div` included, so `impl Scalar for
int` is one line and `Matrix[int]` compiles. Gaussian elimination then runs on it and answers
confidently wrong numbers: integer `/` truncates, so every multiplier becomes zero, no row is ever
reduced, and `det` reports 60 for a matrix whose determinant is 40 while `solve` returns `Ok`
carrying zeros. The guide pins both wrong values exactly rather than asserting an inequality.

`bareiss.sysl` is the answer, and it needs no stronger bound than the one that was already too weak:
every entry it computes is a **minor of the original matrix**, so every division it performs comes
out exact. It divides, but only ever where the quotient is already whole.

**Its import list is the difference between the two algorithms made visible.** `gauss.sysl` imports
`sysl.math.Magnitude`, because partial pivoting has to know which candidate is largest; a
fraction-free pivot is chosen for being non-zero, which `Eq` already answers. This one imports
nothing.

### Where a name was declared, and which construct a cursor is in

Two questions an editor asks, answered off the tree the compiler already builds.

`Locate` takes a place in a file to the constructs covering it, outermost first. It reads a node's
`extent` rather than its anchor, which is the point: a cursor on `print(alpha)`'s closing bracket is
inside the call, where the anchor covers only the callee. The innermost is hover's question; the
chain is an expanding selection's.

`DefinitionIndex` is mostly *derived* — the typed tree already names what each node resolved to and
every node is stamped with its source position, so a walk joins them and no resolution logic moved.
It covers locals, parameters, module `val`/`var`, `extern` variables, and calls including generic
ones. Type names, struct fields and trait members are **not** covered and the tests assert those
absences: they resolve into the shape of a typed node rather than a name it carries.

**A unique name is unique within a function only**, which cost the first attempt: a table keyed on
one collides across a program and answers with whichever function compiled last — a parameter at
line 1 came back declared at line 95 of a library file. The position is carried in the scope entry
now and the reference recorded where a name's binding is actually decided, so shadowing resolves the
way a reader would expect. Recording is off unless asked for, since that function is on the path of
every name in the library.

### A file that does not parse still hands back what it has

`SyslParser.recovered(source, target)` answers a partial tree and a list of diagnostics, so a
half-typed file is something an editor can still be told about. `Analyzer.indexed` answers an
`Indexing` — an optional index beside the problems — instead of an `Either`, which fixes the
ordinary case of a walk that ran to the end, recorded errors, and threw a whole tree away.

**Recovery is a second pass over a file the grammar has already refused, not a change to the grammar
every file goes through.** A recovering block *succeeds* where it used to fail, and that failure is
what hands the position to the next alternative — so recovering on the first pass would silently
re-decide which construct a well-formed file parses as. A file that parses is untouched by any of
this, and `checked` still refuses a recovered file so a build cannot reach a holed tree by accident.

A failed statement is dropped, and so is a **partial** one: `print(2 3)` matches `print` alone, and
keeping that would put a bare `print` in the tree as though the file called it with nothing. A
missing statement is an absence an editor can see; a fabricated one is indistinguishable from a real
one. A line that opened a block takes that block with it, so one bad `if` is one diagnostic rather
than one per line under it.

### A construction over bare literals is as weak as the literals in it

```sysl
same(s, Some(3))    // fine        same(Some(3), s)    // was refused
s == Some(3)        // fine        Some(3) == s        // was refused
```

`Some(3)` is a call, so the check that keeps a literal from settling a type early — which reads the
spelling — never saw it. It went in inference's first round and fixed the type argument on the
strength of an unsuffixed `3`, and the argument that actually knew was then the one the repair pass
tried to re-read. So a symmetric call had a good end and a bad one, in the operator as well as the
call.

A construction over adaptable literals is consulted last now, which takes both orders. **A suffix
stays load-bearing**: `Some(3)` and `Some(3int)` analyze to the same node at the same type, and the
spelling is the only thing that says one of them chose.

### Two compilations may build the artifact cache at once

Starting two builds together against a cold cache failed one of them, with a message about the cache
directory not being empty — which it was not, precisely because the other build had just written the
standard-module artifact into it. Nothing in that reads as a race, and it landed as a failed build on
whichever invocation was second. A `make -j` over a fresh checkout is the ordinary way to meet it.

The directory is made a level at a time now, tolerating one that something else has just made and
still refusing a path where no directory ends up. The same window was open on a fetched package's
directory and on any output directory two builds are pointed at, and all of them are closed.

### Smaller things

- **A tree of source files came in under a thousand lines.** Eight of the compiler's largest split
  along the seams they already had — the typed tree, the statement grammar, expression dispatch, the
  associated-type machinery, `impl` lowering and `Type`'s questions. No behaviour moved with them.
- **`end` markers came off the short blocks.** A block of six lines or fewer, head to marker, says
  where it ends by ending; the marker is for the stretch long enough that a reader has lost the head.
  537 of them went across the library, the guide, the examples and the site. Four stay because they
  are load-bearing: a struct that declares no fields is distinguished from one whose author forgot to
  indent it by nothing else.

### Installing

```
brew update && brew upgrade sysl
```

macOS arm64, Linux x86_64 and Linux arm64 tarballs are attached; the Linux binaries are built on
22.04 images and their measured glibc floor is stated in the release workflow's summary.

## 0.0.71 — 2026-08-23

**sysl.seq, and a range with both ends is a value**

### `sysl.seq` — ten questions a slice or a `Buf` can answer

The library had no `map`, no `filter`, no `fold` and no higher-order sequence operation of any kind:
`sysl.slices` asks *where is this value*, and nothing asked *which of these satisfies this
predicate*. A program wanting every row's name wrote a `Buf`, a loop and a `.view()`.

```sysl
val names = rows.map(r -> r.name)
val ready = rows.filter(r -> r.ready)
val total = amounts.fold(0, (a, x) -> a + x)
```

One trait, implemented for `[]const T` and for `Buf[T]`, so the name is the same whichever a program
is holding — a mutable slice reaches it through the read-only view, and `Buf`'s implementation is the
slice's reached through `view`. It is **eager**, and that is stated rather than discovered: lazy
adapters need a type a trait can name without spelling it, which the language does not have yet.

**And none of it boxes.** A trait's member may take a bare arrow now, so all ten do: a fold over a
slice costs what the hand-written loop costs — one `call ptr @malloc` before, none after, measured
against the boxed spelling written out beside it in the suite rather than asserted.

`generate(n, f)` is the one that makes a sequence rather than transforming one.

### A range with both ends written is a value

`sysl.Range[T]` is three fields — the two bounds and whether the upper one is included — so a range
written outside the four positions that read it as a form is an ordinary value, and it implements
both `Iterate` and `Sequence`:

```sysl
val squares = (0..<n).map(x -> x * x)
if (0..<1_000_000).any(is_interesting) then ...
```

Nothing is materialized to answer a question about one, so that second line costs the predicate and
no storage.

**The four form positions are unchanged, and that is the point** — a `for` header, a slice index, a
`match` pattern and a quantifier each read the bounds directly, so the most ordinary loop in the
language is still a counter and a comparison with no struct, no `Option` a step and no call. A test
asserts the pair: the counted loop calls no cursor, and walking a value does. An **open** end has no
value reading — what an absent bound *is* depends on what is being indexed — so `..`, `lo..` and
`..hi` stay index-only, and the refusal now names all four legal positions instead of two.

**A pre-existing defect the work found.** An inclusive counted loop whose upper bound is the type's
maximum never terminated: `genFor` incremented and then tested, so at `250u8..255u8` the step wrapped
255 to 0 and the walk started again, and `for all i in 0..255u8 do p` hung the same way. There is no
value one greater than the last one to test against, so the test happens before the increment now, on
the inclusive form only.

### An integer has a zero and a one

`[T: Add + Zero]` took a `real`, an `f32` and a `Complex` and did not take an `int`, because every
compiler-provided membership was a method with a receiver and a `zero()` has no value to be lowered
from. `Zero` and `One` are now provided over the integers, which is what let anything generic count —
and it is asked of the type *as written*, so a constrained subtype excluding zero has not got one.

`guide/fft/sum.sysl` declared an `Additive` trait with three `impl` blocks to work around this; the
trait is gone and `sum` is `[T: Add + Zero]` outright.

### Associated types

An associated type is a trait parameter the implementation supplies rather than one written where the
trait is applied — `type Item: Render` in the trait, `type Item = int` in the `impl`, `T::Item` to
read it. What separates it from an ordinary trait parameter is that it is kept out of the key an
implementation is selected by, because it is determined by the subject rather than chosen at the use
site.

`-> some Render` on an `impl` member says the concrete result is whatever the body produced.

A trait declaring one is not erasable, and says so where the object is formed; a type implements at
most one trait declaring an associated type of any one name, since a projection is written without
its trait.

### A trait's member may declare type parameters of its own

No table slot can hold one — it is not a function until a call names its types — but that is a fact
about the member, not about the trait it sits in. The member is left out of the table, the object
still forms and dispatches everything else, and what is refused is the one call the table cannot
carry. That is what makes a `map` on a built-in slice possible at all.

The same work taught `callBounds` to run for a trait's members, so `f: T -> U` on a member is the
bounded type parameter it is everywhere else — called directly, with nothing boxed. Two readings of a
callable argument were wrong and are fixed with it: a literal no longer settles the parameter a
closure is about to be analyzed at (`val n: usize = twice(0, a -> a + 1)` read its closure at `int`
and complained one line below the annotation that answers it), and a closure at a generic member is
now read against what the receiver already settled.

### "this value, with one field different", as an expression

```sysl
val pressed = theme with { bg = ACCENT, fg = WHITE }
```

`base with { ... }` is the two statements a reader writes today — a copy bound to a name, an
assignment per field, the copy — written as one expression, and it desugars to exactly that: a
struct's invariant is rechecked, a private field is refused, a value is converted, and a settable
property runs its setter. What the two-line form cannot do is sit inside a larger expression, which is
the case it was asked for: a style layered at the point it is used.

`with` is a soft word, so it stays a legal identifier — including as a field's own name on both sides
of the clause. A base that is a reference is refused by name, with the spelling that does copy
(`*p with { ... }`), because the desugaring would otherwise reach every other holder of that object.

### A trailing block binds the one value it is passed as `it`

A block filling a callable parameter was always zero-arity, so every handler carrying a value fell
back to the ordinary closure literal — and a widget set ended up with two spellings for one idea,
chosen by whether the callback happened to take an argument.

```sysl
button("Save")
    on_click
        save(it)
```

One value is bound as `it`, none binds nothing, and two or more is refused by a sentence naming the
closure literal. `it` is an ordinary parameter rather than a keyword: unreserved, shadowing and
shadowed. A block at `[]T` is still a list of its lines and binds no name.

### A call chain may be broken before the dot

```sysl
val face = text(label)
    .foreground(WHITE)
    .padding(8)
    .background(ground, radius)
```

A line beginning with a dot continues the line above wherever there is a line above to continue — the
exact dual of the trailing-operator rule, which carries a line because it *cannot* finish an
expression. A block opened by a reserved word keeps its body, since no reserved word can be a
receiver, and a `match` arm written `.Red -> 1` still reaches the implicit-member diagnostic rather
than being joined to the `match`. A trailing dot stays an error on purpose.

### A position is a span, so a diagnostic underlines what is wrong

```
error: 'b' of 'add' is int, but string was given
 --> hello.sysl:7:14
  |
7 | print(add(x, "two"))
  |              ^^^^^
```

Every diagnostic used to put one caret under the first character and leave the reader to work out how
far it ran. A `Pos` carries an exclusive end now, and a node carries both the position a complaint
belongs at and the `extent` of everything its rule read — so a complaint about a whole expression
underlines the whole expression: `print(takes(1 + 2))` puts five carets under `1 + 2` where it used to
put one under the `1`. A parse failure that stopped at a token underlines that token.

### A diagnostic is data, not a paragraph

Every entry point answered `Either[String, ?]` with the diagnostics already assembled into text, so a
caller that wanted to do anything with a mistake other than print it could not. `api.Sysl.check`
answers `List[Problem]` — each with an optional `Span` of plain `String` and `Int` — and empty means
it compiled. Nothing is truncated: the five-diagnostic limit is the renderer's rule, and an editor
marking up a file has the opposite need.

**No existing signature moved.** `compileToLlvm` and its fifteen siblings still answer
`Either[String, ?]`, now as one-line wrappers over structured cores, and the property that says so is
a test: the rendered report over the structured list is byte-for-byte what the string entry point
answers. Two refusals that used to point nowhere — an export clash and a `@tailrec` refusal — now
point at the line the reader has to change.

### The standard-module artifact miscompiled a capturing closure

A capturing closure returning an aggregate read uninitialised memory when a program was compiled
against the **prebuilt** standard module and not when it was compiled against the library's source.
The 0.0.70 artifact defines an instantiation made at a closure and leaves that closure's own body
undefined, so a program linking it called a closure body of its own — a different environment under a
different body, and a wrong answer with nothing to say so. Two mistakes made that pair, both in how a
closure's key is read as a module path; anyone on 0.0.70 who has seen a nonsense value out of a
library call has probably seen this.

### Smaller things

- **A plain alias reaches its base's associated functions.** `type F = real` then `F.zero()` was
  refused with advice to write the line already written.
- **A slice of module storage is not a view of the frame.** `static val table` returned as
  `[]const u8` was refused as an escaping local; module storage is laid down once for the whole run,
  so there is no frame to view.
- **`map` computed its first element twice** — three elements, four calls — and a `fold` allocated
  once where the module's prose claimed twice.
- **A zone by name, with no operating system underneath it.** `sysl.time.tzif` decodes a TZif file
  (RFC 8536) and asks for nothing: a `Zone` is a handful of offsets into the caller's bytes, so a
  board can carry one zone in flash and resolve local time with no filesystem anywhere.
  `sysl.posix.time` gains the half that does need an OS.

### Installing

```
brew update && brew upgrade sysl
```

macOS arm64, Linux x86_64 and Linux arm64 tarballs are attached; the Linux binaries are built on
22.04 images and their measured glibc floor is stated in the release workflow's summary.

## 0.0.70 — 2026-08-22

**a nested function reads anything its block binds**

### A nested function may read anything its block binds

**Helpers can sit above the data they use.** A block's nested functions shared one environment built
where the **first** of them was written, so a binding made after that line was out of reach — of all
of them, however far below it a later one was written:

```sysl
var counter = 0

bump()
    counter += 1

val table: [3]int = [1, 2, 3]

first() -> int = table[0]        -- refused, because `bump` sits above `table`
```

Nothing in that was a fact about the program. A function is not *run* where it is written, so tying
its captures to where a **sibling** happens to sit was the compiler's implementation showing through,
and the natural layout for a script — helpers first, data after — was the one it refused.

The environment is now built after the **last** binding the group reads. One environment for the
block is unchanged, and it is what lets two nested functions call each other in either order.

**The cost is a refusal that is about a real mistake.** Calling one before that point would read a
binding whose initializer has not run, so it stays refused — and all of them are, because there is
one environment and it does not exist yet:

```
error: 'bump' cannot be called here — the nested functions of this block share one environment, and
it is not built until everything they read is bound. 'table' is bound below this call: move the call
below it, or move it above the functions
```

Nothing about this costs anything at run time: same environment, same addresses into the frame. Six
tests that asserted the old restriction are now programs that run.

### The host's own time zone — card 0223

`sysl.time` and `sysl.posix.time` gain `zone.sysl`, so a program that wants the time **where it is
running** asks the host rather than applying a UTC offset by hand. The POSIX half is a shim of its
own, which makes the library's freestanding census five shims rather than four.

### Three more, and a diagnostic that had lost its point

- **A function reaches a bare-arrow parameter across a module boundary.** `resolve(ldt, local_offset)`
  was refused, and a closure literal is what hid it: it is a callable in any scope, so the question
  was being asked in the wrong one. It is decided once in the caller's scope now, and carried.
- **A refusal recovered the sentence that fixes it.** A function name at a parameter that is *not* a
  callable bound falls through to the parameter itself — `spawn(work, &n)` takes a raw `*extern`, so
  there is no bound to read, and without that fallback the refusal lost the sentence naming the `&`.
  That sentence is the only thing a reader of the call needs.
- **A test's needle was three letters wide.** `GhostTests` searched the whole emitted module for
  `big` to say a ghost function had been erased; the module carries the library's type table, and
  `sysl.time`'s new `Resolution.Ambiguous` matched. It asks for `@big(` — what an emitted function
  actually looks like, and what the test meant all along.

## 0.0.69 — 2026-08-22

### A struct and a variant of one name are two names

**A module may declare `struct Segment` and an enum with a `Segment` variant, and now a call can
reach either.** They are in different namespaces — a variant is a value name, a struct is a type
name — so declaring both was always legal, and only a *call* had to choose. It chose the variant
outright, which left the struct impossible to construct by any spelling at all.

The call now chooses the way a bare variant is resolved everywhere else: the expected type decides
where it names the variant's enum, and the struct wins where it does not. The asymmetry is the
argument rather than a preference — a variant keeps `Enum.Variant`, and a struct constructor is named
by the struct's own name and has no second spelling. Type position was never affected.

Found writing the `box2d` binding, whose `ShapeKind` and `JointKind` name five of the shapes the
package also declares as structs.

#### And a private type of that name hid the variant from everybody

Older, and worse. A variant declares no visibility of its own — it follows its enum — so it records
none, and the table that answers "may this name be written here?" is keyed by the qualified name
alone. A `private struct Segment` therefore answered for a public `Kind.Segment`, and every other
file and every importer got `undefined name 'Segment'` with nothing anywhere naming the struct that
caused it. Reach is now asked in the namespace the lookup is in.

### Two diagnostics that were not telling the truth

**A nullary variant given arguments now says what to do.** The advice was *"write it as 'Segment'"*,
printed under a line already reading `Segment(x, y)` — the spelling it had just refused. Both it and
the pattern form now say to drop the parentheses.

**And a capture the block binds too late is no longer told it is "declared below this".** The rule is
real and is unchanged: a block's nested functions share one environment, built where the **first** of
them is written, so what any of them may capture is what the block had bound by that point. But the
message was measured against every name the block binds rather than against where it binds it, so a
use written *underneath* the declaration was told the declaration was underneath *it* — a module
`val` bound at line 155, reported that way against a use at line 827. It now tells the two mistakes
apart and names both ways out: bind it above the group, or make it module storage with `static`.

`reference/modules.md` states that rule for the first time; it had lived only in the compiler.

## 0.0.68 — 2026-08-22

### Identities, and one vector space over any field

**`Zero` and `One` are core traits now.** A generic body cannot spell an identity — `0.0` is a `real`
and nothing else — so an accumulator, an identity matrix and a polynomial evaluation all had nowhere
to start. Two one-member traits with no receiver answer it: `T.zero()` and `T.one()`, in the standard
module beside the operators whose identities they are, so nothing imports them. `Float` requires them
rather than declaring them, and `Complex[F]` implements them. `[T: Add + Zero]` takes a `real`, an
`f32` and a `Complex`; the open `iN`/`uN` families stay outside, because no list of blocks reaches a
width nobody has written yet.

**A generic type may carry a dot product.** `impl[T: Mul + Add] Mul[Vec[T], T] for Vec[T]` — a block
whose trait arguments are built out of its own parameter — was refused while the same block with the
parameter resolved was fine. It says the same thing at every instantiation rather than colliding at
one, and it is the only spelling a dot product has, since trait arguments are positional and reaching
the result means writing the operand.

#### Three inference and instantiation fixes

- **An associated function reached through a substituted type parameter keeps the type's arguments.**
  `var total = T.zero()` at `T = Complex[real]` no longer asks to be annotated: the parameter arrived
  applied, so there is nothing to infer. `Self.f()` inside a generic type's own body is the same case
  and works for the same reason.
- **An `impl`'s stand-ins carry its bounds when one block is read against another.** A bounded generic
  type written as a trait argument no longer makes the *next* block in the file fail its own bound.
- **An instantiation made at a stand-in is no longer confused with another declaration's.** A stand-in
  is its name, so `Buf[T]` under a program's `[T: Anything]` and `Buf[T]` under
  `sysl.container.Heap[T: Ord]` shared one cache entry — and the diagnostic landed in the *library*,
  saying its own `<` needed a bound it had written correctly. Keys now carry what each stand-in
  promises, and are unchanged for every instantiation a value is ever laid out at.

#### Documentation

`guide/matrix` is generic over its element type and runs the same bodies at `real`, `f32` and
`Complex[real]` — four products, Gaussian elimination, determinant, rank and inverse. It writes up
what generalising found: `‖v‖² = v · v` is a fact about the reals, and a pivot needs a *magnitude*
rather than an order, which is the one thing of a field's four the library cannot yet supply.

The reference gains the operator traits' full argument lists, `Zero` and `One`, and the rule that a
type parameter standing for a generic type brings that type's arguments with it.

## 0.0.67 — 2026-08-22

A transparent type alias — `type Name = Existing`, over any type at all.

### `type Name = Existing`

`type` already introduced a constrained subtype. With nothing added to the base
it now introduces an **alias**, which declares no type: `Name` and `Existing` are
one type under two spellings, and a value crosses between them with nothing
emitted and nothing checked, because there are not two things for anything to be
emitted between.

```sysl
type Comparison = *extern(*u8, *u8) -> i32

compare(a: *u8, b: *u8) -> i32 = i32(a[0]) - i32(b[0])

call(f: Comparison, a: *u8, b: *u8) -> i32 = f(a, b)
```

**The base may be anything a type expression can name** — a struct, a pointer, an
array, a callable signature, a generic instantiation — which is the half a
scalar-only reading would miss and is what a C binding needs. A constrained
subtype's base is still a scalar, and that rule is unchanged.

Because an alias is a spelling rather than a type, everything the base has is
reached through it with nothing declared twice: a struct is constructed and read
under the alias's name, a method and a trait implementation on the base need
nothing said about the alias, and a cast or a `::` attribute written on the alias
is the base's — including the base's refusal of one it does not have.

The base is resolved **in the file that wrote the alias**, so `type FRect =
c.FRect` names `c` there and a file that uses `FRect` need not import `c` at all.
That is what lets a binding's pleasant layer name an ABI struct its lower layer
declares.

A chain of aliases resolves to what the last one names, and a cycle is refused
rather than followed.

### Why now

Two reference pages promised the form and the compiler refused it, and
`reference/ffi.md` carried the gap as an open item: *"a signature cannot be named
once"*, with the refusal quoted underneath. Both are now the feature, with
programs.

## 0.0.66 — 2026-08-22

Five containers, a `deriving` clause, a compile-time type identity, and three
pieces of syntax that make a nested structure read like one.

### `sysl.container` — Map, Set, Deque, Heap and an immutable List

One module holding five types. A hash map whose table is **one flat run of
slots**, so a map of a thousand keys is one allocation rather than a thousand and
one; a set over the same table; a double-ended queue; a binary heap; and a
persistent singly-linked list.

`Map[K: Hash + Eq, V]` asks its keys for two memberships, which is what the next
item is for.

### `deriving` — the compiler writes Eq, Ord, Hash and Display

```sysl
struct Size deriving Eq, Ord, Hash, Display
    w: int
    h: int
end Size
```

The clause goes after the name and its type parameters. What it produces is an
ordinary `impl` block — the one a person would have typed — so a derived
implementation is found, checked, dispatched and erased exactly as a written one
is, and a field that cannot do the work says so in the ordinary words.

`Ord` is lexicographic in declaration order, which is the order the fields are
laid out in. A generic type derives **conditionally**: every type parameter gains
the derived trait as a bound, so a `Box[int]` is `Eq` and a `Box` of something
unequatable is not, and neither needs saying. An enum takes the clause too — a
variant renders under its own name, comparison is by variant first and then field
by field, and the variant is mixed into the hash so that two variants carrying
equal payloads are not one key.

The list of four is closed, and a type that orders by one of its fields should
say so rather than derive.

### `T::Id` — a compile-time type identity

A type's identity as a value, so an erased object can be keyed on what it
actually is. `o::Id` asks the same question of a trait object.

### A package may state the oldest compiler it builds with

`package.sysl` carries a compiler floor, and a build below it is refused by name
rather than by whatever the first unrecognised construct happened to be.

### Inference reaches into a closure

Only what a closure *takes* has to be settled before its body is read, so a
closure passed where its parameter types are already known no longer needs them
written out.

### Three pieces of syntax

- **A trailing block argument.** A call may write its last argument as an
  indented block, so a nested structure reads as indentation rather than as
  nested array literals.
- **`&` in front of a value.** A construction, a call result, a literal or an
  arithmetic result may be addressed; the value is written into a hidden local of
  the enclosing scope and what comes back is that slot's address.
- **`for` takes its element apart with a pattern**, rather than binding the whole
  and destructuring in the body.

---

Installation and the full reference are at https://sysl.sh

## 0.0.65 — 2026-08-21

### A `defines` key names its files with braces

```hocon
defines {
  "sh/sysl/miniz/c/{miniz,shim}.c" {
    MINIZ_NO_MALLOC   = true
    TDEFL_LESS_MEMORY = 1
  }
}
```

Several groups multiply out — `"{a,b}/{x,y}.c"` is four files — and expansion happens when the
manifest is read, so nothing further on sees anything but one path and its macros.

**The point is not brevity.** A package whose C shares a configuration was carrying one copy of the
list per file, and every macro in such a list changes a struct's size or deletes a declaration. Two
copies drifting apart is precisely the silent skew a `defines` block exists to prevent, and
duplication is how drift starts. miniz was saying five macros twice.

**There is no `*`, and that is a decision rather than a shortfall.** A wildcard picks up a `.c` added
later without anybody deciding — the same failure by another road, since the new file joins a set
that changes struct layouts. A brace still names every file it configures; it says the shared part
once.

Nesting and empty alternatives are refused, both saying nothing a flat list does not — and so is a
file configured from two blocks, which has no sensible merge: the later would silently win.

## 0.0.64 — 2026-08-21

Two fixes to build machinery, both of the same shape: something keyed by the wrong thing, failing
quietly.

### The `defines` block works when the package root is relative

0.0.63 announced a manifest `defines` block and shipped it half-working. The macros were keyed by
the package root joined to the declared path, and the source walk produces an absolute path whatever
the reader typed — so `sysl test .` in a package's own tree keyed `./sh/…/miniz.c` against a
compilation naming `/private/tmp/…/sh/…/miniz.c`. The same file, and not the same string.

It failed silently, which is the part that mattered: the C compiled under its defaults and only a
`c const` measuring a configured struct noticed. A package built that way reported
`sizeof(tdefl_compressor)` as 319,352 against an object holding 167,800.

Declared paths are now matched against the files the source walk actually returned, within each
package's own tree. Absolutizing the root would have fixed the `.` and not the rest — the walk
canonicalises, so a symlinked path is two strings for one file.

That also makes a bad path detectable, so it is now refused:

```
package.hocon: 'defines."sh/sysl/miniz/c/typo.c"' names a file this package does not carry
```

It is the one mistake the block can make that reading the manifest cannot catch — every other way of
getting it wrong is refused when the file is read, while a path that is merely wrong would compile
perfectly under the library's defaults. It catches naming C the walk never collects, too: a directory
holding no sysl is not a module, so its C is never compiled and configuring it configures nothing.

### An artifact is keyed to the library it was compiled from

`Std.candidates` tries the installed library before the working directory, so an installed sysl run
inside a checkout resolves the *installed* library while being handed the checkout's. `build-lib
--std` named its output with the resolved fingerprint, and so wrote one library's bytes under
another library's key — the single thing a fingerprint in a cache key exists to stop.

Invisible while the compiled tree is a superset of the resolved one, which is what an ordinary
afternoon's editing makes it. An undefined symbol pointing nowhere near the cause, or silently the
wrong implementation, the day it is not.

`build-lib` now names the library it compiled rather than the one it resolved, says so on stderr
where the two differ, and no longer refuses to run on a machine with no installed library at all.

## 0.0.63 — 2026-08-21

Three language changes and the inference work that two of them needed.

### A package says what its own carried C is compiled with

A manifest may carry a `defines` block naming, per C file, the macros that file is compiled with:

```hocon
defines { "geom/shim.c" { DOUBLED = true } }
```

**A `c const` block inherits it**, which is the half that matters. There is no file in the package
for a key to have named a probe after, so a probe reads the headers under the union of what the
carried C in its own directory is compiled with. Every option worth setting is one that changes a
struct's size or deletes a declaration, so a probe reading the *defaults* beside an object built
with the options does not fail — it answers a different number, and nothing in the build says a
word. Inheriting from the directory is what makes a binding's three translation units — the
implementation, the shim and the `c.sysl` — agree by construction.

### A leading dot resolves against the type the context expects

`.Green` is `Colour.Green` with the qualifier left off. It is a lookup rather than a new inference:
the expectation supplies the qualifier and the rest is the resolution the qualified spelling already
gets — a variant, a variant carrying data, or an associated function of an enum, a struct or a
constrained subtype.

Every position that already pushes an expected type down supplies one, so the argument, the
annotated binding, the return, the struct field, the array element, the tuple part, the named
argument, the parameter default and each branch of an `if` or `match` used as a value all work. An
operand takes it from the operand beside it (`c == .Red`), and at a generic parameter it is held
back to the second pass exactly as `null` is.

The type need not be nameable where the dot is written — a variant of another module's enum needs
no import and no path. Visibility is untouched: what the dot leaves off is the spelling, not the
check.

### An `Option` and a `Result` compare, and render

```sysl
val a: Option[usize] = Some(166)

assert_eq(a, Some(166))
print(a, a == None)
```

`Eq` and `Display` for both, bounded on the payloads rather than on the whole — so a
`Result[T, E]` compares exactly when both halves do, and an option of something incomparable is
refused at the comparison, naming the payload. `assert_eq` is bounded over `Eq + Display`, which is
why the rendering is not a companion nicety: without it a comparable option is still refused.

There is no `Ord` and no `Hash`. An order on a `Result` has to decide whether every `Err` sorts
below every `Ok`, which is a statement about what the type means rather than an absence.

### What the two of them needed: an expression settled late is read at the type it turned out to have

Three holes, all of the same shape — something analyzed before its type was known was never looked
at again:

- an argument that cannot be read on its own (`None` at an `Option[T]` parameter) raised where a
  `null` would have waited. It now waits.
- an argument analyzed before its type parameter was solved (`Some(3)` at a `T` the other arguments
  made `Option[usize]`) is read again at the answer.
- an **operand** is not an argument, so neither reached `==`. `assert_eq(got, Some(166))` worked
  while `got == Some(166)` did not; both do now, and `n == None` with it.

Each is conditional on the disagreement, so nothing that resolved before resolves differently.

### Fixed

- **A trait's signature no longer leaks an abstract instantiation.** A `Type.Abstract` is identified
  by its name, so `trait Sink[T]` promising an `Option[T]` registered an `Option` at the *trait's*
  unbounded stand-in — and the definition-time walk of an unrelated `impl[T: Eq] Eq for Option[T]`
  was handed that one and told its body assumed what its own bounds promise. Present since well
  before 0.0.62 and invisible until the standard library gained its first `impl` blocks for
  `Option`.

- **A bare function name at a generic `*extern` parameter** now gets the sharp diagnostic naming the
  `&` rather than the general one, since the parameter is a type by the time the name is read.

### Internal

`design/` is gone. The reference section on sysl.sh is the specification, and every program on it is
compiled by the real compiler on every build.

## 0.0.62 — 2026-08-21

### sysl runs on a phone — `aarch64-android` is a registered target

The row is `aarch64-android` / `aarch64-linux-android24`, and its ABI answers are aarch64-linux's,
**measured rather than inherited**: `AbiAgainstClangTests` agreed with clang on all 21 of its cases
for the new target on the first run.

`Os.Android` rather than reusing `Os.Linux`, and it cost nothing structural — `#if android` and
`__android__/` arrive by the enum existing. Answering `linux` would have answered a question about
glibc with a fact about a kernel: the `-l` names differ, there is no pkg-config, and what a program
reaches for is libandroid and liblog. The API level is in the triple because clang puts it there —
a bare `aarch64-linux-android` defines neither `__ANDROID_API__` nor `__ANDROID_MIN_SDK_VERSION__`,
so the first Bionic header that guards on the level refuses to compile.

**Two of the three things this was expected to need turned out not to exist.** PIC needs no target
field, no flag and no wiring: every global sysl writes is private, so it lowers to a PC-relative pair
and its calls go through a PLT, and a real object's one absolute relocation is in emulated-TLS data
the dynamic linker relocates anyway. The pkg-config probe was already correct for a non-host target.
What a *package's* carried C needed was `-fPIC`, which it now gets when the target asks for it.

The NDK is found from the environment, and an Android build without one is refused rather than
attempted. `ANDROID_HOME` is the name to set — `ANDROID_SDK_ROOT` is deprecated.

**`13 §5` predicted this and was right**, which is the part worth reading: a third POSIX system leaves
`__posix__` directories covering it untouched while `__macos,linux__` directories silently would not.
All three of the standard library's selector directories are `__posix__`, so Android was covered with
nothing edited.

The one thing this does not answer is that having the back end is not having the toolchain: `findClang`
takes a named compiler, three of its diagnostics say to name one with `--cc`, and no such flag exists.
That is a design decision rather than the tail of this work, and `targets.md § Open` carries it.

**This is the first release [`sysl-lang/androidkit`](https://github.com/sysl-lang/androidkit) can be
built with** — the template for an Android app whose native half is sysl, which needed a compiler
that knows this target.

### A property may be written as well as read

`set count(x)` beside `count -> int` makes `p.count = v` run code — the member a cell that has to
notice its own writes could not be written without.

```
struct Counter
    private n: int

    count -> int = self.n

    set count(x)
        self.n = x
        self.log()
end Counter
```

A setter is **not a new kind of member**: it is an ordinary method with a `*self` receiver and one
parameter, filed under a name holding a character a source name cannot. Conformance, visibility,
lowering, the method table and the serialized AST all treat it as the method it is, so nothing
downstream of the parser learns a new concept. The parameter's type is the property's result and is
filled in from it, which is also what refuses a setter with no property to write.

`set` is a soft keyword, and the parameter is named and untyped — as Swift's, Kotlin's and C#'s are,
and unlike all three it is named rather than appearing in the body unwritten.

A write is a **call** rather than a store, exactly as `b[i] = v` is on a container: it yields `unit`,
it cannot be one place of a multiple assignment, and it has no address to take. The compound forms do
work, which is where this parts company with `IndexSet` — a property has no index, so taking the
receiver's address once is the whole of what `count += 1` needs.

A trait may ask for one. A setter mentions `Self` nowhere but its receiver, so object safety is
untouched and both a bound and an erased object write through the extra slot.

**And an accessor may not reach the member it is defining.** `count -> int = self.count` reads
itself, and a `set count` writing `self.count` writes itself; both compiled and ran until the stack
ran out, and both are refused where they are written now. The line is the *member* rather than the
name, so reading `self.count` inside `set count` still works — that calls the getter, which is a
different member and terminates.

The getter half predates settable properties and was quiet for as long as it existed. What makes it
worth refusing now is that a setter makes the shape likely rather than rare: the field and the
property want the same name, and `self.v` against `self.count` is one character.

### A default is read at the type its parameter declares

`12 §2a` says a default is written in the declaration's terms, and a parameter's own type is the
first of those terms. A **free function**'s default was analyzed against it and a **member**'s was
analyzed against nothing, so the two forms of one declaration disagreed about what a default may be:

```
grown(self, by: Option[int] = None) -> int      // refused as a method, accepted as a function
```

It cuts both ways, and the second half is a refusal that was never being made: a member's default
whose type disagrees with its parameter is now reported at the declaration, exactly as a function's
always was.

### A closure literal is a default a parameter may take

The same rule reaches the one kind of parameter that exists for taking a closure, which was the one
whose default could not be one:

```
apply(g: int -> int = y -> y * 2) -> int = g(21)
apply(g: &Fn(int) -> int = y -> y * 2) -> int = g(21)
```

`y` needs no annotation in either spelling, and the placeholder form `= _ * 2` needs none either.

It was three holes wearing one diagnostic. A **generic** declaration analyzed its defaults against
nothing at all — which covers more than it sounds like, since a bare-arrow parameter *is* a bounded
type parameter, so the bound that says what the closure takes was exactly what was withheld. A
default filled at a call arrives **wrapped**, and every shape question at that call — is this a
`ptr_cast`, a branching form, a closure, `null` — answered about the wrapper rather than about what
was written inside it. And under that, a generic call decides which arguments to hold back *before*
it analyzes any of them, so it never reached the second fix at all.

The wrapper being answered for also produced a second, confusing diagnostic beside the true one when
a default was `null`. That is gone.

### An `@export` in a header-less file no longer vanishes

An `@export`ed function in a file with no `module` header that reached module storage made **every**
`@export` in that file disappear — one warning saying the module exports nothing, no error, and an
archive with no entry point.

The mechanism was nowhere near where it looked. A lone top-level `var` in a header-less file is read
as a body's local, because that is what keeps a one-file `var n = 1` meaning what it always has. But
`build-c` emits no `main`, so there is **no body** — and the file was chosen as one anyway, which
turned its functions into nested functions of a body nothing emits, renaming them and dropping the
attribute. A compilation with no beginning now has no body for any file to be.

## 0.0.61 — 2026-08-20

### Code after a divergence no longer emits invalid IR

A `match` arm or an `if` branch that diverges — `exit`, `return` — and then names a value whose
lowering opens a basic block of its own produced a module clang refused outright, with `use of
undefined value`. The registers the value computed were dropped with the closed block while the
blocks reading them were emitted whole.

It needed both ingredients, which is why it lasted: a divergence, and a value that opens a block.
`exit(1)` followed by `[0; 0]` is the shape; the same arm at `string` was always fine, a constant
string opening no block. **`sysl test` was silent on it and `sysl build` was not** — the analyzer has
no objection to make, only the LLVM verifier does, so a project could be green under `sysl test` and
unbuildable.

Fixed for every construct at once rather than for `match`: a label no emitted terminator names now
opens a closed block, so the suppression reaches through. An `if` used as a value, a plain statement
after an `exit`, and a diverging loop body were all affected and are all pinned.

### A selector directory may name a family, or several machines

`__<os>__` could say *this differs between macOS and Linux* and could not say *this needs an
operating system at all*. A shim needing one was therefore written once per hosted system, and the
copies were byte-identical — all four in the standard library were.

A selector now names one or more symbols, comma-separated, and is taken when any of them holds:

```
__macos__            one operating system
__macos,linux__      either of two
__posix__            whichever operating systems POSIX means
__hosted__           any machine with an operating system under it
```

The vocabulary is `#if`'s own, which is what makes this one idea rather than two — `posix` and
`hosted` were already symbols a source line could test and a directory could not name. A processor is
the one thing a selector may not name; that axis is `#if`'s.

Two selectors may both answer — every POSIX machine is hosted — and both are taken. What is refused
is the two of them holding a file of the same name between them.

The standard library's eight shims are four, under `__posix__`.

**Prefer the name that says why over the list that says which.** `__posix__` and `__macos,linux__`
select the same machines today and are not the same claim: add a third POSIX system and the first
covers it untouched.

## 0.0.60 — 2026-08-16

### A sixteen-bit target, and an index that is checked rather than refused

**`craft-freestanding` is CRAFT** — *Compact RISC Architecture For Teaching*, a 16-bit load/store
machine with eight registers, a 64 KiB virtual address space over 20 bits of physical memory, and a
software-managed TLB. It is the first row in the registry that is **not 32- or 64-bit**, and the
first that **no clang can build for**.

Its LLVM back end lives out of tree, so what exists is an `llc` rather than a compiler driver — and
the machine has no libc, no object format and **no linker**, since `craft as` reads one file and
resolves every label inside it. So sysl writes the LLVM and stops:

```
sysl emit-llvm hello.sysl --target craft-freestanding > hello.ll
llc -march=craft hello.ll -o hello.s
craft as hello.s
```

Every other subcommand refuses that target and says so, naming what does work. It is not a target
sysl cannot *lower* for — it is one with nothing for a driver to call. Two consequences follow and
are stated rather than discovered: the standard module is taken **from source** there, because an
artifact is an archive of objects and this machine has neither; and the row records **no C calling
convention**, because there is no C on the far side of any call to disagree with. That is a fact
about the machine rather than a measurement nobody made, and it is the one exception `targets.md`'s
*"measure it against clang"* rule now carries.

#### An index wider than an address is checked, not refused

This is the half that is **not** about CRAFT, and it changes what compiles on every target.

`a[i]` with an index wider than `usize` used to be refused, on the argument that a truncated index is
not the index that was written — `2^64 + 5` narrowed to 64 bits arrives as `5` and would pass on a
six-element array. The argument is right and the conclusion was wrong: the answer is to ask whether
the value fits **before** narrowing it, where every bit of it is still there. Nothing holds more than
`usize` elements, so an index that does not fit names no element — which makes it an ordinary index
out of range and not a program to decline.

So `xs[i]` with a `u128` now works, and traps when the value cannot name an element. It costs one
comparison on that path and **nothing at all** where the index is not wider, which is every ordinary
program on every target shipped before this one.

`07 § Indexing` had specified this all along — *"the index may be any integer type… requiring `usize`
would make `for i in 0..<10 do a[i] …` need a conversion for no benefit"* — and at 64 bits the only
index that ever reached the refusal was a `u128`, so it cost nothing and went unnoticed. On a machine
whose address space is 64 KiB an `int` is wider than an address, and the standard library stopped
compiling in twelve places.

#### A build reports what is wrong with the program

Two fixes to the order the driver does things in, both found by CI on Linux and both invisible
whenever every step succeeds.

The library carries C now, and a shim under a `__<os>__` directory is chosen by the **target's**
system and compiled by the **host's** clang — so a cross-system build asks this machine to compile
another system's C against headers it has not got. That is a limitation rather than a defect; the
defect was that its complaint arrived *first*. `sysl build --target aarch64-macos` on Linux reported
that `library/sysl/fs/__macos__/dirent.c` would not compile, in place of the diagnostic about the
program in front of it, and `sysl test` for a cross target reported the same thing in place of the
refusal it was about to make anyway.

A tree's C is now compiled where each command needs it: a test build below its own refusal, an
ordinary build below the analysis.

## 0.0.59 — 2026-08-15

### Per-OS source directories, and a standard library that can carry C

**A directory named `__<os>__` selects source for one operating system and names nothing.** Its
files belong to the directory that holds it, so a module keeps one name and one API while its
implementation differs underneath:

```
library/sysl/fs/path.sysl              module sysl.fs, on every target
library/sysl/fs/__linux__/dirent.c     that module's C on Linux, absent everywhere else
library/sysl/fs/__macos__/dirent.c     that module's C on macOS
```

The vocabulary is the operating systems a target can have — `__macos__`, `__linux__`, `__windows__`,
`__freestanding__`, the same words `#if linux` uses. Exactly one is true of a target, so at most one
directory is selected at a level: there is no precedence to remember and no tie to break. Files
sitting beside a folder are compiled for every target. Nesting is refused, and a `__x__` naming
nothing the compiler knows is an error rather than a directory that silently selects nothing.

**It exists for the C.** A module that differs by platform can usually say so with `#if` in one file,
but a `.c` cannot carry a sysl attribute and will not take a sysl-shaped name — so the path is the
only place its selector could go. This replaces the filename-suffix scheme `13 §5` had specified and
never grown, which would have selected everything except the one kind of file the feature is for.

**The standard library carries C now, and lists a directory with it.** `sysl.fs.entries` answers the
one thing `sysl.fs` had named as genuinely missing: `readdir` hands back a `struct dirent` whose name
field sits at an offset the platforms disagree about, which is the transcription the module refuses
everywhere else. A four-line shim returns the `char *` and nothing in sysl learns the layout. The
folder is what keeps that file off a target with no directories to list — the library is compiled
whole for every target, so a shim it could not compile everywhere had nowhere to live until now.

```
import sysl.fs.entries

for name in entries("/etc").unwrap().view()
    print(name)
```

`.` and `..` are left out. The order is the filesystem's. A name that is not UTF-8 stops the program,
which is the rule `read_text` already set for a file's contents.

### A type parameter answers `T::Min` and `T::Max`, and is solved to the type that was written

A generic can now ask a type parameter for its bounds, and a written type argument is solved to the
type it names rather than to one inferred past it.

### Upgrading

Nothing here is a breaking change. A tree with no `__<os>__` directory in it compiles exactly as it
did; the axis is opt-in and invisible until a directory has that shape.

## 0.0.58 — 2026-08-15

### Type arguments are written at a call

`id[int](7)` names an instantiation, and so do `Pair[int, real](1, 2.5)`, `x.pick[int](3)`,
`chunk[8]()`, `Maybe[int].Just(1)` and `va_arg[int](ap)`. A **value** argument is written exactly as
a type one is, since the two share a list and a position.

The form was deferred because a type-argument list and a subscript are one grammar — which is true
of the *parser*, and was never true of the compiler: the head is resolved first, and a function is
not a thing that can be indexed. The nearest binding still wins, so a local standing over a
function's name keeps the subscript.

**What earned it is a signature inference cannot reach from either direction.** A width-generic
kernel over three slices — `add[const W: usize](a: []const f32, b: []const f32, out: []f32)` — names
`W` in no parameter and answers `unit`, so nothing at a call could say what `W` is and no annotation
could either. That is the plainest SIMD kernel there is, and it could not be called from anywhere.

Two things stay inferred: a **type pack**, which has no expression spelling, and an **associated
function selected from an applied type** (`Box[int].of(1)`), whose own type parameters and its
type's come out of a single solve. A `[]int`, a `weak T`, a `volatile T`, a `<4>f32` or a callable
written in the brackets is refused by the parser — an annotation reaches all of those.

A call that leaves such a parameter unsaid now names the list rather than asking for an annotation
that cannot exist: *"'W' is in neither the parameters of 'add' nor its result, so nothing in this
call says what it should be — write it out, as 'add[…](…)'"*.

### An operand is a place a vector load's width comes from

`xs.load(i) * by` reads its lane count off `by` now, rather than being refused for having nothing to
read. The operand rule has three tiers: an operand carrying a type of its own is read first, a load
at what that one said, and a bare literal last at whatever the two settled — so
`xs.load(i) * by + 1.0` needs no annotation anywhere.

`out.store(i, xs.load(i))` is still refused, and has to be: a store takes whatever width it is
handed, so every width type-checks and there is nothing to infer.

### Inside — the IR is data

Codegen built LLVM by writing text, so the IR existed only as characters and a second back end would
have had to parse them back. It is a module of case classes now: every instruction, every operand,
every signature and every type, with one printer that writes the text down. `Inst.Raw` and `Val.Raw`
are both gone, which is what says the conversion is complete rather than mostly done.

`sh.sysl.ir` is public API, so the promise it makes is one the version number now covers.

## 0.0.57 — 2026-08-15

### Vectors — `<N>T`

`<4>int` is four lanes of `int`, and it is the one sequence shape that is not storage: it computes
lane-wise and has no address. Arithmetic, bitwise and comparison operators apply per lane; a
comparison yields a mask, and `select` is a method rather than an `if`, because nothing
short-circuits one lane and not another. The reductions fold a vector to a scalar.

The point of it is the last section of the reference page: **one kernel compiled for more than one
register width**, from a lane count that is an ordinary value parameter rather than a separate body
per width. `guide/simd` is the worked example — the loop the program said could not be written.

A vector is **refused at the C boundary** in both directions. Which register it arrives in differs by
target *and* by what the other side was compiled with, so a permitted call would resolve and corrupt
its arguments rather than fail to link. A compound assignment splats its scalar.

### A vector's lanes reach memory

`xs.load(i)` reads a vector's worth of lanes from a slice, `xs.store(i, v)` writes one back. Without
them a width-generic kernel could compute results and had nowhere to put them.

A type that **declares its own `load` or `store`** wins over the builtin pair, so a container with
those names keeps them.

### A binding's `=` introduces an indented block

A multi-line initializer no longer has to be dressed up as a control-flow construct. A `const`'s
value may also sit on the next line.

### An exported struct chooses its C name

`@export` on a struct derived the name its generated C header carried and gave no way to choose it.
The name is now the author's, as a function's symbol already was.

### Inside

Codegen's LLVM types are data rather than characters — one function writes them down, which is the
first half of giving a second back end something to consume.

---

**Install**

```
brew install sysl-lang/tap/sysl
```

macOS arm64, Linux x86_64 and Linux arm64 tarballs are attached. The Linux builds require glibc 2.34
or newer.

## 0.0.56 — 2026-08-15

### A package can name an installed library, and the machine is asked where it is

Building anything against cairo, SDL3 or SQLite meant typing where your machine keeps them — and
typing it correctly, which for the box2d demo meant knowing that cairo's headers are in
`include/cairo` while SDL3's want the directory *above* `SDL3`:

```
sysl run . --link-path /opt/homebrew/lib \
           --include-path cairo=/opt/homebrew/include/cairo \
           --include-path sdl3=/opt/homebrew/include
```

A package can now declare the library instead, and `pkg-config` answers for this machine:

```hocon
requires {
  pkg_config { sdl3 = "SDL3 — brew install sdl3, or Debian's libsdl3-dev" }
}
```

```
sysl run .
```

**One declaration answers both halves.** `--cflags` reaches every C compilation in the tree and
`--libs` reaches the link line — including the `-Wl,-rpath` that decides whether a dynamically-linked
program finds its library at run time, which a hand-written `--link-path` leaves out. The hand-typed
line was also the *minimal* answer rather than the complete one: cairo's real cflags are twelve
include directories across five projects.

`packages.md § 8`'s split is unchanged — the package names the requirement and something else supplies
the path. What differs is that the something else is the machine rather than a person copying its
layout. No code the package supplied is run.

**Three things it deliberately does not do:**

- **Derive the module name.** The sdl3 package writes `@link("SDL3")` and files as `sdl3`, so the link
  name fails on case; and a `headers` requirement name that happened to match some `.pc` file on your
  box would satisfy a requirement nobody answered.
- **Probe for a cross build.** `pkg-config` answers for the machine it runs on, so a target that is not
  this machine is refused by name rather than compiled against your laptop's `/opt/homebrew`.
- **Override your flags.** `--include-path <name>=<dir>` answers this exactly as it answers a header
  requirement and stops the probe, so no build that works today changes what it does.

A machine without `pkg-config` lands exactly where it was, with a refusal that now names what was
looked for. macOS ships none and the libraries do not bring one, so **`brew install sysl` now installs
`pkgconf` with it**.

Asked on every command that compiles C — `run`, `build`, `test`, `build-c` and `build-lib`.

### Also in this release

- A probed search path goes above the objects on the link line rather than after them. `-L` is
  order-sensitive on GNU ld, so a probed path at the end would leave a `@link` directive's `-l`
  unresolvable on a Linux where the library sits outside the default prefix; macOS's `ld64` gathers
  every `-L` before resolving and cannot see the difference.

The packages themselves take the new key in their own releases: an older compiler reads
`requires { pkg_config { … } }` as a capability and refuses it, so cairo, sdl3 and sqlite3 can only
declare it once this compiler is out.

## 0.0.55 — 2026-08-14

Five cards, four of them about the boundary with C. They came out of surveying box2d for a binding,
which is what a real API asked of the language rather than what the compiler's own tests thought to.

### `@export` publishes a C-convention entry, not a rename (0137, 0136, 0140)

**An `@export` used to be a rename**: the definition took the C symbol and kept sysl's own parameter
lowering, so a C caller passed its arguments where C's convention says and the body read them where
sysl's does. For a scalar those coincide and the two agreed by luck; for a struct they do not, and
what arrived was whatever had been in the registers sysl looked at — a silent wrong answer at the one
boundary this compiler cannot check, because the other side of it was compiled by somebody else.

The exported symbol is now a **thunk** with the signature C uses, which reassembles each parameter
into the shape sysl expects and calls the definition. It is `ForeignEmitter` read backwards, so one
classifier answers both directions and they cannot drift.

Two things follow:

- **`&f` on an exported function is that thunk's address**, so a sysl function can now *be* a C
  callback that takes a struct by value. `&f` on an `extern` is admitted too — there was never
  anything to check there, since the callee is C.
- **`sysl test` runs the export check**, which it never did (0140), so an `@export` refusal is no
  longer silent in a package's own loop.

### A `c const` measures a float (0138)

`c const` read integers only, so a float macro was still written down by hand — the practice the
chapter condemns. It reads `f32` and `f64` now. The case that settles it is the macro that is an
expression over other macros: transcribing `0.25f * B2_PI` is not copying a number, it is doing the
arithmetic by hand.

`c type` still refuses a float, and that is not an inconsistency: a typedef is measured for its
width, and `float` and `double` are binary32 and binary64 on every target sysl has.

### A function's address may be named through its module (0139)

`&c.less` was refused while `&less` worked, for all three address forms. And the refusal quoted the
compiler's internal key — telling the reader to write a name with a `$` in it, which nothing in
source may contain. Both halves are fixed, and they close each other: the advice is now typable
*and* true.

### `sysl.posix.time` (0070)

Nothing in the library read a clock, so nothing could be measured. Both of the two the host keeps.

## 0.0.54 — 2026-08-14

Four fixes, three of them found by writing something against the language rather than by reading it.

### A transparent subtype's name converts what its base's name converts — card 0132

`Age(n)` required its operand to have *arrived* at the base already, so the only way into a subtype
was to be holding one of its base's values. `16 §2` already said a transparent subtype **is** its
base; in call position that now reads as: its name converts exactly what the base's name converts,
narrowing by the ordinary rules, with the range checked on the value that arrives.

```sysl
type Age = int within 0..150

var n: usize = 42

print(Age(n))
```

**It matters somewhere other than subtypes.** A `c type` is a transparent subtype of a width the
program is deliberately not told, so `u32(n)` would be one machine's answer written into the source
and there was no portable spelling at all. Anything a program works out — a `sizeof`, a slice's
`len` — is a `usize`, which is a distinct type from whatever C measured, so a binding could *declare*
a measured typedef in a signature and never call it with a number it had computed. `Tick(xs.len)` is
what the FreeRTOS binding needed, and `sh.sysl.freertos` 0.3.0 now spells every kernel type exactly
instead of proving a guess about it with `@assert`.

A **derived** type keeps the stricter rule: `new` is what makes it distinct, so `Meters(n)` on an
`int` is still refused, and an unwritten conversion into a `c type` is still refused too — a silent
narrowing is exactly what breaks on the target whose typedef is sixteen bits.

### The first `impl Drop` in the standard library was emitted into every program — card 0130

A root the program did not write — an export, an interrupt handler, a `@section` definition, a
destructor — counts only where the program reaches its module (`15 §12`). The standard library sat
outside that rule for a mechanical reason rather than a decided one, so a destructor anywhere in
`library/` would have been emitted into every program linking it, including one whose body is
`print(1)`.

Measured rather than assumed: on a freestanding target the program gained the destructor's body
**and** an undefined `fclose`, out of a module whose own header says `requires os` — a capability that
target's configuration says the program cannot reach at all.

### `null` at a generic pointer parameter — card 0131

`null` could not be written where a parameter's type had already been solved by an earlier argument,
which is the shape a callback binding meets constantly: the parameter naming `T` is usually the
function's own. A solved `&T` now answers in its own terms rather than in `null`'s.

### An annotation above a member says what is wrong — card 0129

`#` above a member was refused with *"dedent expected"*, which names nothing. It is answered by the
same sentence a statement-position attribute gets, and three design passages that described the
absence of a rule rather than a rule were rewritten to say where an attribute may stand.

## 0.0.53 — 2026-08-14

### A `c const` may be declared at a transparent subtype of an integer — card 0127

A `c const` held its declared type to a *primitive* integer name, so a transparent constrained type
over one was refused — including a `c type` the C compiler had just measured. That left the two
blocks unable to describe the case they were both built for: a typedef whose width the configuration
decides, and the constants that have to be that width.

```
@include("FreeRTOS.h")

c type
    Tick = "TickType_t"

c const
    forever: Tick = "portMAX_DELAY"
```

`16 §1` already says a transparent subtype *is* its base; this was the one place in the language
where that was not true. A name is now followed against the file's own declarations — a `c type` the
same probe measures, or a `type` whose base reaches an integer. The value is deliberately **not**
cast through the C type: C narrows, so `(uint8_t)800` is `32`, and a constant that should have been
refused would arrive looking like one that fits.

`new` is refused, since reaching a distinct type from its base is a written conversion and a constant
is the value it was written as; a `where` predicate is refused, since a predicate is checked where a
value is *made*; a `within` range **is** checked, while compiling, against the measured number.

The same sentence was wrong in `ConstFolding`, so **a plain `const` at a constrained type compiles
now too**, and its `within` range is a compile-time refusal rather than an `@assert` written
underneath.

### The target's half of the capability rule, asked where it can be answered — card 0126

A library handed over by path was held to the target's capabilities the way the program is, so one
module of it requiring POSIX refused a build on a machine without POSIX **even where nothing named
that module**. The compilation's own modules are now told apart from the ones handed to it; only the
against-the-target check is guarded by that, because whether a clause names a capability at all, and
whether the files of one module agree, are facts about the files that nobody else has checked.

That surfaced the other half: a program compiled for a target whose config says `os = false` and
calling `sysl.fs.exists` **compiled**, and emitted a call to `access`. The chapter says the whole
transitive graph must fit within the target's set, and the target half now reports at the reference
the same way the narrowing half already did. A clause the reader wrote still wins where there is one,
so the existing `no os` diagnostic is unchanged byte for byte.

## 0.0.52 — 2026-08-14

### `@crossing` — where the domain-crossing rule is asked

`06`'s rule about what may cross a concurrency domain has been specification since it was written:
the check was to land with a channel, and a channel is library surface the chapter defers. That left
every scheduler sysl did not write permanently outside the model — FreeRTOS's `xTaskCreate` creates a
domain exactly as `spawn` does, and had no way to say so.

`@crossing(p)` is the annotation a facility writes above the function that hands a value to another
domain, naming the parameters it hands it through. It names parameters the way `@reads` and
`@writes` name module storage, emits no code, and moves no signature — what it adds is a refusal,
made at each call against the argument's type.

```
@crossing(arg)
spawn[T](body: *extern(*T) -> unit, arg: *T) -> Option[Thread]
```

**A `*T` parameter is looked through, and that is the whole of what it buys.** A raw pointer carries
no refcount *of its own*, which says nothing about the object at the far end — and the object at the
far end is what crossed. So the walk asked is the strict one a `&sync T`'s pointee already goes
through, and a plain `&T`, a `weak T`, a `string` or a slice anywhere inside the state is refused:

```
what 'arg' of 'sysl.posix.threads.spawn' points at reaches another concurrency domain, so every
count inside it has to be atomic — but its 'r' reaches a '&Cell', whose count is not. Hold it as a
'&sync Cell' ('06')
```

`sysl.posix.threads.spawn` carries `@crossing(arg)`, so the standard library's own thread API is the
first thing held to it. A package binding FreeRTOS, Zephyr or a scheduler of its own writes the same
line above the wrapper it already has, and its callers get the same refusal.

A channel that genuinely *copies* will want the other half of `06 § Crossing copies` — a heap-backed
view may cross by copying its bytes — and that stays deferred with the channel.

### A `c const` block is not probed on a machine that cannot have its headers

A `c const` or `c type` block is a C compilation, so a file carrying one asks for headers. A library
is built for every target, so without a rule here a library can hold no probe at all: one module
measuring `sizeof(regex_t)` failed every freestanding build of every program, including programs that
never name it, because there is no `<regex.h>` for a bare Cortex-M and no reason there should be.

The probe now skips a file that declares `@requires` on an environment capability the machine cannot
have. The header stays and the declarations go, so the skip is invisible to everybody but the module
that opted out — a program that *does* reach it is still told *"this reaches 'x', which requires
'posix'"* by the ordinary rule, rather than being answered with an undefined name.

The gate asks the **target's inherent** capabilities rather than what the project provides, and that
distinction is the whole of why it works: `package.hocon` defaults every capability to provided, so a
gate reading `provides` would gate nothing. A file that requires nothing is measured wherever it is
built, deliberately — such a file claims to build anywhere, so a header missing there is the file
having mis-stated itself.

### A measured constant is a constant on the source path too

`CProbe.lower` had three callers and only two of them ran it: `Analyzer.analyze` for a program's own
blocks, and `LibraryArtifact.build` on the way into an artifact. `Stdlib.fromSource` — the path that
reads the library **as source** — called neither, so the first `c const` in `library/` made a
measured constant not a constant there. `@assert` over one was refused for not being a constant
expression, and the refusal named a library file the program's author had not written.

The strip comes first, so a `@tests` file is never probed: it is dropped either way, and asking the C
compiler about a file nothing will compile is a clang invocation for nothing. What survives is
lowered identically to the artifact path's, which is what keeps the two ways of reaching the standard
module comparing equal.

### Where a binding that reads a system header belongs

Related, and worth stating because it is a boundary rather than a fix: a library `c const` is fine
when it needs no header the host lacks for the target — `sizeof(long long)` cross-compiles, because
clang knows a target's basic types without a sysroot. It is `@include` of a **system** header that
ends cross-compilation, since a Mac has no sysroot for `x86_64-linux`. So a binding that must read
one belongs in a package, which is what packages are for: a package is built by somebody who has that
target's headers.

### Also

- `design/06` gains **Marking a domain boundary**; `03`, `capabilities.md` and `testing.md` are
  corrected where they described the rule as unchecked or the annotation set as smaller than it is.
- `sysl.sh` documents `@crossing` on the memory, threads and attributes reference pages.

## 0.0.51 — 2026-08-14

A binding can now ask the C compiler what a typedef **is**, not only what it measures — and the
library's POSIX-gated modules have moved under one name that says so.

### `c type` — a typedef spelled exactly, instead of guessed and asserted

`c const` could answer `sizeof(TickType_t)` and nothing turned that into the type of a parameter, so
a binding picked one integer and was right by luck. Getting it wrong is not a size mismatch anything
sees: it is an `extern` declaring a different argument width from the function it names, which links
and then passes garbage in the high half.

```
@include("FreeRTOS.h")

c type
    Tick = "TickType_t"
    Base = "BaseType_t"

extern "vTaskDelay" c_task_delay(ticks: Tick)
```

The measurement is the same probe `c const` uses, so a file writing both blocks asks the C compiler
**once**: `_Generic` over an object of the type says which integer C thinks it is, `sizeof` says how
wide that is here, and one further global says whether plain `char` is signed on this machine.

- an **enum** matches its compatible integer type rather than the default arm, so an enum typedef is
  measurable with no special case;
- a **qualifier** drops out through the lvalue conversion, so `const unsigned short` needs none either;
- a **float, a pointer, a struct and an array** are refused **by name** — each already has a better
  answer in sysl than a same-width integer standing in for it.

What it lowers to is a transparent subtype with neither a range nor a predicate, which is what a
typedef means. `16` still refuses that as a bare alias for anything a person wrote; the declaration
the compiler writes carries a marker to say the C compiler measured it.

**It also fixed a hole nothing else had found:** no `type` was importable at all — measured or
hand-written. `import shape.Meters` was refused with *"'shape' declares no 'Meters'"* while
`shape.Meters` beside it resolved, because the import resolver asked every table a name may be
declared in except the one constrained subtypes live in.

### `sysl.posix` — three modules moved, and the `threads` capability is gone

**This is a breaking change to three import paths.**

| was | is |
|---|---|
| `sysl.thread` | `sysl.posix.threads` |
| `sysl.term.tty` | `sysl.posix.tty` |
| `sysl.rand.sys` | `sysl.posix.rand` |

The library's three modules that need POSIX were scattered under portable parents, each split one
directory down so the parent stayed reachable from a freestanding target. The split was right and the
placement said nothing: a reader looking for what a bare-metal target does *not* get had to open
every module and read its clause.

`sysl.fs` stays where it is. Its clause is `os`, not `posix` — files exist on operating systems that
are not POSIX — and that is what keeps `sysl.posix` meaning exactly one thing.

**The `threads` capability is deleted rather than renamed.** It gated one module, and what that
module is built on is pthreads, so `posix` is the whole claim. A fourth capability asserted that the
compiler tracks whether a scheduler exists, which it does not: a target running FreeRTOS or Zephyr
has threads and no POSIX, never reaches this module, and binds its own kernel as a package. So
`@no_threads` and `capabilities { threads = false }` are now unknown-capability refusals, and the
refusal names where the module went.

Enforcement is unchanged and is still the clause's — the gate reads what a module *requires*, never
a path, so every file under `sysl.posix` still writes `requires { posix }` for itself.

## 0.0.50 — 2026-08-14

A target row now states what floating-point unit the machine has, rather than leaving it to whichever
clang is installed — which two releases had been doing, with different results on different machines.

**If you build for a Cortex-M33 or a Cortex-M4F, this release changes what you get, and 0.0.48 and
0.0.49 were wrong about it.**

- **On Linux**, `thumb-freestanding-softfp` produced a silently unit-less image: no `__ARM_FP`, an
  `__aeabi_fmul` call for every multiply, and a CMSIS header checking `__FPU_PRESENT` passing where
  it should have refused. The clang shipped by apt.llvm.org defaults the bare `thumbv8m.main-none-eabi`
  triple to `-mfloat-abi=soft`, and a row that named no unit inherited that.
- **On macOS**, the same rows claimed a *double-precision* unit the M33 has not got, so an `f64`
  multiply lowered to a `vmul.f64` — an instruction that faults on the board.

A Thumb row now says both halves on every clang command line sysl builds for it — the link, the
object, a package's C and a `c const` probe alike:

| the row | what is said |
|---|---|
| no unit | `-mfloat-abi=soft -mfpu=none` |
| a unit, arguments in core registers | `-mfloat-abi=softfp -mfpu=<unit>` |
| a unit, arguments in it | `-mfloat-abi=hard -mfpu=<unit>` |

The convention is said with the unit because `-mfloat-abi=soft` **overrides** `-mfpu` outright, so
naming the unit alone left the row exactly as it was on the toolchain that had the bug.

The units named are the silicon's: `fpv5-sp-d16` for the Cortex-M33 rows and `fpv4-sp-d16` for
Armv7E-M. The rows that were already right are unmoved — assembly byte-identical with the flags and
without them.

Also in this release: the `markdown` dependency moves to 0.4.7.

## 0.0.49 — 2026-08-13

### sysl installs on Linux

`brew install sysl-lang/tap/sysl` now works on x86_64 and arm64 Linux as well as Apple silicon. Both
tarballs are built on Linux, because Scala Native does not cross-compile and there is no Linux
machine — so this is the first artifact a release ships that Actions makes rather than checks, and
the exception is written down where the rule is.

They are built on the 22.04 images rather than 24.04, so the glibc floor is low enough to include
Debian 12; the floor is then measured from the binary itself and reported per build, since nothing
else announces it. The gate for these artifacts is the formula's own `test` block applied to the
tarball before it is uploaded — extract, `--version`, assert the library travelled, then compile and
run a program and pin the whole of its stdout. Not the Native suite, whose memory bounds are measured
for an 18-core machine with 64 GB.

### Bitfields

A `@packed` struct whose fields all lower to integers, one of them narrower than a byte, is now one
unsigned integer: the fields are bit ranges of it, filled from the least significant bit upward in
declaration order, straddling byte boundaries freely. C leaves both of those implementation-defined,
which is why portable embedded C avoids bitfields; stating them is the point.

The rule is over the integer's value and never over memory bytes, so the container is emitted as an
`iM` and how it reaches memory is the target's own byte order for an integer of that width.

**And a bitfield may be `volatile`**, which is the hardware register the feature was asked for and
was the one thing it could not describe when the layout first landed. The qualifier reads off the
field and means a volatile access of the *container*: one volatile load to read a range, one load and
one store to write one, which is what C does with `volatile unsigned x : 3`. It is a property of the
container rather than of one range, since every field of a bitfield struct is bits of one word. What
that costs is stated rather than diagnosed — a write is a read-modify-write, so a register whose
reads have side effects is corrupted by one, and nothing in the language describes a register's read
semantics for a diagnostic to consult.

A simple enum may be volatile too: it is its underlying integer and so is the single load the
qualifier promises, and it is the spelling a mode field wants. A data enum is refused, now with a
message that says why rather than through the catch-all about scalars.

Two `CAbi` fixes came out of putting the shapes to the clang oracle, and the first is older than the
feature: System V classifies an aggregate with a member off its own alignment as MEMORY whatever its
size, and sysl was passing every `@packed` struct in registers.

### A dependency contributes no roots unless the program reaches it

A dependency's source root is compiled whole rather than by what the program imports, and
`Reachability.entryPoints` had four unconditional kinds — an `@export`, an interrupt handler, a
`@section` definition and a destructor. So an unimported module's contribution reached every
consumer: an exported symbol in the archive, a placed definition marked `used` so nothing downstream
removes it, a handler in the consumer's vector table, a destructor nothing can call.

What that cost was a package carrying its own program. A test application's `@export("main")` reached
every consumer and the two `main`s fought at the link, which is why `sysl-lang/zephyr`'s suite had to
go in a second repository.

One predicate over all four, and it is about **provenance** rather than about kinds: told per kind it
becomes a rule about which attributes appear together, since a function that is also placed, or also
a handler, was kept for that reason and landed its C symbol anyway. A module the compilation is
itself building keeps the unconditional meaning — a `build-c` or `build-lib` compilation has no entry
point, so a conditional rule there would prune the artifact to nothing.

What it costs is that a consumer wanting a package's handler or placed definition names its module.
An `import` is enough, and a vector table slot and a RAM-resident `.ramfunc` region are the scarcest
things on the parts those attributes exist for.

### `weave` and `tangle` replace `doc`

`sysl doc` wrote Markdown, which a `.lsysl` file already is — so the command was close to the
identity map and left the rendering to whatever the reader happened to point at it. It is now
`weave`, it writes HTML, and `tangle` sits beside it printing the program with the prose stripped.

The renderer turned out to be a setting rather than a product: the one thing the format gives up is
that the program is marked by an indent and an indented block carries no language, and `markdown`'s
`indentedCodeLanguage` puts exactly that back. So the source reaches the renderer verbatim and
`Doc.scala`'s 140 lines of re-fencing and fence widths are deleted rather than ported.

A woven document carries its own styling and colours its code with the grammar the site uses, so a
reader needs no JavaScript for the program. Mathematics is the exception and is set by KaTeX from a
linked script, which is a real cost: a woven document needs the network for its equations and nothing
else.

### Type arguments where a function's address is taken

A C interface that calls back fixes the callback's signature to untyped pointers, so a trampoline
over `*u8` mentions its own type parameter nowhere and there is nothing for the expected type to
solve. `&f[T]` and `&f[A, B]` now name the instantiation directly. This is the one position in the
language that takes written type arguments; a call head still refuses them, with its message
unchanged.

The grammar gives `&f[T]` the same shape as `&xs[i]` and the analyzer is what tells them apart. More
than one thing in the brackets was a parse error until now, so `&f[A, B]` reads with no name
resolution at all.

`guide/qsort` is rewritten to the interface the trampoline's C actually asks for, with the casts
inside where a C programmer writes them — which removes the `ptr_cast` that used to stand at the
call.

### An array is a view of itself

A `[N]T` standing where a `[]T` is wanted is `a[..]`, applied by the position instead of written by
hand. One arm in `coerce`, building the same `TSlice` node the explicit form builds and by the same
rules: read-only storage gives a read-only view, and a writable one is refused over storage a
struct's invariant reads.

Nothing was added to the TAST, so escape, frames, purity and aliasing reach a coerced array by the
arm they already reached an explicit one by. Being in `coerce`, it covers every position that
converts rather than arguments alone — a binding, a return, module storage, a default.

The guide, the examples and the library's own sources lose 122 of those brackets.

### `@no_alloc` is a promise about a module's own conduct

A generic has no conduct until somebody else picks a type, and charging the monomorphized instance
put the caller's choice on the declaring module: an allocator-free library was refused for what a
program's `impl` does.

The instance is charged to nobody now, and the generic is charged from the body the definition-time
pass analyzed — the one form in which the two can be told apart, since a call through a bound names
the trait's member while a call to something concrete keeps the name it always had. A generic calling
another generic names an instantiation that pass throws away, so those names are recorded and
answered with the body they were made from; without that a module could promise `no alloc` and reach
an allocator through a one-line generic of its own.

Two defects came out of it: a trait default was read with no imports at all, and a member of a
generic type is a generic like any other and was charged the same wrong way.

### A `--lib` source root's own dependencies

A package reached as a source root is the same package it is by coordinate, so what it depends on is
a property of it rather than of the road it arrived by. Read only by coordinate, the same directory
handed a build the package's sysl and nothing it was written against — and what a caller got was the
unresolved-name cascade that follows code whose imports point at something nobody brought, naming
neither the dependency nor a flag.

One graph rather than one per road, so MVS takes its maximum over every claim at once and a
coordinate two roots share resolves to one copy at one version. That last point had a hole in it: a
root's dependency claiming a name the root itself declares was the silent winner section 9 forbids
everywhere else — the local module answered, the dependency's was unreachable, and the build was
green.

### A tree's C comes from its modules, not every directory under it

`Project.cSources` took `.c` from every directory it walked, so a project compiled C nobody had
written for it. `cmake -B build -S .` is CMake's default and puts the build directory *inside* the
project, and a Zephyr build fills `build/zephyr/` with generated C for the toolchain it was
configured with — handed to clang against headers Zephyr emitted for `arm-none-eabi-gcc`, which fails
with `processor architecture not supported` and reads as a target-support failure in sysl or in
Zephyr and is neither.

`15 §5` already says every directory containing sources is a module, and `Project.modules` applied
exactly that rule to the sysl half; the C half did not, and the two disagreeing is the defect. The
root is exempt, because the root is the tree rather than a directory in it — a package namespaced by
reverse DNS has no sysl at its top, so C belonging to no single module has nowhere else to go.

### A generic candidate can win overload resolution's exactness tie-break

The second tie-break asked whether a candidate's parameters are exactly the arguments' own types and
resolved those parameters with an empty substitution, so a type parameter resolved to nothing and a
generic candidate was inexact whatever the call had solved it to. `g[T](x: T)` beside
`g(s: []const int)` was ambiguous at a `[]int` argument, though only the generic one takes it as
written.

The substitution the filter was missing is already in the tables by the time it runs. That makes a
generic candidate exact for the first time, which needed a third tie-break: an ordinary declaration
beats a generic one where both are exact.

---

Artifacts on Maven Central as `sh.sysl:sysl_3:0.0.49`, and `brew upgrade sysl` for the compiler.

## 0.0.48 — 2026-08-13

### WebAssembly is a target

`wasm32-freestanding`, on `wasm32-unknown-unknown` — the first row in the registry that is a virtual
machine rather than a processor. A silent program comes out as a couple of hundred bytes of `.wasm`
that `wasmtime --invoke main` runs; one that prints fails at the link naming `putchar`, which is the
honest answer for any bare target.

Its calling convention is a sixth one and the simplest of them: an aggregate that is a single scalar
with structs and one-element arrays wrapped round it travels as that scalar, and **everything else
goes in memory at any size at all**. A pair of `i32` — eight bytes, a register or two everywhere else
— is passed by memory here, and so is a pair of floats. There is no threshold to be off by one about,
because there is no threshold. All fifteen shapes were measured against clang before a line of it was
written.

Two link flags are where the work actually was, and one of them exists because of a link that
**succeeds**. `wasm-ld` is not a variation on `ld`: without `-nostdlib` the driver opens with
`crt1.o`, `-lc` and a wasm `libclang_rt.builtins.a`, none of which exists for this triple. Then the
obvious `--no-entry` — a wasm module has no `_start` — paired with the `--gc-sections` every link here
passes leaves nothing reachable from anywhere, so the linker drops the whole program and reports
success: 278 bytes, no `main`, exit 0. `--entry=main` keeps `main` and everything it reaches, and
exports it for an embedder to call.

WASI is deliberately not here: `wasm32-wasip1` is this same convention with an operating system above
it, and it needs a wasi-libc sysroot to be measured against clang rather than against a document.

### A narrow scalar is widened on the way to C

A `u8` reached a C function as a different number. sysl named the argument `i8` and said nothing else,
where every convention but two requires the caller to extend a sub-register value first — so the
callee, promised by its own ABI that the top bits were extended, acted on whatever was left in the
register.

`CAbi.extension` is now the rule, read off clang for all fifteen triples, and three of them depart
from the ordinary sign-follows rule: AArch64 away from Darwin widens nothing at all, the Microsoft
convention widens only `_Bool`, and RISC-V 64 widens a 32-bit value with `signext` whether or not it
is signed.

The two obligations fall on opposite sides. Widening an **argument** is the caller's, and goes on a
foreign call and the declaration it names. Widening a **result** is the callee's, and goes on every
definition sysl emits — a definition cannot know that C is not on the other end of it, and `@export`
and `&f` hand this very symbol over. Nothing goes on a sysl parameter: neither end of a sysl-to-sysl
call claims the extension, so neither may rely on it.

`AbiAgainstClangTests` had never asked about a scalar, having been written when the docstring said one
crossed as itself and needed no decision. It now asks clang for every width on every target.

### Three targets for a core with no floating-point unit

A triple names an architecture and a calling convention; on Arm it says nothing about whether the
floating-point unit is there. A target now records that separately, and a Thumb row answering no
carries `-mfpu=none` on every clang command line — the link, the object, a package's C, and a
`c const` probe.

- `thumbv7m-freestanding` — the Cortex-M3, which needs no flag because the architecture has no unit
- `thumbv7em-freestanding-soft` — an Armv7E-M part with the unit off
- `thumb-freestanding-soft` — the same for Armv8-M

`soft` is gcc's own spelling for "no FPU instructions at all", so the Armv8-M rows now read `hard`,
`softfp`, `soft`. Two rows therefore share a triple, which used to be impossible: the registry's
uniqueness claim moves to what is said to clang entire. `mps2-an385` joins the QEMU tier for the
Armv7-M row.

### A constant is the declaration, not a spelling of it

`13 §7` says a constant expression is a literal, a `const`, a conversion and the operators — a `const`
meaning the declaration, however the name is spelled. The folder read it as a spelling: it matched a
bare identifier and nothing else, and a full path is a field read after parsing. So the same
declaration was a constant when imported unqualified and not one when named through its module, in
exactly the two positions the declaration exists for — an array bound and a `const` initializer.

It was never about `c const`, though that is where it was reported: a binding keeps its measured
constants in a sub-module of their own, so every consumer names them qualified.

### A vtable adapter emitted a module LLVM would not parse

A return attribute belongs to the signature and not to the terminator: `define zeroext i1 @f()` states
what the function guarantees, while the `ret` inside it names only the value's type. The vtable
adapter — the one place that fed the same spelling to a `ret` as to a `define` — emitted `ret zeroext
i1 %x`, which LLVM refuses. It reached only the tiers that run clang, on a closure reaching a trait
object.

---

Artifacts on Maven Central as `sh.sysl:sysl_3:0.0.48`, and `brew upgrade sysl` for the compiler.

## 0.0.47 — 2026-08-13

### `@section("…")` — a symbol may be placed in a named linker section

`@section(".vectors")` above a module `var`, a module `val` or a function says which linker section
the object lands in. It is C's `__attribute__((section(…)))`, and it is what a program says when the
*address* of a thing is part of what it is: a vector table at the address the processor fetches from,
storage in `.noinit` that survives a warm reset, a DMA buffer in the RAM bank the engine can reach, a
function copied into RAM so it can run while flash is being erased.

Until now that was the one thing reachable only from C with **no shim that removes it** — a shim can
define the object, but then the object is C's, and sysl can neither name its type nor reach its
fields. So a vector table kept a whole program half-written in another language.

```
@align(4096)
@section(".noinit")
var page_table: [512]u64

@section(".ramfunc")
erase_page(n: usize) -> bool = …
```

**A placed symbol is kept.** Nothing inside a program reads a table a linker script gathers — that is
the point of writing one — so a placed definition is a reachability root and every placed symbol is
marked `used`. Without that the attribute would compile, link and place nothing, the failure being
the *absence* of a section, which is not something anybody looks for.

The section name is not validated beyond being non-empty: `.vectors` is ELF's spelling and
`__DATA,__mine` is Mach-O's, and a character set chosen by the compiler would refuse a section some
target requires. It is refused where there is no address for a section to be about — a `const`, a
type, an `extern`, and a local, whose storage is the frame of whichever call is running.

`design/15 §13`, and the [attributes page](https://sysl.sh/reference/attributes/).

### `build-lib` asks a package what headers its C needs

The one command whose whole job is turning a declaring package into something distributable was the
one command that never asked what the C it compiles requires — so it answered with clang's
`'probe.h' file not found` out of the package's own shim, which is the exact failure
`requires { headers }` exists to replace. Worse, a **bare** `--include-path` satisfied it in effect,
because nothing was asking: a requirement could be met by accident on the machine that built the
artifact and go unenforced everywhere else.

### A `--lib` source root's allocator is the program's too

A package that brings its own heap settles the allocator for the whole program (`packages.md § 13`).
That held when the package was reached by coordinate and not when the same directory was reached by
`--lib`, so one tree answered two ways:

```
by coordinate    allocator: pvPortMalloc / vPortFree
by --lib         allocator: malloc / free
```

**The disagreement was silent**, and what it shipped was a mixed heap: the kernel's objects out of its
own allocator and every sysl allocation in the same program out of libc's, with a `free` of storage
the other one owns waiting at the end of it.

---

Artifacts on Maven Central as `sh.sysl:sysl_3:0.0.47`, and `brew upgrade sysl` for the compiler.

## 0.0.46 — 2026-08-12

Four fixes, and all four are about a build being given something it then failed to pass on.

**`sysl test` now honours `--include-path`, `--link-path` and `-D`.** It was the one subcommand that
did not hand its search paths to a `c const` block's probe compile, so a package whose constants come
from its own headers could be run, built and turned into a library — and could not have its tests
run, which is exactly the tree the feature exists for. `CPATH` was the workaround and is no longer
needed.

**A header requirement is now asked whichever way the package arrived.** A package declaring
`requires { headers { … } }` was refused by name only when it built itself; a consumer reaching it
through `--lib` or a dependency coordinate got clang's `'foo.h' file not found`, naming a header they
had never written. The refusal now reaches the reader it was written for.

**An `@export` in a `@tests` file no longer counts against a build that discards it.** The
duplicate-export check ran before the test files were stripped, so a name that was never going to be
emitted could refuse a program over a collision that could not happen.

**A failed `@assert` says what the sides actually were.** The compiler folds both sides in order to
decide the comparison and then threw the result away, so a reader told that a struct is not 16 bytes
had no way to learn what it now is except to edit the literal and rebuild until the message changed.
Recovering `16` from a failed `@assert(sizeof(Value) == 12)` took three builds; it now takes none.

Also: `design/09` no longer argues for narrowing a data enum's tag on a payoff that does not follow,
and the niche case it was conflated with is written separately.

## 0.0.45 — 2026-08-12

A package can now say which heap the program allocates from, a mirrored C struct's field order
is checked, and a package can declare the headers its C includes and does not carry.

### Naming the allocator (0076)

A package that brings its own heap says so in `package.hocon`, and that settles the pair for the
whole program:

```hocon
allocator {
  alloc = "pvPortMalloc"
  free  = "vPortFree"
}
```

Every allocation the compilation emits — a string concatenation, a `Buf` growing, a box the
reference counter builds — calls that pair, and every release gives the storage back to it.
Declaring nothing anywhere leaves libc's `malloc` and `free`, so nothing built before this
changes.

It has to be the program's pair rather than the package's because there is one heap: ownership
is settled by reference count, so which code frees a thing is not knowable when a package is
written. Two packages naming different pairs is refused when the dependency graph is resolved —
the link would not refuse it, since both symbols resolve and the program simply hands one
allocator's storage to the other's `free` at run time. Two naming the same pair unify, which is
the ordinary case for a kernel package and a driver built on it.

A library artifact records the pair it was built with, and a program that allocates another way
refuses it — a `.syslib`'s object half is compiled code and calls the pair by name. The standard
module is under that rule too and needs no action: its cache is keyed by the pair, so a program
that names an allocator gets a standard module built for it, built on demand.

`design/packages.md § 13` has the whole of it.

### `offsetof`, so a mirrored C struct is checked (0088)

`@assert` could already check a mirrored struct's size and alignment, which left the case that
actually goes wrong: two fields transposed. Sizes match, alignment matches, and every read is of
the wrong field. `offsetof` closes it.

### A package declares the headers its C includes and does not carry (0048)

A binding whose C includes headers it does not vendor now says so, and
`--include-path <name>=<dir>` answers it. The requirement is checked before clang runs, because
a header that is not there otherwise fails inside a compiler that has never heard of sysl —
`'lwip/tcp.h' file not found`, naming neither the package that wanted it nor the flag that would
have supplied it.

### `heap` is what the capability is called (0092)

A project can say it has no heap. The capability is `heap` in `package.hocon` while the source
attribute stays `@no_alloc` — two words on purpose: one says a facility exists on this machine,
the other is a promise a module makes about its own conduct. The old `alloc` key is still
accepted, because a tag is immutable and published packages already name it.

### Also

- `@assert` inside a generic is settled once per instantiation rather than once (0025).
- `..=` is refused by name rather than by a parse failure further along the line, and the
  overload roster no longer lists candidates that cannot match (0086, 0080).
- A freestanding port says where the ARC reaper's scratch is, checked against the linker rather
  than only the text (0089).
- Each processor without an interrupt form gives its own reason for not having one.

## 0.0.44 — 2026-08-11

Two things a package could not do: ask C for a value, and be built on another package.

### `c const` — a constant the C compiler works out

```
@include("<limits.h>")

c const
    BITS: u32 = "CHAR_BIT"
    SIZE: usize = "sizeof(StaticTask_t)"

var tcb: [SIZE]u8 = [0; SIZE]
```

A macro and a `sizeof` have no symbol, so the only way to reach one was a shim function that returned
it — and what comes back from a call is not a constant. It cannot size an array, stand in a match
arm, or be folded into a bound, which is exactly what a program with no allocator needs it for.

The value is measured by compiling a probe translation unit with `-S -emit-llvm -O0`, never linking
or running it, and reading the number out of the IR. So it is the **target's** answer: the same
`sizeof(void *)` measures 4 for `thumbv7em-freestanding` and 8 for the host, from a build that ran no
code. It lowers to an ordinary `const`, so nothing downstream — constant folding, array bounds, the
artifact codec — learned that the feature exists.

`build-lib` lowers before it encodes, so an artifact ships the measured number. A program linking a
package needs neither its headers nor a clang, and could not honestly be given the expression anyway,
since an artifact is built for one machine.

Both bindings that had a shim full of getters have crossed: `sqlite3` traded five C functions and
five `extern`s for a five-line block, and `qcbor` moved thirty-five values — the tag numbers, both
context sizes, the head buffer, the tag limit and the pool overhead. Both are API breaks in the
parens alone: `tag_cwt()` is now `tag_cwt`.

### A library may be built on another library

`build-lib` took `--lib` off the command line and dropped it: the driver returned to build a library
above the block that resolves libraries, so every artifact was compiled against the standard module
and nothing else.

`sdl3-ttf` declares a `Font` that renders to an `sdl3` `Surface`, so without the other library's
declarations it does not compile at all — and what a reader got was `no module is called
'sh.sysl.sdl3'`, a diagnostic in the dependency's vocabulary rather than in the flag's.

What arrives through `--lib` is kept apart from the library's own files, which is the artifact's
correctness rather than tidiness: what the object half defines is the modules the tree declares, so a
dependency folded in with them would be emitted here as well as in its own artifact — a duplicate
definition that archives cleanly and fails at somebody else's link.

`build-lib` still does not **fetch**. A `dependencies` block is a coordinate to resolve over the
network, and a command whose job is to compile one tree into an artifact for one machine does not go
looking; a package that declares dependencies and is handed no library is refused, naming the
dependency and the flag that answers it.

### Also

A `c const` is compiled outside the module's own C, so a package vendoring its header beside its sysl
could not use the feature at all — the module's directory is now searched, ahead of the command
line's paths, which is C's own precedence for a quoted include.

## 0.0.43 — 2026-08-11

One fix, for a build a package could not make at all.

### A test build keeps the definitions nothing names

An interrupt handler, an `@export` and a destructor are reachable from somewhere no reachable body
names: the processor enters the first, something outside the compilation calls the second, and the
release hook the emitter builds calls the third. Each is therefore a root of its own.

A program build knew that. A **test build** replaces the roots with the tests it is about to run, and
it replaced them with the tests *alone* — so all three became reachable from nothing and were pruned
out from under the code that calls them.

The two halves failed differently, and only one of them was loud. A pruned destructor left the
release hook calling a symbol nothing defined, so a package with `impl Drop` could not compile its own
suite at all; the failure was at the link, against a name no line of the package contains. A pruned
export had no dangling call site, so the suite passed and the package's C found out instead — which
for a package built with `sysl build-c` is its whole surface.

The list of entry kinds now sits in `Reachability.entryPoints`, because there is more than one place
a walk begins and a list written beside one of them is a list the other does not have.

### Also

`guide/lisp` said sysl has no user-facing destructor, which stopped being true when `impl Drop`
landed in 0.0.42.

## 0.0.42 — 2026-08-11

Three language features, each of which had been written up somewhere in the specification as an
absence to work around.

### Module storage may hold a counted value

A module `val` or `var` may now hold a `&T`, a `weak T`, a slice, a `string`, or anything built out
of them. The count the storage takes is given back on every assignment that replaces it, and the last
one it holds is never given back at all — which is what a static is.

The rule this replaces refused a counted type unless its initializer was a constant tree, on the
ground that storage lasting the whole run has no line to write a release on. That is true and is a
description of a static rather than an argument against one: the only release with nowhere to go is
the one at exit, and there is no exit pass.

What it cost was the shape every callback interface needs. A C function that calls back takes an
address and an opaque word, so a binding wanting to offer a sysl closure has to keep it where the
trampoline can find it again — and module storage is the only storage that outlives the call.

Storage with no initializer must have a type with a **zero**, which is the narrower rule that
replaces it: a `&T` has none, and neither does an enum. That is the same question a local declared
with no initializer is already held to.

### Function overloading, `extern`s included

A name may be declared more than once, and every use of it still means exactly one declaration.
Which one is decided by the arguments a use passes — how many, and what type each is. Never by what
the declarations return: a pair differing only in the result is refused where the second is written,
because sysl reads an expected type inwards and such a call could not be resolved before its context
was typed.

A pair no call could tell apart is refused at the declaration rather than at each use, which covers
both the return-type case and a difference hidden behind a default — in the second the default is
unreachable, since the shorter declaration takes every call that could have used it.

Two `extern`s of one name are two functions exactly when they name two symbols, so a C family spelled
`_solid`/`_shaded`/`_blended` can be one sysl name. Two naming the same symbol are refused: that is
one C function claimed at two signatures.

`sysl.math` and `sysl.text.Ascii` stay traits. Their chapters argued from overloading's absence, and
what survives is the half overloading does not give: a member written over the others is inherited
once rather than repeated per width.

### A type-bound destructor

`impl Drop for T` says what a type does when the last reference to one of its values goes. One
member, `drop(self)`, answering nothing.

It is for the resource the language does not manage — a descriptor, a mapping, a handle a C library
made. `defer` covers every site a program can name, and this covers the deaths it cannot: a resource
inside a container, or inside a struct inside a container, dies at a point with no expression in the
source, so no `defer` reaches it.

It runs before the value's own references are released, so `self` is intact and a field may be read
to close what it names. It is not called for a value that never reached the heap, for a value in a
reference cycle, or for module storage when the program ends — and no order is promised among values
that die together.

### Also

`main` does not overload. A program starts in one place, and the entry point is found by asking which
declarations are called `main` — so a second one would be invisible to that question and the program
would start at whichever was written first with the other silently unreachable.

An overload exported under its own name is refused by name rather than by the rule that an export's
symbol must be a C identifier, which had been reporting the compiler's internal spelling for a second
declaration.

A module-level `val` holding a callable is now callable by name, as a local one already was.

## 0.0.41 — 2026-08-11

The standard library's directory is called `library` now, and a literate source can be rendered.

### `lib/` is `library/`

The directory holding the standard library's source was called `lib`, which reads as a build output
or as somewhere to keep somebody else's jars. It is neither: it is the language's own source, it is
permanent, and it is meant to be read. So it is `library/` in the tree, and an install puts it at
**`<prefix>/share/sysl/library`** rather than `<prefix>/share/sysl/lib`.

**Nothing breaks, and that is deliberate.** Both spellings are searched, the new one first, in the
install prefix and in the working directory alike. A compiler installed before this release goes on
finding its library; so does a checkout that has not been updated, and so does any project pointing
`SYSL_LIB` at a copy it unpacked earlier. `SYSL_LIB` itself is unchanged — it already said *library*,
and it names a root outright.

**One thing did change for anybody quoting a diagnostic.** A message naming a library file now always
says `library/sysl/…`, whichever directory the library was actually read out of. It used to be built
from that directory's own name, so the same file was `lib/sysl/print.sysl` out of one install and
`library/sysl/print.sysl` out of another — and for anyone who had pointed `SYSL_LIB` at, say,
`/tmp/mine`, it was `mine/sysl/print.sysl`. A path a page or a test can quote has to be the same
everywhere.

### `sysl doc`

```
sysl doc guide/lisp/lisp.lsysl
sysl doc library/sysl/regex -o regex.md
```

A **literate** source rendered as Markdown. A `.lsysl` file is already a Markdown document whose
four-column-indented part is the program — which is what makes one readable with nothing rendering
it, and is also the one thing this command exists to undo. An indented code block carries no
*language*, so no highlighter can dispatch on one and nothing scanning for the code can find it.
`doc` puts each block inside a fence tagged `sysl` instead, and passes prose, illustrations and
heading levels through exactly as written.

It is a **source-level** command: no target, no standard module, no libraries, so a package's prose
is readable on a machine that could not build it. What it does share with a compilation is the
reading, so a file the compiler would refuse — a tab in an indent, a fence that is never closed — is
refused here with the same message.

This is not an API reference generated from declarations. sysl has no documentation comment yet, so
there is nothing for such a thing to read; that needs a decision about the language before it needs a
renderer.

### A callback across the C boundary, and a correction

`guide/qsort` binds C's `qsort` — the smallest honest example of a C routine that calls *back* into
sysl, needing the address of a comparison generic in the element type, the address of a slice's
storage, and the size of an element, all at once.

It also corrects something the FFI reference has been claiming. A trampoline whose signature hides
the type — C's own `int (*)(const void *, const void *)` — cannot have its address taken, because
there is nothing for the expected type to solve. The docs concluded from that you need a concrete
trampoline per element type. You do not: write it over `*T`, name its address in a `val` whose type
mentions `T`, and cast the **function pointer** at the call rather than casting inside the body. One
generic body then serves every element type.

**The sorts in `sysl.slices` stay written in sysl**, and the library page now says why. It is not
speed. `sysl.slices` requires no capability, which is a promise made to every target sysl builds for
— including the freestanding ones, where there is no C library and `qsort` is an undefined symbol at
the end of somebody's link. On a hosted machine the promise fails more quietly: glibc's `qsort`
allocates a merge buffer and Darwin's sorts in place. The compiler cannot see either fact, because
what is behind an `extern` is behind it.

## 0.0.40 — 2026-08-10

The original Raspberry Pi Pico — the RP2040 — is a target now.

### `thumbv6m-freestanding`

A Cortex-M0+, which is **Armv6-M**: Armv8-M's predecessor rather than a subset of its options, and
the first Arm target here that is a different architecture rather than a second convention over the
same one. Its name carries the sub-architecture where `thumb-freestanding` and
`thumb-freestanding-softfp` do not, because that is the whole of what separates it.

Build an original Pico's program for this one. Building it for either `thumb-` row produces Armv8-M
instructions the core cannot execute, and the failure is a fault at whatever ran first rather than a
refusal at the link.

**The ABI needed no work**, which was the surprise. AAPCS32 under soft-float already described this
core exactly and the clang oracle passed on the first run: the only convention question it asks —
whether a homogeneous floating aggregate travels in floating registers — has the same answer on a
core with no such registers as on one whose convention declines to use them.

So the float column in the target list now reads `no` for two unrelated reasons, and `targets.md`
says so rather than leaving it to be misread. `softfp` is a *convention* chosen over an FPU that is
present; the M0+ has no FPU at all.

### What Armv6-M actually costs

Everything below the convention, and the QEMU tier is what found each of it. There is no Thumb-2, so
a literal-pool load into `sp` and a store with writeback — both ordinary on the M33 — do not
assemble. There is no divider, no 64-bit shift and no widening multiply, so `/`, `>>` on a `long` and
`*` on a `long` are all **calls**. A real project gets those from the toolchain's runtime and
pico-sdk links one; sysl emits the same references any C compiler would for this triple.

The tier runs on QEMU's `microbit`, an nRF51822. That is not an RP2040 and does not need to be: what
is being exercised is the architecture, running instructions the back end actually chose. All 27 of
its cases pass, including rendering a `long` through software division and multiplication.

### Atomics are the board's, and deliberately so

Armv6-M has no `ldrex`/`strex`, so LLVM cannot lower an `atomicrmw` inline and calls
`__atomic_fetch_add_4` instead. That reaches **exactly one construct**: the atomic retain pair is
emitted only for a program holding a `&sync T`, so an ordinary program on this target emits no atomic
at all and links as it stands.

A program that shares across the RP2040's *two* cores needs an answer, and it is not the compiler's
to give. **Disabling interrupts is the obvious implementation and is wrong** — `PRIMASK` is per-core,
so it buys atomicity against this core's handlers and nothing whatever against the other core, and a
lost reference-count update is a premature free that surfaces nowhere near the mistake. What the chip
has instead is a hardware spinlock, which is a fact about that board. The `pico` package carries it.

### Two new packages

- **`sysl-lang/rp2040`** — the chip's register map, 37 peripherals and 15,313 constants, generated
  from Raspberry Pi's own SVD by `sysl-lang/svd`. It is *not* `rp2350` with a name changed: the two
  share an APB base and almost no addresses.
- **`sysl-lang/pico`** — the board through the C SDK, which is `pico2` for the older part plus the
  atomics above.

## 0.0.39 — 2026-08-10

A closure written inside a test could not name what the test was written beside.

### The bug

```
@tests

fixture() -> int = 6 * 7

apply(f: &Fn(int) -> int, n: int) -> int = f(n)

@test("the fixture is the answer, reached through a callback")
the_fixture_holds() =
    assert_eq(apply(v -> fixture() + v, 0), 42)
```

That was refused, with the diagnostic meant for a shipped function reaching into a test file's
scaffolding — while calling `fixture()` *directly* from the same test was fine. A lambda, a bare
function name and a read of the file's own storage all failed the same way, and only turning a name
into a **callable** did.

It made a recording test double impossible in the obvious way, which is the shape anything taking a
callback is tested with. It was found writing `sysl-lang/st7796`'s.

### Why

A closure lowers to a function of its own, under a name no reader wrote, in no table that remembers
which file it came from. The rule about who may name a `@tests` file's declarations is stated over
the *referring declaration*, and a closure had lost which declaration that was — so it was held to
the rule about everything else while sitting inside one of the two things the rule exempts. It is
now judged by the body it was written in.

### And the drop was owed with it

A closure is a struct and an `impl` of `Fn`, so lowering one inside a test writes a method table —
and a table is a **root** for pruning rather than something pruning decides about. Left behind, it
would have held the lowered body alive to call helpers a shipping build had just removed: a missing
symbol at the link rather than a diagnostic. A table whose slot names something the drop took now
goes with it.

Nothing else moved. A closure written in an ordinary function still may not name scaffolding, and
that is the case the regression tests are built around.

## 0.0.38 — 2026-08-10

A line editor for terminals that will not do it for you, and a defect in how the standard module reached a compilation.

### One line editor, a Mac terminal and a microcontroller

A terminal with no line discipline hands a program nothing: no echo, so nothing appears as it is typed, and no editing, so a mistake cannot be corrected. That is every serial console, and it is a hosted terminal in raw mode. The only editor in the language lived in one board's package, wired to that board's `getchar`, and could be exercised only by a person typing at a cable.

`sysl.term.edit` is that editor over a `*Reader` and a `*Writer`. It reads `ESC [ …` for the arrow keys and writes no escape sequence at all — every movement is relative, a backspace to go left and a re-echo to go right — so it asks nothing of the platform and every target compiles it on the same terms. Insertion, backspace, delete, `←→`, Home and End, `Ctrl-A/E/B/F/U/K`, and a 64-line history on the arrows.

It answers whole lines through `Iterate[string]`, the same as `sysl.io.lines` and `console_lines`, so the three are interchangeable at a call site and a program chooses by what is producing its input. The same twenty-line REPL now runs at a desktop terminal and over a serial cable to a Pico 2 W, differing in which streams it is handed.

Two things it does that the editor it replaces did not. It measures in **columns**, so erasing a CJK character or an emoji clears both of them rather than leaving half on the screen. And it reads **both spellings of an arrow key** — `ESC [ D` is CSI and `ESC O D` is SS3, and a terminal picks between them by whether application cursor key mode is on; reading only the first means a left arrow inserts a stray `D`.

### `sysl.term.tty` takes the terminal over

`raw()` puts the kernel's line discipline out of the way and `cooked()` puts it back. **`raw()` answering `false` is not an error** — with input redirected there is no terminal to change and an editor is the wrong facility anyway, so a program picks its reader from the answer and `prog < script.txt` goes on working.

It sets cbreak rather than raw, and gives up one thing deliberately: signals. Leaving `isig` alone would keep Ctrl-C interrupting, which reads like a feature — but a program interrupted in cbreak must restore the terminal from a signal handler, and restoring means allocating a command string and forking a shell, neither of which is async-signal-safe. It deadlocks rather than tidying up. So Ctrl-C arrives as a byte for the editor, which is also what it has always been on a board.

### Reading from memory, and writing to it

Every `Reader` and every `Writer` in the library was a file descriptor, which made anything taking one testable only by arranging for real input on a real one. `sysl.io.bytes_reader` and `bytes_writer` are the pair that were missing.

`bytes_reader_at_most(b, n)` earns its place: anything reading a stream has cases that arise only when a unit of input straddles two reads — the two bytes of a `\r\n`, an escape sequence, the continuation bytes of one character — and those are exactly the cases a hand-rolled reader gets wrong. A reader that always empties itself in one go can never produce them.

### The standard module was carrying its own tests

`Stdlib.fromSource` handed every compilation the library's own `@tests` files. They were ordinary declarations on that path — nameable, and worse **instantiable** — so a generic named only by a test helper was monomorphized into every program compiled against the source standard module, and the library shipped instantiations no caller had asked for. `LibraryArtifact` had always stripped them; this path never did, which also meant the two ways of reaching a standard module disagreed.

It surfaced as an emitted-type *order* difference rather than a missing type, because the leaked instantiation happened to be one a later library function asks for anyway: it arrived earlier on the path that could see the tests. A test file naming a type nothing else used would have been a plain divergence and far easier to read.

### Also

Documentation now tracks development rather than releases. `sysl.sh` has `dev` and `stable` matching this repository's, and CI publishes an interim compiler on every push to `dev` for the site's `dev` to document against — so a page is written and *verified* as a feature lands, instead of drafted blind and held until a release. A release merges dev into stable in both repositories.

## 0.0.37 — 2026-08-09

Five fixes, four of them found by writing something against the language rather than by reading the compiler.

### A package can name its own modules again

A written module path was decided by its **leading segment**: if any module anywhere in the compilation began with that word, the path was taken as written and the package layer was never reached. `packages.md § 9`'s reverse-DNS convention makes every package in an org share `sh`, so the convention guaranteed the collision rather than making it unlikely — one `--lib` root supplying `sh.sysl.harness` was enough to stop a fetched package importing its own `sh.sysl.pico2.externs`, and to stop the program that depended on it naming `sh.sysl.pico2` at all.

The question is asked of the whole path now, and the three readings are ordered in one place: the package the file belongs to, then the path as written, then the packages its manifest named.

### `sysl run` gives the program its own input

It ran the program through the function the compiler uses for **tools** — clang, git, llvm-ar — which closes the child's input before it starts. Every sysl program that reads was handed end-of-input at once, so `examples/wc.sysl` read nothing from a pipe and a console printed its banner and exited. What the program writes is now forwarded as it writes it, rather than collected and printed at the end.

### `@align(n)` marks a `var` or a `val`

C's `alignas` beside Rust's `#[repr(align)]`. It says nothing the struct form could not, and what it saves is at the use site, where wrapping a buffer in a type turns `region[i]` into `region.bytes[i]`. All four spellings take it, `@packed` takes none of them, and an aligned array whose view outlives its frame is refused rather than quietly moved to a heap where the boundary would be the allocator's answer.

### A box's storage comes back through its own hook

Releasing a slice went through `arc.unshare`, which named `free` directly — so any module that so much as touched a view emitted a call to it, and a program built for a bare board failed at the link on a symbol it had gone out of its way not to need. `@no_alloc` changed nothing.

The header keeps its three words and the hook takes the phase it is being called for: run over the contents when the strong count reaches zero, give the storage back when the weak count does. A module that builds no box emits no hook and calls no `free`; a module holding a heap slice something else made frees it with the allocator that made it.

### A package's C takes its target's enum width

AAPCS says an enumerated type is the smallest containing type and GNU's `arm-none-eabi` defaults to that; clang on the same triple defaults the other way. sysl passed neither, so a package's C and the project it was linked into disagreed about how wide an enum is — which the linker reported on every link of a Wi-Fi demo for two days.

### Also

A link tier that links against a board's startup and nothing else — no putchar, no malloc, no free. Every cross-target tier below QEMU stops at an object file, and a call to a function nothing defines makes a perfectly good object, which is why the deallocation defect above survived months with freestanding targets in the registry.

## 0.0.36 — 2026-08-09

### A variant belongs to its enum, not to the module

Two enums in one module may now each name a variant `Failed`, and neither has to be renamed. A
module that accumulated enums was previously running out of the ordinary words one at a time —
`Failed`, `Done`, `Empty` and `Invalid` were each usable once, and the pressure was towards either a
worse name or a longer one.

**What a bare name means is settled where it is used, by the type expected there.** That is where
sysl parts company with Rust, which requires `Link::Failed` at every site unless a scope opts into
`use Link::*`. Nearly every site in sysl supplies an expected type already — an argument, an
annotated binding, a `return`, a field — so the short form is what you normally write, and nothing
that compiled before needs changing:

```sysl
enum Shape
    Circle(r: int)
    Square(side: int)

enum Hole
    Circle(r: int)
    Slot(len: int)

val s: Shape = Circle(2)        -- the annotation says which
print(depth(Circle(5)))         -- the parameter says which

s match
    Circle(r) -> ...            -- the scrutinee says which
```

Where two enums answer and nothing says which, that is a diagnostic naming every candidate rather
than a quiet choice — a construction that picked the first-declared enum would be a line whose
meaning changed when somebody added an unrelated enum above it:

```
error: 'Circle' is a variant of 'Shape' and 'Hole', and nothing here says which — qualify it,
       as 'Shape.Circle'
```

`Shape.Circle(1)` is what that line wants. The qualified form is the dot spelling, and it already
worked in patterns.

Two rules fall out of it, both stated in `09 §3`:

- A variant still may not share a name with a **constant, a `val`, a module `var` or an `extern`
  variable**. Two variants of a name are told apart by the enum they belong to; a variant and a
  constant have nothing to be told apart *by*.
- **Visibility follows the widest enum offering the name.** A `private enum` naming a `Circle` does
  not hide a public `Shape.Circle`, and from outside the module the private one is not a candidate
  at all — so a name that is ambiguous inside its own file can be perfectly clear elsewhere.

### A member whose signature failed to analyze no longer takes the compiler down

A member is filed in the declaration table before its signature is built, so that a mistake in the
signature does not also erase the member it is about. Everything reaching a member through that
window indexed `funcInsts` with `apply`, which throws.

Whether it fired came down to which of two methods was written first: a caller written *above* its
callee arrived before the signature had failed to be recorded and found a declaration with no
signature, while the same code written below reported the ordinary diagnostic. So seventeen lines
either said `unknown type 'Nonesuch'` or died with
`NoSuchElementException: key not found: S.inner`, depending on nothing but ordering.

## 0.0.35 — 2026-08-09

Seven kanban cards, and the two that were meant to be small found a compiler defect each.

### Write a duration the way a datasheet writes one

`5.us` `5.ms` `5.s` `5.minutes` `5.hours` `5.days`, as properties on any integer, so a timeout reads
as a number and a unit rather than as a call:

```sysl
import sysl.time.*

sleep(250.ms)
join(ssid, pw, auth, 20.s)
```

Short units are symbols because that is what a datasheet uses and what gets written most; long ones
are words because the line is not dense there. `5.min` is deliberately absent — it would sit beside
`min(a, b)` and `int::Min` meaning something different in each position. The free constructors stay:
a duration built from a computed value reads better as `millis(n)`.

**And `d * 3` compiles.** It was refused for wanting `sysl.Mul[int]` from a type implementing
`sysl.Mul[long]` — a complaint about the literal's default rather than about anything the program
said, because a bare literal beside a *written* type has nothing to take and falls back to `int`. The
right operand is now read against the argument the subject's own implementations agree on, which is
the concrete counterpart of what a type parameter's bounds already did. Where a type implements the
operator at more than one right-hand type there is nothing to agree on, so the diagnostic stays.

### A terminal does not end a line the way a file does

`console_lines(r)` takes CR, LF and CRLF alike, where `lines(r)` splits on LF and is unchanged. A
terminal sends a bare `\r` when Enter is pressed, so a cursor splitting on LF never saw a line at
all — it waited forever and looked hung, with nothing to grep for.

The hard half is that the two bytes of a `\r\n` can arrive in different reads, and a bare-CR terminal
never sends the LF at all, so the line ends at the CR and the cursor carries the debt across the
refill rather than waiting inside the read.

### The Cortex-M33's other float ABI

`thumb-freestanding-softfp` — `thumbv8m.main-none-eabi`, arguments in core registers. GNU ld refuses
to link the two conventions together and pico-sdk defaults to softfp, so offering only the hard-float
target meant a C project had to be rebuilt to follow sysl rather than the other way round.

**Registering it found a real defect**: AAPCS32's homogeneous-floating-aggregate rule was applied
under both variants, and softfp has no floating registers for an aggregate to travel in. A struct of
one float returned as itself where clang returns an `i32`; two floats and four doubles wanted an
`sret` clang asks for and sysl did not. Caught by the clang-diff oracle the moment the target
existed, which is what that oracle is for.

### `-D`, the flag `--include-path` could not stand in for

A header found is not a header that compiles. A C project of any size configures its own headers with
macros, and pico-sdk's `pico/cyw43_arch.h` `#error`s on a build that has not said which architecture
variant it means — so a real consumer had every include path right and still stopped inside a header.

`--link-path`, `--include-path` and `-D` are now the three steps of one thing: find the archive, find
the header, configure the header. Nothing is defaulted, for the same reason sysl does not add
`/opt/homebrew/lib` on its own.

### Reading material

`guide/ring` stopped teaching a cost the language no longer charges: an invariant across two fields
used to make the container's own update cost the size of the container, and the multi-assignment of
`00 §2` answered that a day after the program was written. Eighteen hand-rolled counter loops across
`guide/` and `examples/` became `for` ranges; the ones left as a `while` move irregularly and say so.

### Elsewhere in the org

`sqlite3` **v0.3.0** receives `sqlite3_prepare_v2`'s trailing-text pointer, so text holding several
statements is no longer silently one. `sqlite-repl` **v0.1.1** deletes the quote-aware scan it needed
to refuse such a line.

## 0.0.34 — 2026-08-09

Five cards from the kanban board, batched into one release.

### A simple enum is `Eq`

An enum whose variants all carry nothing is its own discriminant, so there is
exactly one thing equality could mean — and no finite list a library could have
written blocks over. It is now a compiler-provided member of `Eq`, by the same
rule that makes every width of integer one. The membership satisfies an `Eq`
**bound**, so a simple enum goes into anything written over `[T: Eq]`.

It is deliberately not `Ord`: declaration order is an order and not a meaning.
A data enum is unchanged — comparing two of those means comparing payloads,
which is an `impl` a program writes.

Writing the block by hand for a simple enum is now refused rather than silently
ignored, and the refusal says why. `qoi`, `qcbor` and `termbox2` each carry such
a block and are swept alongside this release; every one of them was
`int(self) == int(rhs)`, which is the comparison the membership emits.

### `sysl build-c` compiles the C in a `--lib` tree

`15 §7` gives a source root named with `--lib` the same answer as the project's
own tree: its C is compiled and reaches the link. `build-c` walked the project's
tree alone, so a package carrying a C shim built cleanly, wrote an archive, said
nothing, and handed the C project a wall of undefined references — from a link
its author had no reason to connect to a flag passed to a different command.

### A bare literal infers at a `volatile` field

`Gpio(10, 0)` against `input: volatile uint` was refused with *"'input' of
'Gpio' is volatile uint, but int was given"*, while `regs.input = 10` had always
been fine. The literal was read against the expected type directly while the
mismatch was judged through the representation, so the position asked for
exactly the type the literal was then refused for not having. A transparent
constrained subtype over any base but `int` was refused the same way.

### A closure is called a closure

A closure struct is filed under a number that runs over the whole compilation
with the standard library lowered first, so `.closure4` in a diagnostic named
nothing a program contains and moved whenever `lib/` gained a closure literal.

### A malformed header attribute is refused where it is malformed

`@requires(` on line 1 ended the file's header and was told by the statement
grammar that it belonged in the header — false advice, with nowhere to act on
it. The header now commits once it has seen `@` and a header-attribute word, so
the `')' expected` under the unclosed parenthesis stands.

## 0.0.33 — 2026-08-08

Six tickets, all of them found by writing sysl against real hardware — a REPL and a
blink program on a Raspberry Pi Pico 2 W, hosted by the C SDK.

### `sysl build-c` writes an archive a C project can actually link

The advertised command failed for any module that printed, which is the first thing
anyone writes. The archive referred to the standard module and did not contain it, and
the standard module exists only as a `.syslib` — not something a C link line can carry
— so `clang main.c libmylib.a -o app` came back with an undefined `sysl$prints` the
reader had no way to place.

`build-c` now compiles the standard module into what it writes. `--no-std-lib` asks for
what already happens, and `--std-lib` is refused rather than silently discarded.

The suite could not have caught it: its one end-to-end case exported arithmetic, which
lowers to instructions and never reaches the library. It now also exports something that
prints, and links it with clang.

### `emit-header` carries divergence across

`never` and `unit` both spell as `void`, so a generated header made the two
indistinguishable. A function returning `never` is now annotated `SYSL_NORETURN`, a
macro resolving to C++11's `[[noreturn]]`, C11's `_Noreturn`, or nothing on anything
older — a macro because the header serves both languages and `_Noreturn` is not valid
C++. That settles what the header assumes of its consumer: C99, or any C++.

### `\b` and `\f`

The escape table was C's set less two of it. `\e` stays out: a GNU extension rather than
standard C, and Rust and Go both refuse it deliberately.

### Four things the library did not have

- **`encode_utf8(c, into)`** — the library decoded well and could not encode at all.
  Three places had inlined their own copy of the table; all three now call this one.
- **`Buf.insert(i, v)`** — `remove`'s other half. Hand-rolling it is a shift written
  backwards, which is where the off-by-one lives.
- **`millis` and `micros`** for `Duration`, with `whole_millis`/`whole_micros` to read
  them back. A blink loop wants 120 milliseconds and `seconds(1)` is not it.
- **`from_utf8_lossy`** — U+FFFD substitution, one per maximal ill-formed subsequence,
  which is what Unicode asks and what a per-byte loop gets wrong. It walks the same
  table `from_utf8` does.

### Reading a line without agreeing to be stopped by one

`line_text` answers ill-formed input by printing a panic and calling `exit`, which is
right for a program reading a file it expects to be text. On a freestanding target
`exit` is a halt with nobody to notice, so one mistyped byte hangs the board.
`try_line_text` and `Lines.try_getline` are the same conversion and the same walk,
reporting instead.

`sysl.io` also now says that a terminal is not covered — `lines()` splits on LF, a
terminal sends bare CR on Enter, so a program reading a console waits forever and looks
hung with nothing to grep for. Saying so is not a fix; the silence was the worst part.

### Also

A single-slot memo race in `LibraryTests`, found by the gate: it asked for the library
twice and compared the answers, and another suite asking for a different target in
between cleared the slot. And the word "prelude" is gone from nine places — the
mechanism was deleted a while ago, and what a program starts with is a module.

## 0.0.32 — 2026-08-08

### sysl under C, not only on top of it

`@export` publishes a definition under a plain C symbol, and `sysl build-c` writes a static archive
and a header an existing C project links against.

```
sysl build-c mylib -o libmylib.a   # writes libmylib.a and libmylib.a.h
clang main.c libmylib.a -o app
```

**`@export` is `extern` read the other way**, rename and all. An `extern` names a symbol the linker
has; an `@export` publishes one. `@export("mylib_parse")` publishes that symbol while every sysl
caller goes on naming `mylib.parse` — the module path already does the prefixing job a C library
needs, so requiring the function to be *called* `mylib_parse` would be spelling it twice.

Until now only one direction was possible. Putting sysl on top of C and adding libraries one `@link`
at a time is what every package in the org does — but a C shop with an existing program and an
existing build had no way in, because every definition carried its module path and there was nothing
to hand a C linker.

#### The boundary is a facade, and it is meant to be

What gets written is one file whose job is the export surface, exactly as a Scala program exposing
itself to Java grows one. That is why the refusals cost so little: they fire inside a file somebody
wrote to be the boundary. A generic, a method, a `private`, a `@ghost`, a `@test` and a variadic are
each refused for a reason that is a consequence rather than a decision, and **every refusal names the
shape to write instead** — a slice becomes the pointer and length C's own buffer functions take.

**An aggregate passed by value is refused rather than lowered hopefully.** Each ABI decides which
registers a struct arrives in, LLVM applies no rule of its own, and getting it wrong is a corrupt
call rather than a link error — the worst available outcome.

#### A computed module `val` cannot be reached from an export

A C project supplies its own `main`, so nothing sysl emitted runs first and the storage would hold
whatever the loader left. That is refused, transitively, and the diagnostic names the export that
reached it. **A `val` whose initializer is constant data is fine** and is never looked at — it is
written straight into the object file, which is the rule C already has for static storage.

There is no `sysl_init()` to call and no `llvm.global_ctors` registration: the first is silent zeroed
storage when somebody forgets it, the second a link-order dependency that does not exist on every
target.

#### The header

`sysl emit-header` writes the same declarations to stdout. It uses `<stdint.h>`'s fixed-width names
because sysl's integers say what they *are* where C's say what they are *at least* — an `i32` is
`int32_t`. A `char` is a Unicode scalar value and becomes `uint32_t`, never C's `char`, which would
be wrong by a factor of four.

## 0.0.31 — 2026-08-08

### 32-bit targets, and a test framework that runs on them

sysl builds for the RP2350 — both of it. `thumb-freestanding` is the Cortex-M33 half and
`riscv32-freestanding` is the RV32IMAC Hazard3 half, and they are the first targets in the registry
whose addresses are not sixty-four bits wide.

```
sysl build --target thumb-freestanding blink.sysl
sysl build --target riscv32-freestanding blink.sysl
```

**A width is now a value the compiler carries rather than a constant it assumes.** `Layout` takes the
machine's word, and exactly one LLVM type in the language mentions it — a slice is `{ ptr, ptr, iN }`,
because its length is a `usize`. Everything else is spelled identically at both widths, which is what
made this a change worth making rather than a rewrite.

**Both calling conventions were measured against clang rather than read from a document.**
`AbiAgainstClangTests` compiles the equivalent C for the same triple and requires clang's own
declaration to be the one sysl emits — fifteen aggregate shapes across ten targets, re-derived on
every run. It found a real divergence on x86-64 the first time it ran, which is the argument for it.

And programs are **run**, not merely assembled: both halves boot under QEMU from the compiler's own
suite, print through a UART, and report a verdict.

#### `sysl.harness`

A test framework that runs on the target, in the standard library:

```sysl
import sysl.harness.*

adds()
    check_eq(2 + 2, 4)

run("adds", &adds)
finish()
```

Named tests, a failure located to the file and line it was written on with both values rendered,
three verdicts — `skip` is routine on a board, where the part may simply not be fitted — and a tally.
No allocator, no operating system, no debug host: it prints through a `*Writer` you hand it, and a
UART is a volatile store through a pointer.

`sysl test` is still the answer for anything that runs on the machine you are typing on. This is for
the checks that only exist on the target.

It was the `sysl-lang/harness` package, and the package is retired in favour of this.

#### Also

- The standard-module cache is keyed by target. It never gave a wrong answer, but a cross build and a
  host build used to evict each other's artifact and rebuild, every time, announcing it as a fault.
- The compiler holds **one** target's parsed standard module rather than one for every target it has
  been asked about. A build for a single target is unaffected — it hits the same memo it always did —
  but a process that compiles for many targets in a row no longer grows by a standard module each
  time.
- `sysl targets` gives the same reason for refusing a target that `--target` does. It had been saying
  `x86-linux` was refused for being 32-bit, which stopped being true with this release.
- A missing `asm` arm names the processors as a list rather than a chain of "or", now that a bare
  block is missing five of them.

#### Freestanding does not mean self-contained

A program for a bare board names C symbols its runtime has to define — `putchar`, `free`, `memcpy`,
`memset` — and on a 32-bit machine a 64-bit division helper as well, since a `long` is sixty-four bits
everywhere and neither RP2350 core has the instruction. That is what any C compiler emits for the
same code, and an SDK links `libgcc` or compiler-rt without being asked. It arrives as
`undefined symbol: __aeabi_ldivmod` at the link and nowhere earlier.

## 0.0.30 — 2026-08-08

Two language features, and a sweep across everything anybody reads.

### `T::Min` and `T::Max`

An integer type's name now answers the extremes it can hold, in the same `::` spelling the enum and
constrained-subtype attributes already use.

```sysl
print(u8::Min, u8::Max)          -- 0 255
print(i8::Min, i8::Max)          -- -128 127
print(int::Min, int::Max)        -- -2147483648 2147483647
```

**The open `iN`/`uN` family is why these have to exist rather than be written out.** A program can
spell `4294967295` for a `u32` and lose nothing; the largest `u10000` is 3,011 digits, so for a wide
member the attribute is the only way to name a value the type obviously has. It is also why they
cannot be a library table the way C's `UINT8_MAX` is — there is no finite set of integer types to
tabulate.

They are constants taking no argument, so they fold, which puts them where no call is admitted: a
`const` initializer, an `@assert` condition, an array bound.

`Min`/`Max` are deliberately not `First`/`Last`. Those name the ends of a *declared sequence* and an
enum's discriminants may be explicit and non-contiguous, so its first-declared variant need not carry
the smallest value. A `within`-ranged subtype answers both, since there the two questions agree.

### `@packed` and `@align(n)`

Two attributes on a struct, and they are separate axes: `@packed` removes the padding *between*
fields and drops the aggregate's own alignment to one; `@align(n)` raises where the aggregate must
*begin* and rounds the size up so an array keeps every element on the boundary.

```sysl
@packed
@align(16)
struct Head
    tag: u8
    len: u32
```

They compose because the gaps between fields and the boundary the whole thing starts on are different
questions — a wire header that has to live in a DMA-capable buffer is exactly this shape.

`@align`'s bound is folded rather than lexed, so `@align(CACHE_LINE)` works wherever a program has a
name for the number, and it must be a power of two. It may only raise: asking for less than the fields
need changes nothing, since lowering is what `@packed` is for.

**A packed field has no address.** `&s.f` is refused, because the field sits at its declared offset —
very often not a multiple of its own alignment, which is the point — while a `*u32` is a `*u32`
wherever it came from and every use of one is entitled to assume the address is aligned. Reading and
writing the field are untouched.

Sub-byte fields are not part of this: an `iN` field still occupies its allocated width, so a
five-bit hardware register is still shifts and masks.

### The library and the guides read better

Roughly 2,300 redundant literal type suffixes are gone from `lib/`, `guide/` and `examples/` — a 97%
strip rate. `1usize` and `0u8` say nothing `1` and `0` do not, and these trees are the reading
material the language is judged by.

Where a type was genuinely needed it moved to the declaration, which states it once instead of at
every use. About forty suffixes stayed, each for a reason: an array literal carries its element type
in the suffixes, a range with both bounds literal types its loop variable, an `if` or `match` used as
a value takes no type from a sibling branch, and a float width or an out-of-`int` magnitude is a real
difference rather than noise.

The same sweep went through every package in the organisation and through the documentation site.

### Also

`guide/slab` recorded that the language could not demand aligned storage. `@align(n)` answers it, and
the guide now says so — the allocator still rounds up, because it is handed a slice it did not
declare, but that is now a choice rather than the only option.

`sysl.sh` gained reference pages for both features, and lost two claims that had gone false: that
"two kinds of type" answer `::` attributes, and that a packed layout and an alignment annotation
"is not designed".

## 0.0.29 — 2026-08-07

Three new library modules, a faster integer rendering, and a compiler bug that only a library build could expose.

### `sysl.slices` — what a program does to a `[]T`

Searching (`index_of`, `last_index_of`, `contains`), the extremes as indices (`min_index`, `max_index`), comparison (`equal`, `starts_with`, `ends_with`), rearrangement (`reverse`, `swap`, `fill`), and `binary_search`, which answers a pair — whether the value was found, and the index it is at *or would be inserted at*, since the insertion point is the expensive half and is wanted on a miss.

**Two sorts, and neither allocates.** `sort` is an introsort: insertion sort below sixteen, a median-of-three quicksort above, and heapsort past twice the base-two logarithm, which holds the worst case at O(n log n). It recurses on the smaller partition and loops on the larger, so the stack is bounded at O(log n) rather than O(n) — on a target with a small stack that is the difference between working and crashing. `sort_stable` is a bottom-up merge through scratch **the caller supplies**, so the stable half needs no allocator either.

`as_ptr` and `as_mut_ptr` hand a slice to C as the pointer-and-length pair every C interface takes. An empty slice answers `null`; the alternatives were a pointer into a dead stack frame, or a scratch that cannot be generic.

The module requires no capability at all.

### `sysl.encoding` — bytes to text and back

Hexadecimal and base64 in both directions, and fixed-width integers to and from bytes at either byte order.

Encoding writes to a `Writer`, so hex to a file costs no intermediate string; decoding writes into a slice the caller supplies, because the output length is computable before anything is read. `DecodeError` says what a caller can act on — a bad byte and where, a bad length, misplaced padding, or an output slice too small *and by how much*, so a caller resizes once.

base64 takes its alphabet and padding as parameters rather than naming four functions, and decoding accepts either alphabet without being told, which is unambiguous because `+/` and `-_` do not overlap.

### `sysl.rand` — PCG32, seeded by the caller

Reproducible by construction, which is what a test that must replay and a shuffle both want. The algorithm is named in the source with its reference and pinned against the reference implementation's own outputs, because a generator nobody can identify is one nobody can verify.

`below` rejects the unfair tail of the range rather than taking a remainder: the modulo bias in the obvious `% n` is invisible in a handful of draws and is a real defect in a simulation. `unit` takes 53 bits, the width of a `real`'s mantissa, and never returns 1.0. `shuffle` is Fisher-Yates.

**This is not a source of unpredictability and says so in its own header.** Seeding from the host is `sysl.rand.sys`, a module of its own, so that importing the generator cannot drag an operating system in behind it — the same split `sysl.term` and `sysl.term.tty` already make.

### Decimal rendering is up to 22x faster at the wide integers

`impl[T: Integer] Display for T` peeled one digit per division, and a division costs what the type's *static* width costs — 39 wide divisions to print a `u128`, about 6000 for a `u10000`. It now divides by the largest power of ten the width holds and splits that chunk with ordinary 64-bit arithmetic, paying the wide division once per nineteen digits.

`u128` and `u256` render **22x** faster; a 3011-digit value **19x**. **`int` and `u64` are unchanged**, which is what makes this safe rather than a trade: at those widths the chunk is the whole value and the divisor-computing loop folds away entirely.

`Integer` gains `Mul` as a result — every integer type has it, so no type is excluded.

### Fixed: a closure inside a generic crashed the compiler

A generic function passing a closure literal to another generic's bare-arrow parameter — `sort[T: Ord](xs) = sort_by(xs, (a, b) -> a < b)` — crashed code generation with an uncaught `IllegalStateException: the type parameter 'T' reached codegen` whenever it appeared in a **library**.

Only a library could expose it. A program instantiates the enclosing generic and emits a concrete copy beside the abstract one, which the backend never asks about; a library build strips its tests before analysis, so nothing instantiates the generic and the abstract closure is the only copy there is.

### Also

`lib/` now carries 143 of its own tests, up from 89, all run as part of the compiler's suite.

## 0.0.28 — 2026-08-07

**backticked names, compile-time assertions, and a generic function's address**

Three language changes, batched into one release because the release process is long enough that
grouping is worth more than shipping each on its own. All three came out of asking what a binding to
SDL3 would need (ticket 0023), which is the largest C library the project has measured itself
against: 1264 functions, 928 object-like `#define`s, 115 structs whose layout a caller must know.

### Backtick-quoted identifiers

A name between backticks may be anything the ordinary identifier grammar refuses — a reserved word,
or a name carrying spaces and punctuation:

```
var `item count` = 3
val `match`: int = 5

struct `Grid Cell`
    `row index`: int
end `Grid Cell`
```

**In a pattern it references rather than binds**, which is the half it was added for. A bare name in
a pattern binds unless it resolves to a nullary variant or a constant, so a `val`, a local or a
parameter could not be tested at all — each is storage read while the program runs, and there was no
value for a compile-time pattern to compare against. Quoting says the test was meant:

```
n match
    `limit` -> "at the limit"     -- tests: n == limit
    other   -> "something else"   -- binds, exactly as before
```

A reader no longer has to know what is in scope to know which reading applies. Such an arm is a
runtime equality, so it tells exhaustiveness nothing and a catch-all stays required.

A backticked name may not contain a `.`, and a module path may not be quoted at all — a qualified
name is carried as a dotted string, and the module a symbol belongs to is recovered from it.

### `@assert` — a condition settled while compiling

```
@assert(sizeof(FRect) == 16, "FRect must match SDL_FRect")
```

The condition is a constant expression, folded by the machinery a `const` initializer already goes
through, so it may name constants, `sizeof`, `alignof` and the arithmetic over them — including a
constant declared below it. A false one is a compile error quoting the message; a true one emits
nothing.

`require` is a *runtime* precondition: it is compiled, it branches, and it traps when the program
reaches it. Until now nothing could fail a build on a fact already known while compiling.

**What it buys is a C struct layout that is checked rather than transcribed.** sysl lays a struct out
in declaration order and is C-compatible by construction, but from inside sysl that claim was
unverifiable: `sizeof` reports what sysl laid out, not what the header says, so comparing the two was
a tautology. Pair it with a `_Static_assert` in a `.c` beside the sysl — already compiled with the
tree, and for the same target, so it reads the right machine's headers:

```c
_Static_assert(sizeof(struct pair) == 8, "struct pair size moved");
_Static_assert(offsetof(struct pair, b) == 4, "struct pair.b moved");
```

One pins the header, the other pins sysl's layout, and neither half finds the other's mistake.

### A generic function's address

The address of a generic function used to be refused outright, on the grounds that it is a body per
set of type arguments and so there is no one body to name. That holds only until the arguments are
settled, and the **expected type** settles them:

```
ascending[T](a: *T, b: *T) -> i32 = …

qsort(ptr_cast(&xs[0]), 3usize, 4usize, &ascending)   -- T from qsort's parameter
```

A comparison written once over `*T` is now usable at every element type, where a program previously
needed a concrete copy per type.

**It does not reach the `void *userdata` pattern.** A trampoline for one has the signature C fixed —
`(*u8, Event) -> bool` — in which the state type appears nowhere, so there is nothing for the
expected type to solve. That pattern still takes a concrete trampoline per state type.

There is no written `&f[T]`: it and `&xs[i]` are the same shape to the grammar, and only knowing
whether the name is a generic function separates them. Written anyway, it is answered by a message
naming where the arguments come from.

### Also

Two diagnostics were reworded to point at the new spelling — a bare `val` in a pattern now says a
bare name would bind rather than match, and names the backticked form; `&f[T]` no longer reports that
the function is not a value and then sends the reader to a second error.

## 0.0.27 — 2026-08-07

**a parse error points at the mistake**

A parse error now points at the mistake, and names something you could have written.

Before this, nearly every malformed expression in a function body produced one message at one
position, and both were wrong. The position was the start of the enclosing block, so the further
into a function the mistake was, the further the caret sat from it. The message was `'..' expected`
— a range operator — for four unrelated mistakes, none of which involved a range.

```
main()
    val a = 1

    val b = 2

    print(a b)
```

```
error: '..' expected          error: ')' expected
 --> a.sysl:2:5                --> a.sysl:6:13
  |                             |
2 |     val a = 1             6 |     print(a b)
  |     ^                       |             ^
```

**The position.** `scala-parser-combinators` carries the furthest failure a parse reached on its way
to succeeding, which is the only record of a mistake the grammar backtracked away from, and reports
it when the parse stops short. Every production here is wrapped in a rule that stamps the node's
position, and that rule rebuilt the result — silently emptying the field, since it has no public
constructor. What was left to report had failed outside the outermost production.

**The message.** Candidates that fail at one position are ranked last-wins, so a token that can begin
no expression at all was reported against whatever sits at the bottom of the ladder's last
alternative. Four combinators shape it now, three of them on one rule: an absence must not be spoken
of as an expectation. An optional construct that is simply not there was recording what it wanted,
and that outranked the real mistake by sitting further into the file.

Eight messages that named a construct nobody had written are gone:

| input | was | now |
|---|---|---|
| `print(1 2)` | `'..' expected` | `')' expected`, at the `2` |
| `val x = 1 2` | `'..' expected` | `newline expected`, at the `2` |
| `print(1 +)` | `'..' expected` | `expression expected`, at the `)` |
| `const A: string =` | `'..' expected` | `expression expected` |
| a stray `)` after a statement | `'match' expected` | `expression expected` |
| a file opening with a stray token | `newline expected` | `expression expected` |
| `@` with no name | `'tests' expected` | `an attribute expected` |
| a match arm with no pattern | `'false' expected` | `a pattern expected` |
| a struct field with no name | `dedent expected` | `identifier expected` |
| `break 'a 'b` | `'for' expected`, next line | `newline expected`, at `'b` |

A loop's label is also read only where a loop keyword follows it, and an indented block requires at
least one item — the lexer emits no `indent` for an empty one, so a block that parsed nothing has an
item that will not parse, and that refusal says more than the block's own closing token.

## 0.0.26 — 2026-08-07

Two additions to the specification vocabulary, both from the list of things trisc's sysl had and this
one did not.

### `@reads` and `@writes` — what a call may touch

`@pure` says a function touches no module storage at all. The looser form says *which* storage it
touches:

```
@reads(limit)
@writes(count)
bump()
    if count < limit then count += 1
```

A frame is what makes a call something other than an eraser. Given a call with nothing written down,
everything a prover knew about module state afterwards is `true` — any variable might have changed.

The compiler enforces three rules. In the body, a read needs the variable in `@reads` or `@writes`
and a write needs it in `@writes`. At a call, the callee's frame must fit inside the caller's. And an
annotated function may call only annotated or `@pure` functions — not through a value, not through a
trait object, and with no `asm` block, each being a call site with no declaration to consult.

Writing no frame is not the same as writing an empty one. A function with no annotation has effects
nobody has written down and may call and be called by anything, exactly as before; `@reads()`
`@writes()` is the positive claim that it touches nothing. That is what lets the discipline start at
the leaves and climb at whatever pace its author sets, rather than arriving as a flag day.

### A parameter may be passed by name

A parameter written with the arrow and nothing on its left takes an expression the call does not
evaluate, and the body evaluates at each use:

```
log(on: bool, m: -> int)
    if on then print(m)

log(false, expensive())        -- `expensive()` never runs
```

It costs nothing at runtime. `x: -> T` has the type `Fn() -> T`, so it lowers to a bounded type
parameter exactly as the ordinary bare arrow does — one specialized copy per call site, called
directly, with no allocation.

`x: () -> T` keeps its meaning exactly: same type, but there the caller constructs the callable and
the body calls it. The two spellings sit beside each other and a caller who wants to hand over a
callable rather than an expression writes the one that says so.

### Also

Module invariants are declined rather than left open, and chapter 17 records why: an invariant over
module state is `require` plus `ensure` written on each public function, so it saves repetition
without adding reasoning power — unlike a frame, which is not derivable from anything.

## 0.0.25 — 2026-08-07

Seven guide findings had been answered without being told, and one of them was a real gap in the standard library.

### `Instant - Instant` is the operator

`sysl.time` writes both rows of `Sub` on `Instant`, told apart by the type of the right operand and nothing else: subtracting a `Duration` lands further along the timeline, subtracting an `Instant` measures across it.

```sysl
import sysl.time.*

var t = Instant(1000000i64)
var later = t + hours(3i64)

print(whole_hours(later - t))     // 3
```

`Sub` has carried its result as `Out` since the vector-space work, so this was expressible for a while and the library had not caught up — the most frequently written operation in a date-time library was a named function because a paragraph said it had to be. `since(later, earlier)` stays, as the spelling that says which end is which.

`weekday_name` is one attribute read rather than a seven-arm table, for the same kind of reason.

### The rest is prose, and that is the point

A guide program's findings are the record of why the language moved, so a finding that stopped being true is worse than out of date. Six were rewritten against what the compiler actually does today, and five of them still carried the workaround the absence had forced:

- a simple enum's variant name is `T::Image(v)` — two six-line renderers deleted
- a struct member says `private`, on fields and methods, enforced per file
- a constructor takes its fields by name; a field still declares no default
- `Buf` has `remove` and `truncate`
- `Integer` is compiler-supplied membership over the open `iN`/`uN` families, so a sum over the built-in widths seeds from `var acc: T = 0` with no trait at all

Each was checked against the compiler before it was rewritten, and the claims that still stand were checked too and left alone.

**Nothing in the compiler changed in this release.** `shared/src/main` is byte-identical to 0.0.24; what moved is `lib/sysl/time`, the guide programs, two design chapters, and four tests.

## 0.0.24 — 2026-08-07

Four tickets, one release — the first batch assembled on the project's kanban board rather than in a
conversation.

### `sysl.term.tty` — whether to write escapes at all

`sysl.term` shipped as constants only, leaving every program to work out for itself whether output is
a terminal, whether `NO_COLOR` is set, and whether `TERM` is dumb. Most programs answer by not
asking, and a build log full of escape sequences is what that looks like.

It could not go beside the constants: a capability requirement is module-wide and `isatty(3)` needs
`posix`, so one function there would have taken all forty constants away from the allocator-free,
OS-free programs the module is arranged for. So it is a second module.

```sysl
import sysl.term.{red, reset}
import sysl.term.tty.color

main()
    val paint = color()

    print(f"${if paint then red else ""}error${if paint then reset else ""}: not found")
```

| name | answers |
|---|---|
| `is_tty(fd)` | is this descriptor a terminal? |
| `color_wanted()` | does the environment want colour — `NO_COLOR`, `TERM=dumb`? |
| `color_on(fd)` | both, for one descriptor |
| `color()` / `color_err()` | both, for standard output and standard error |

`NO_COLOR` is read as the convention actually specifies it: present and non-empty disables, whatever
the value, so `NO_COLOR=0` means no colour and `NO_COLOR=` does not.

### A project can say what it is called

`package.name` now names a directory project's executable, where before the output took the
directory's own name:

```hocon
package { name = "tool" }        # myproj/ builds myproj/tool rather than myproj/myproj
```

Requiring a `package.hocon` was the other way to give a project an identity, and it is deliberately
not what happens — a scratch directory holding one `.sysl` file is the cheapest thing in the
toolchain and stays that way. A file project is unaffected: `sysl build foo.sysl` still writes `foo`
beside the caller whatever a config in the same directory says.

### Two diagnostics that sent readers to the wrong line

**A binary operator's mismatch points at the operand that is wrong.** `print(1 + "x")` named `'+'`,
said `got int and string`, and put the caret under the `1` — the one part of the expression nobody
was complaining about:

```
error: '+' needs matching types, got int and string
 --> a.sysl:1:11
  |
1 | print(1 + "x")
  |           ^
```

The dispatched path had always done this, so `Box[int] + string` and `1 + "x"` disagreed about where
a mismatch lives. They agree now.

**A type that is not there is no longer also a failure to infer.** Naming a type the file never
imported produced the right diagnostic and then a second, wrong one:

```
error: unknown type 'IoError'
error: cannot infer the type argument 'E' of 'Ok' here — annotate the expected type
```

The second is a consequence of the first — the unresolvable name is recovered so the walk can carry
on, and the recovered type then leaves the parameter unsolved — and it asked the reader to annotate a
return type they had already annotated as fully as they could. Only the first is reported now.

---

7,572 tests green on the JVM and the full Native suite green for the release binary.

## 0.0.23 — 2026-08-07

**a package offers module paths, not top-level directory names**

**Any two packages laid out the way the org's are could not be used in one project.**

Every package published under `sysl-lang` puts its source at `sh/sysl/<name>/`. Resolution bound each
dependency's top-level *directory*, so all of them claimed the single name `sh` — a name none of them
declares, since a directory holding no source is not a module (`13 §1`). A project naming two was
refused:

```
sysl: error: package.hocon: 'sh' is the root name of two packages —
github.com.sysl-lang.linenoise.sh and github.com.sysl-lang.sqlite3.
Give one of them a 'mount' in package.hocon to say which name it takes here
```

and the mount it sent you to spells the import `tb.sh.sysl.table`, where the package's own
documentation says `sh.sysl.table`. That is the tax on import lines `design/packages.md` §9 exists to
refuse, levied by the convention the same section recommends for avoiding collisions.

### What changed

- **A package offers its module paths** — the shallowest directories holding source. One at
  `sh/sysl/table/` offers `sh.sysl.table`, and a binding covers everything below it, so
  `sh.sysl.table.Style` reaches the same package and keeps its tail.
- **A collision is two packages claiming the same path, or one claiming a path inside another's.**
  Nesting is refused rather than resolved to the longer, which would be a silent winner under a rule
  nobody wrote down.
- **A mount is unchanged.** It is a name the consumer chose, so there is no tree to read it off, and
  it still hangs a whole package under one segment.
- **A qualified reference through a namespaced package now resolves.** `sh.sysl.table.Style` written
  as a reference read as a field of an undefined `sh`, while `import sh.sysl.table` beside it
  resolved — the reference path asked the package layer with the chain's first segment alone.

### Why it lasted

Nothing had two dependencies. A single namespaced dependency binds `sh` with nothing to collide with,
so the only shape that fails is the shape nobody had written. It was found the day a program wanted
three of them.

Full suite green on JVM (7,544) and on Scala Native.

## 0.0.22 — 2026-08-07

### The standard library tests itself

`sysl test --std` says the tree in front of the compiler **is** the standard module, which is what
lets sysl's own library run its own `@test` functions.

Until now it could not. The compiler supplies `sysl` to every compilation, so pointing the runner at
`lib/` meant the library arriving twice — once as the tree being compiled and once as the copy handed
over — and every declaration was already declared. `build-lib` has had `--std` for the same reason
since libraries existed; this is that word arriving at the command that runs the tests.

Nothing infers it. A program with a `sysl` directory of its own is nearly always a mistake, and a
build that guessed would turn that refusal into a collision at the link.

```
sysl test lib --std
sysl test lib --std --filter sysl.time
```

**`sysl.buf`, `sysl.text`, `sysl.time` and `sysl.math` now carry their own tests** — 87 of them,
running in about half a second, because the library is compiled once and each test then costs a
process. What went in is what is true of the library: that `fields` collapses a run of blanks, that
1900 was not a leap year, that `is_power_of_two` says no to zero where the one-line version says yes.

They ship with the library, so an installed sysl can run them:

```
sysl test $(brew --prefix sysl)/share/sysl/lib --std
```

What deliberately stayed in the compiler's own Scala suite is anything a sysl test cannot honestly
assert. A `@test` runs code the compiler under test produced and asserts with `assert_eq`, which that
same compiler produced — so a claim like "an `f32` computes at `f32`'s width" has to be checked
somewhere the expectation was written in another language, or a compiler that had quietly widened
everything would compile the assertion into the same widening and pass it.

### A library drops its tests before analysis

Found by writing those tests. Analysis is not a passive reading: a test naming `Buf[int]` **creates**
the whole of `Buf` at `int`, and a monomorphization is an ordinary library function afterwards, with
nothing in it recording which declaration demanded it. Dropping the test from the analyzed program
therefore dropped the test and kept everything it had caused — so an artifact shipped instantiations
no caller of the library ever asked for, and its contents were a fact about its tests.

`build-lib` now removes them from the source tree instead.

**The line falls between parsing and analysis.** Every source is still parsed before the drop, so a
**syntax** error in a `@tests` file still stops a `build-lib`. What such a file no longer gets is
everything after the parse — name resolution, types, visibility, capabilities, `@test`
well-formedness, generic instantiation. So a library test that is well-formed text and wrong in every
other way builds clean, and `sysl test` is where its real errors are reported: `--std` for the
standard library, plain `sysl test` for a package's own.

### Also

- `#test` is refused with a diagnostic naming the right spelling — it always was, but the CLI page
  had said `#test` in three places, none of them in a fenced block.

## 0.0.21 — 2026-08-07

### `@tests` — a file of test scaffolding

A file's header may now say `@tests`, beside `@no_alloc` / `@requires(...)` / `@link("...")`:

```sysl
module sysl.text
@tests

fixture() -> Layout = ...

@test("a column is measured with its header")
header_counts() =
    assert_eq(fixture().width(), 12)
```

It says two things, and either alone would be unsound. **Every build but `sysl test` drops
everything the file declares**, and **nothing outside a test may name any of it**. Without the
second, a program that called a helper would compile and then fail at the link, naming a missing
symbol rather than the line that named it.

The restriction is stated over the referring *declaration* rather than over the file it sits in, so a
`@test` function may name scaffolding wherever it was written — which is what keeps a test able to
sit beside what it tests.

A program never needed this: a helper only a test calls is already pruned. A **library** did — it has
no `main` to lower outwards from, so every public declaration is emitted and advertised, and a helper
rode into the artifact. `build-lib` now leaves such a file out of the metadata a consumer reads,
while still compiling it, so a broken test in a library is reported by the library's own build.

An `impl` block may not sit in one: it fills a method-table slot, and one kept in a test build and
dropped elsewhere would mean a trait answering differently in the tests and in the shipped program.

### `assert_eq`

```sysl
assert_eq[T: Eq + Display](got: T, want: T, msg: string = "")
assert_slice_eq[T: Eq + Display](got: []const T, want: []const T, msg: string = "")
```

in the core, and `assert_approx_eq` / `assert_approx_eq_rel` in `sysl.math` for floats, where `==` is
the wrong question.

What they add over `assert(a == b)` is the two values — `panic: got 36, want 42 (main.sysl:3)` —
so nobody runs the program a second time to find out what it returned. `assert_slice_eq` reports the
lengths where those differ and otherwise the first index the two disagree at.

The hand-written form, `assert(a == b, s"got $a, want $b")`, evaluates each side twice and builds a
string; building one allocates, which puts it out of reach of an `@no_alloc` module. Rendering
through `Display` costs neither.

### Fixed: an array written where a slice goes, at a generic callee

```sysl
count[T](xs: []T) -> int = int(xs.len)

print(count([1, 2, 3]))     # was: cannot infer the type argument 'T' of 'count'
```

The non-generic `plain(xs: []const int)` took the same literal and always had — an ordinary call,
answered with a demand for an annotation. Two halves: `unify` never matched an array against a `[]T`
parameter, so `T` stayed unbound; and once it was bound, the generic call path still held a `[3]int`
where `coerce` cannot help, because becoming a slice is something the analysis does from the position
the array was written in. The argument is now re-analyzed against the parameter the solution gave it,
which is what a non-generic call does in the first place.

**A named array still does not convert** — `a[..]` is how one is written — and a generic callee now
refuses it in the same terms a non-generic one does, rather than with a failure to infer.

Found while writing `assert_slice_eq`, and fixed in the same release.

## 0.0.20 — 2026-08-06

**diagnostics point at what they are talking about**

Diagnostics point at what they are talking about.

### A message that names a member now points at the member

```
error: 'set_style' takes '*self', so it writes through what it is called on,
and a 'val' is written once — write 'var t' if it is meant to change
 --> main.sysl:6:7
  |
6 |     t.set_style(Plain)
  |       ^
```

The caret used to sit on the `.`, one column to the left of the word the message had just named. The
position of a member access is now taken **after** the dot, so this fixes every member diagnostic at
once — a field that is not there, a method that is not there, a wrong arity, a receiver that may not
be written through.

### The `val` message names the fix

`val` bound to something with mutating methods produces one error per call — five, for a five-line
program that builds a table — and each one has to be enough on its own to find the single word that
is wrong. So it now names the binding and what to write instead: **write `var t` if it is meant to
change**.

### An index of the wrong type points at the index

```sysl
print(a["x"])
```

The caret was on the `[`; the message is about `"x"`, and that is where it points now.

### What deliberately did not change

`1 + "x"` still reports `'+' needs matching types` with the caret at the `1`. **An expression's
position is its start**, which is a convention the compiler applies everywhere — it is why an `if`
reports at its keyword rather than at an operand buried inside it — so moving it is a decision about
that convention rather than a caret to fix in passing. It is written down as a ticket with the three
candidates, and the current behaviour is now pinned by a test so it cannot drift silently.

## 0.0.19 — 2026-08-06

**a val is no longer mutable through a *self method, and four more**

Five things, grouped into one release because the release process is long.

### A `val` was mutable through a `*self` method — and at module scope that was unsound

Every way of reaching writable storage out of a `val` was refused except one:

```sysl
val c = Counter(0)

c.n = 5      // refused: a 'val' is written once…
poke(&c)     // refused: …so '&' has nothing to write through
c.bump()     // accepted, and the write landed
```

The implicit `&` that a `*self` call takes of its receiver never asked the question the explicit one
is asked. At module scope that was worse than loose: a module-level `val` is emitted as an LLVM
`constant`, so the call aimed a store into read-only storage, and the optimizer — entitled to assume
that never happens — folded the reads and lost the write. Nothing in the program was unsafe.

Now the receiver is asked, and the message names the member, because the write is not at the call:

```
'bump' takes '*self', so it writes through what it is called on — and a 'val' is written once, so
there is nothing to write through
```

**A `val` holding a reference is unaffected**, which is the shape that makes `val` worth having: the
binding cannot be reaimed and what it points at is ordinary writable storage. `val h = Holder(buf())`
then `h.xs.push(1)` was always fine and still is.

### `main` may answer with a `Result`, so `?` reaches the top of a program

```sysl
import sysl.fs.{read_text, IoError}

main() -> Result[unit, IoError]
    val text = read_text("/etc/hosts")?

    print(text)

    Ok(())
```

A failure is reported once, on stderr, with a non-zero status — instead of `.unwrap()` on every
fallible call, which reports a failure as a panic naming the line that gave up rather than the thing
that went wrong. `E` must be `Display`; the status is `1`; and `Result[int, E]` is refused, since
what the platform takes is a status and not a value.

### `-v` / `--verbose`

What the build decided: which standard module it got and whether that was linked or compiled from
source, the files it read, and the command lines handed to clang with the search paths behind them.
On stderr, where `wrote <exe>` already goes. No phase timings — a build that is slow is diagnosed by
asking what it did.

### `sysl.math.approx_eq`

Binary floating point does not hold `0.1 + 0.2 == 0.3`. `approx_eq(a, b, tol)` takes an absolute
tolerance; `approx_eq_rel(a, b, tol)` scales it to the larger operand, so the tolerance reads as a
fraction at any magnitude. Identical infinities are close, and a NaN is close to nothing including
another NaN.

`guide/shapes`, `guide/matrix` and `guide/fft` each hand-rolled this; all three now call it.

### `Hash` is written `impl`s rather than a rule

A membership the compiler hands out has no function for a method table to point at, so a built-in
could satisfy a `Hash` bound and still not be erasable. `bool`, `char` and `string` now have blocks
and the integers have one blanket over the closed `Integer` bound, so this works:

```sysl
val xs: [3]&Hash = [7, "abc", true]
```

The same move `Display` made, for the same reason. A wrong-arity call now gets the ordinary member
diagnostic naming the receiver, rather than the special case's bespoke wording.

## 0.0.18 — 2026-08-06

### A build's output stops depending on where it was started

`sysl build .` failed for every project there has ever been, and `sysl build test` failed for every
project sitting in the working directory:

```
ld: open() failed, errno=21 (Is a directory) for 'test'
```

The default output was the path's last segment with an extension dropped — which for a directory
project is the directory itself, so the linker was handed a path it could not open. The same build
succeeded from one directory further up, because the collision was with the caller's working
directory rather than with anything about the project.

A directory has an obvious place to put the thing built out of it, which is inside itself. So
`sysl build .`, `sysl build test` and `sysl build ../test` are three ways of naming one project and
now write one path, `test/test`, from anywhere.

`build-lib` had the same defect wearing a different symptom — the extension is appended to the name,
so `sysl build-lib .` wrote `..syslib`, a hidden file named after nothing. Its artifact now lands
inside the root it was built from, by the same rule.

**A file project is unchanged**: `sysl build foo.sysl` still writes `foo` beside the caller. The
extension is what always saved it.

### `sysl.term`

The escape sequences a terminal understands — colours, backgrounds, emphasis, and the fixed screen
and cursor sequences.

```sysl
import sysl.term.*

print(f"${red}${bold}error${reset}: ${msg}")
```

Every one is a `const string`, and a string literal is immortal, so naming forty of them costs
nothing at run time and a `@no_alloc` module can reach every one — which is why the module declares
`@no_alloc` itself. Colouring a line is exactly what a program with no allocator most wants to do.

Two things are deliberately absent. **Anything that asks whether the terminal is a terminal**: that
needs `os`, and putting it here would take the module away from programs that only wanted to name a
colour, so the decision stays the caller's. And **anything taking a number**, since moving the cursor
to a row and column means building a string, which is the one thing the module is arranged to avoid.

`default_color` and `on_default` are there because `reset` ends *everything* — ANSI has no way to end
one attribute — so a program wanting its colour back without losing an emphasis needs them.

## 0.0.17 — 2026-08-06

Three fixes, and the second and third were found by the first.

**An unresolved name is reported at the definition.** A generic body is walked once with its
parameters standing for themselves, and that pass dropped every complaint except the ones naming a
missing bound — deliberately, because a mistake in the concrete part of a body depends on what the
parameters turn out to be and is better said at the instantiation, in concrete terms. The trade
assumes there *is* an instantiation. Where nothing ever asks for one the mistake was found nowhere:

```sysl
f[T](y: T) -> usize = nosuchthing      // compiled
g(y: int) -> usize = nosuchthing       // error: undefined name 'nosuchthing'
```

A name that names nothing is the one complaint that reasoning does not reach — it is wrong at every
instantiation and wrong at none — so an undefined name and an unknown type are now reported at the
definition whatever the pass. Everything else the pass drops, it still drops.

**A value parameter reaches a member's body in every place one can be written.** Which of a block's
parameters stand for values was carried onto the members of an `impl` matching a *shape* and onto
nothing else, so both of these compiled a body in which the parameter's name stood for nothing:

```sysl
struct Buf[const N: usize]
    used: usize

    room(self) -> usize = N - self.used     // N named nothing here
end Buf

impl[const M: Mode] Display for Run[M]      // nor here
```

Neither could fail visibly, because the pass that walks those bodies was the same one dropping what
it had to say about a name. The two halves of this release are therefore one story: the first is what
made the second findable.

## 0.0.16 — 2026-08-06

A member may declare a value or a type pack in its own parameter list.

A method's parameter list is its own, exactly as a function's is — so it may take a `[const N: usize]` or a `[..A]` that the type's own parameters know nothing about. The type's are fixed by the receiver; the member's are solved at the call.

```sysl
struct Sum
    seen: usize

    take[const N: usize](*self, xs: [N]int) -> usize
        self.seen = self.seen + N
        N
end Sum
```

A trait's member is still refused, and not for a reason about packs: no member a trait requires may declare parameters of its own, since a table slot cannot hold a function that does not exist until a call names its types.

Also pinned this release: the library's tuple rows work for parts of different types at any arity, including nested tuples — `Ord` and `Hash` had only been tested on homogeneous ones.

## 0.0.15 — 2026-08-06

### Tuples stop having an arity

`Display`, `Eq`, `Ord` and `Hash` now cover a tuple of any width. The library used to write a row
per arity and stop at three, so a tuple of four parts implemented none of them:

```
error: cannot print a (int, string, bool, real) value — the library provides 'sysl.Display' for
tuples of up to 3 parts and this one has 4, so a product this wide wants a struct of its own
```

That diagnostic is gone, along with the ceiling it explained.

```sysl
print((1, "a", true, 2.5, 'z'))
print((1, 2, 3, 4) == (1, 2, 3, 4), (1, 1, 1, 2) < (1, 1, 1, 3))
```

```
(1, a, true, 2.5, z)
true true
```

### What made it possible: type packs, and a loop the compiler unrolls

The library's eight blocks became four, and this is one of them — its own source:

```sysl
impl[..A: Eq] Eq for (..A)
    eq(self, rhs: Self) -> bool
        for const i in 0..<A.len
            if self.i != rhs.i then return false

        true
    end eq
```

**`..A` is a type pack**: one parameter standing for a list of types, where a type parameter stands
for one type and a `const` parameter for one value. `(..A)` is the tuple of it, and it matches a
tuple of any arity — inferred from the argument exactly as an array's length is.

**A bound on a pack distributes over its members.** `[..A: Eq]` asks it of every part, which is the
whole of the bound syntax a pack needs, and it is what keeps the membership answerable before any
body is compiled: a tuple holding something uncomparable is still refused where it is written.

**`for const` is unrolled at compile time**, once per value of a range that has to be known then —
`A.len` is how many types the pack stands for. Each copy is type-checked **on its own**, which is the
point rather than an implementation detail: the parts of a tuple have different types, so `self.i`
is a different selection in each copy and one written line covers all of them.

A pack may also be a function's own parameter or result:

```sysl
widths[..A: Display](t: (..A)) -> usize
    var n = 0usize

    for const i in 0..<A.len
        n = n + str(t.i).len

    n

print(widths((1, "abc", true)))
```

```
8
```

### Two of the rows came out better, and one changed its answers

`Ord`'s lexicographic ladder no longer special-cases its last position. Written per arity it had to,
having no next position to fall through to; the loop runs the same two-test ladder at every position
and ends `false` — every position agreeing is not-less.

`Hash` **produces different values than 0.0.14 did**. The hand-written rows seeded the fold from the
first part's hash, and a loop needs a constant to start from, so the FNV offset basis is now the
seed. Nothing in the language persists a hash, so this affects no stored data — but a program that
recorded one across the upgrade will not recognise it.

### Also

A tuple now has three shapes an `impl` may match, found most specific first: the tuple written out
in full, then one arity, then every arity. `impl Tag for (int, int)` still beats
`impl[A, B] Tag for (A, B)`, which still beats `impl[..A] Tag for (..A)` — the same
"written-out beats a parameter" ordering an array's two shapes have, one rung longer.

`break` and `continue` are refused inside a `for const`, and the reason is worth knowing: the copies
are straight-line code inside whatever the loop was written in, so a `break` there would leave *that*
loop, one copy at a time and silently. `return` works and is what `Eq` and `Ord` are written with.

The reference documentation for all of it is under
[generics](https://sysl.sh/reference/generics/).

## 0.0.14 — 2026-08-06

### A library may keep state

A module that holds storage and hands a private helper the job of maintaining it could not be
compiled as a library. This is the first release in which it can.

```sysl
module counter

private var count: int = 0

bump() = count += 1
peek() -> int = count
```

`sysl build-lib` refused that with two diagnostics, neither of which named the cause:

```
error: 'bump' is declared inside a function body, so nothing outside can name it and there is
nothing for a visibility modifier to restrict
```

and, where the helper was generic, `… cannot be generic — the type arguments would have nowhere to
come from`. What had happened is that the file was being read as the one the program starts in.
Where nothing runs anywhere, a lone file of bindings is a body after all — that is what keeps a
one-file `var n = 1` meaning what it always meant — but the fallback reached files with a `module`
header too, and a header says there is no body for a binding to belong to instead. The file became a
body, so every function *reading* its `var` became a nested function of it, and was refused for the
two things a nested function may not be.

It fired only where no other file supplied a beginning, so the same module compiled when a program
imported it and was refused by `build-lib` — which is exactly the shape a library has: files, and no
beginning.

### `private var` parses

A `var` at the top of a file that names a module is that module's storage, and is the same
declaration `static var` spells in the entry file. It now takes a visibility for the same reason the
`val` beside it does:

```sysl
module counter

private var count: int = 0
private[counter] var ticks: int
```

`private static var` had parsed all along; the plain spelling answered with `identifier expected`
pointed at the modifier, which reads as a missing name rather than as a form that takes none.

Both were found writing [harness](https://github.com/sysl-lang/harness), the first sysl library that
keeps state.

## 0.0.13 — 2026-08-06

### Value generics

A generic parameter may now stand for a **value** rather than a type, written `const`:

```sysl
total[const N: usize](xs: [N]int) -> int
    var t = 0
    for i in 0..<N do t = t + xs[i]
    t

var a: [3]int = [1, 2, 3]
print(total(a))                       // 6 — N is inferred from the argument
```

Ada has had this since 1983, C++ since templates, Rust since 1.51, and Zig spells it `comptime`. The
languages without it are the ones where every array is a heap object carrying its length at run time,
so there is nothing for a value parameter to be *for*. sysl is not one of those.

**A fixed-size array prints.** That is what the feature was for. One block in the standard library —
`impl[const N: usize, T: Display] Display for [N]T` — covers every array at every length, where
before it took one implementation per length and so could not be written at all:

```sysl
var a: [3]int = [1, 2, 3]
print(a)                              // [1, 2, 3]
```

Previously `print(a)` was refused and you took the whole-array view, `a[..]`. That still works and
still renders the same; it is no longer necessary.

#### Where a value parameter may be declared

A function, an `impl`, a struct, and an enum. A function's argument is inferred from the call; a
type's is written out, because a type has no call to infer one from:

```sysl
struct Buf[const N: usize]
    data: [N]byte

var b: Buf[4] = Buf([1u8, 2u8, 3u8, 4u8])
```

`Buf[2]` and `Buf[4]` are two types, with two layouts.

#### Which values

**Integers, `bool`, `char`, and a simple enum's variants.** A value parameter puts a value into a
type's identity, so it must compare and it must mangle — which those do and floats do not (`NaN !=
NaN` would make a type unequal to itself). Strings wait on interning.

#### What is refused

- `[N + 1]T` — carrying the result of a computation into a *type* is type-level arithmetic, a
  separate feature. A body may compute with `N` freely.
- `f[T](xs: [T]int)` — a type parameter where a length belongs. This used to compile, standing the
  array at length zero.

#### Diagnostics

- An array whose elements do not render now names the **element**, as a slice already did, instead
  of advising a block that could not be written.
- A type written where a value argument belongs, and a value where a type belongs, each say which of
  the two the slot is.

## 0.0.12 — 2026-08-06

### The compiler stops re-reading your program

**`sysl run` on a three-line program took 4.39 seconds of CPU. It now takes 0.56 — 7.8x faster.**
Nothing about the language changed, and nothing about the code sysl generates changed; the programs
it produced were always fast. This is the compiler itself, which had been quadratic in the length of
its input.

#### What it was

Every token's position asked `OffsetPosition` for its line and column, and `OffsetPosition` answers
by indexing every line start in the source. It caches that index against the source — but
`scala.util.parsing.input.PositionCache` is a **different file per platform**, and only the JVM's one
caches. The Scala.js and Scala Native builds of `scala-parser-combinators` substitute a map whose
`put` discards what it is given and whose `get` returns null; its own comment calls it "the /dev/null
of Maps".

So off the JVM, **every position asked for its line re-indexed the whole file**. Lexing a file cost a
pass over that file per token.

The off-side-rule lexer under sysl asked twice over. Besides stamping every token, it compared two
readers with `skipSpace(in1).pos != r.pos` to detect a line indented with both tabs and spaces — and
`Position.equals` is defined as *equal lines and equal columns*, so that comparison indexed the
source twice more per newline. That was the larger half.

#### Why it lasted this long

The development gate is `syslJVM/test`, and **the JVM is precisely the platform where the upstream
cache works**. The suite could not observe the bug. And because the cost is quadratic, it stays
invisible on the small files a test suite uses — it only bites at the size of the real standard
library, which every compilation parses.

The fix is in `io.github.edadma.indentation` 0.0.6, which indexes the source once per scan and
compares readers by offset. Positions themselves are unchanged, and that library's equivalence tests
hold it to the answers the previous implementation gave. Measured there on Scala Native over a
6,476-character source: **10,124,683 character reads before, 108,187 after**, against the JVM's
120,739.

sysl's own suite — 7,332 tests, every one of them carrying positions into diagnostics — is unchanged.

## 0.0.11 — 2026-08-06

### Reserved identifiers, and a check that names its own line

**An identifier that begins and ends with `__`, holding only capitals and underscores in between,
now belongs to the language.** Nothing may declare one — not a function, a type, a `val`, a field, a
parameter, a type parameter, a local, or an import alias.

Reserving the *shape* rather than a list of names is the point of the release. It is what makes every
future addition non-breaking: a later version adding a built-in cannot collide with a name you had
already declared, because the shape was never yours to declare. C reserves the same territory and
diagnoses nothing in it; here taking it is refused where it is written.

These are predeclared identifiers, like `int` and `usize` — not keywords, and absent from the
reserved-word table.

#### The six

| identifier | type | value |
|---|---|---|
| `__FILE__` | `string` | the file, as a diagnostic names it |
| `__LINE__` | integer | 1-based line |
| `__COLUMN__` | integer | 1-based column, in the file |
| `__FUNCTION__` | `string` | the enclosing function, as written |
| `__DATE__` | `string` | build date, `Mmm dd yyyy` (UTC) |
| `__TIME__` | `string` | build time, `hh:mm:ss` (UTC) |

`__LINE__` and `__COLUMN__` are ordinary integer literals, so each takes the type its context asks
for and is range-checked with it.

#### A default reports the caller

Nothing in this feature knows what a caller is. A default argument was already *evaluated at the
call, standing exactly where the argument would have been written*, so a parameter defaulted to
`__LINE__` reports the caller's line and no call-site machinery was needed:

```sysl
where(line: int = __LINE__) -> int = line

print(where())
print(where())
```

```
2
3
```

That is why sysl needs no equivalent of Rust's `#[track_caller]`.

#### `assert`'s message is now optional

`lib/sysl/check.sysl` required a message because the condition's source could not be printed. It can
now, so `assert(x == 2)` is a complete assertion that names its file and line. A message is still
worth writing where it says something the condition does not.

The location is streamed through `prints` and `printi` rather than interpolated, because building a
string makes heap storage — which would have put `assert` out of reach of a module under
`@no_alloc`, the module that wants an assertion most.

#### Notes

- `__FILE__` holds the path the compiler was given, so an absolutely-invoked build embeds absolute
  paths. A basename variant is reserved for when that matters.
- `__DATE__` and `__TIME__` make a build non-reproducible, as C's do.
- `__FUNCTION__` is empty outside any body, and in a closure names the function the closure is
  written in.

Full design: `design/reserved-identifiers.md`.

## 0.0.10 — 2026-08-06

### `override` — a program may replace an implementation that already covers its type

`impl Display for []Point` used to be refused: the library implements `Display` for every slice, and
two implementations for one type are one too many. Now it is refused only when it is *unmarked*.

```sysl
override impl Display for []Point
    display(self, out: *Writer, fmt: FormatSpec) = display_str("points", out, fmt)
```

The keyword goes on the **overriding** side, which is the whole of the design and the opposite of
C#'s `virtual`/`override` and of Rust's unstable `default`. Both of those make the general
implementation grant permission in advance, and a library author cannot know which of their
implementations somebody will need to replace. It grants no permission, so what it buys is the
diagnostic: an unmarked second implementation is refused exactly as before, and the duplicate written
by accident is still found.

Two checks run the other way. An `override` with **nothing to override** is refused — the one that
earns its keep when a library later drops or narrows what a program was replacing. And an `override`
on the **general** kind is refused: a shape and a generic type each have one key, so neither has
anything below it to be more specific than.

**A member that replaces a trait's default body says so too**, since that is the same act:

```sysl
impl Fallible for File
    override failed(*self) -> bool = self.err != 0
```

Required where a body is replaced, refused where the trait declared the member without one — so an
`impl` block says which of its members answer a requirement and which replace something, a question
that otherwise means opening the trait.

### A slice of anything printable now renders

One `impl[T: Display] Display for []T` in the library, so `print(xs)` works the moment the element
type renders itself. The elements are written to the sink as they are met rather than gathered: a
growable buffer is not reachable from where `Display` lives, and printing a slice must not start
allocating in a language whose printing does not. A width — the one thing needing a length before any
byte goes out — is answered by rendering once into a sink that counts and keeps nothing, so a slice
prints under `@no_alloc` exactly as a number does.

This is what waited on `override`: a blanket over every slice would otherwise have shut the door on
every program's own block, permanently.

### A derived subtype may render as something other than its base

```sysl
type Stamp = new int

override impl Display for Stamp
    display(self, out: *Writer, fmt: FormatSpec) = display_str("#" + str(int(self)), out, fmt)
```

`16 §3` said a derivation may replace none of its base's catalogue, and that ruling stands for the
operators — a `Stamp` that ordered differently would be a set of `i64`s that do not order the way
`i64`s order. Rendering is not such a guarantee: a `Stamp` printing as `#7` is the same `i64`. So it
is the one row a derivation may take back.

## 0.0.9 — 2026-08-06

### Text is measured in the columns a terminal draws

`sysl.text` gains `width.sysl`, which answers a question neither `s.len` nor a `Chars` walk does:
how much room text takes on a **terminal**.

```sysl
import sysl.text.{columns, char_columns}

print(columns("café".bytes))     // 4 -- four columns, five bytes
print(char_columns('日'))         // 2 -- East Asian wide
```

- `char_columns(c: char) -> usize` — **two** for the East Asian wide and fullwidth forms, **none**
  for a combining mark or a format character, one for everything else.
- `columns(text: []const u8) -> usize` — the sum over a run of UTF-8. It takes bytes rather than a
  `string` so text being assembled can be measured without being copied into one first.

This is **data rather than algorithm**: the rule is two lines and what makes it right is 499 ranges
out of the Unicode Character Database, which no program should be carrying its own copy of. A
`no alloc` module may use all of it — the tables are static and the search allocates nothing — and a
program that calls neither function links neither table.

**`sysl.args` help text now sets its description column in screen columns.** A program whose
`<placeholder>` is not ASCII previously had that column go ragged by one position per accented
character. It costs a program that prints help about four kilobytes of tables; one that never calls
`help` reaches none of it.

### A `var` outside the entry file is the module's storage

Mutable module state was reachable only as `static var`, and only in the file the program starts in.
Elsewhere it had no spelling at all: `static` is refused in a file that names a module — correctly,
since it asks for the module instead of the *body* and such a file has no body — and a plain `var`
was read as a *statement*, so the file was refused as a second beginning.

```sysl
module counter

var count: int = 0

bump() = count += 1
```

That is the same declaration `static var` is: same visibility rules, same value namespace, same
initializer graph, same rule that it may not hold a value owing a release. The entry file's own
top-level `var` is unchanged and is still a local of its body.

**Which file the program starts in is unaffected.** A binding is not a beginning: a file carrying a
statement that is not a binding is the entry file, and where one exists every other file's top-level
`var`s are the module's. Where nothing runs at all, one file of bindings is still a body — so a
one-file `var n = 1` means exactly what it always meant.

Found from the verification chapter, whose module-invariant example had been written against this
spelling and did not compile.

### Fixed

- **An initializer cycle through module storage crashed the compiler** instead of reporting. The
  cycle diagnostic looked its name up among the `val`s alone, so a cycle running through a `var`
  threw rather than explaining — the compiler dying on the program it was meant to be reporting on.
  It had stood since `static var` landed, because every cycle anyone had written was between two
  `val`s.
- Five diagnostics about module storage stopped naming `static`, which a file that names a module
  refuses. A reader told to write it there would have been told something false.

### Upgrading

**The first build after upgrading rebuilds the standard-module artifact**, because `lib/sysl` gained
a file. It is announced on stderr, takes well under a second, and is cached per library fingerprint
thereafter — `0.0.9-0dae8c2b6d7b18ae` on aarch64 macOS. This is expected, not a regression.

Install: `brew install sysl-lang/tap/sysl`

## 0.0.8 — 2026-08-05

**sysl has moved to the [sysl-lang](https://github.com/sysl-lang) organisation, and its published artifacts have moved with it.** This release exists to carry that change; the compiler itself behaves exactly as 0.0.7 did.

#### If you install the binary

The tap moved too:

```
brew uninstall sysl
brew untap edadma/tap
brew install sysl-lang/tap/sysl
```

A tap move is not something an existing install follows on its own, which is why the untap is there. Nothing else changes — same binary, same standard library, same behaviour.

#### If you depend on the compiler as a library

The Maven coordinate changed:

```scala
libraryDependencies += "sh.sysl" %% "sysl" % "0.0.8"   // was io.github.edadma
```

and the Scala package changed with it:

```scala
import sh.sysl.api.Sysl                                 // was io.github.edadma.sysl.api.Sysl
```

`io.github.edadma:sysl` is **not** withdrawn — 0.0.1 through 0.0.7 stay on Central indefinitely and keep resolving. There is simply no 0.0.8 under the old coordinate, and there will be no further versions.

#### Why `sh.sysl`

It is the reverse of `sysl.sh`, verified against the domain rather than against a personal GitHub account — so it does not have to move again if the repository does. It also agrees with something the language already says: every library module a sysl program imports is `sh.sysl.<name>`, so the Maven coordinate and the module prefix are now one spelling instead of two unrelated ones.

The alternative, `io.github.sysl-lang`, could not have been the Scala package as well: a hyphen is legal in a Maven groupId and illegal in a package name.

#### Verification

Gated on the full Scala Native suite — **7,198 tests, 0 failures, 252 suites** — because the Native binary is what ships. The released tarball was then extracted into a bare prefix and run through a symlink under a clean `HOME`, which is the shape Homebrew installs.

**macOS on Apple silicon only.** Other platforms build from source: see [sysl.sh/getting-started/installation](https://sysl.sh/getting-started/installation/).

## 0.0.7 — 2026-08-05

### A package that carries C can now be depended on

Both published sysl packages vendor C beside their sysl — it is how a binding reaches constants a
header defines as macros, and how a package ships a shim without needing a makefile. Until now, a
package brought in through a `dependencies` block had its `.sysl` compiled and its `.c` **silently
dropped**, so the build got all the way to the linker before failing on symbols the package's own C
defines:

```
Undefined symbols for architecture arm64:
  "_sysl_sqlite_ok", referenced from:
      _github.com.sysl-lang.sqlite3.sqlite$open in ...
```

Which meant that in practice `dependencies` worked for pure-sysl packages and for nothing else — and
no pure-sysl package existed.

**The same drop was hitting two more trees through the same missing call**: a project's own C, and a
source root named with `--lib`. All three are one mechanism now, which is what the design said they
should be. Each tree is staged separately, so two packages may each hold a `net/util.c` without
colliding.

### `--link-path` and `--include-path`

`@link("name")` could previously reach only libraries already on clang's own search path. The link
line carried no `-L` and nothing could put one there, so a library installed anywhere other than the
toolchain's prefix was unreachable — `-lpng` failing with `library 'png' not found` on a machine
where `/opt/homebrew/lib/libpng.dylib` had been sitting the whole time.

```
sysl build . --include-path /opt/homebrew/include --link-path /opt/homebrew/lib
```

Both are repeatable and searched in order. **Both exist because half of this capability is none of
it**: a binding to a library outside the default prefix fails at the `#include`, one step before
anything reaches a linker, so shipping only the link flag gets a build further and no closer.

A directive names a library and never a flag, because what a name becomes on a command line is a
property of the machine being built *for*. Where that name is looked for is a property of the machine
being built *on* — a different question with a different owner, which is why this is a driver flag
and not an attribute or a `package.hocon` field. A prefix is one machine's directory layout, and
committing it to source is the same mistake as writing `-lm`, one level up.

Nothing is guessed at: `/opt/homebrew/lib` is **not** added by default, because a compiler ruling on
where a platform keeps its libraries is wrong about a machine nobody here has.

`LIBRARY_PATH` and `CPATH` always worked, since sysl execs clang and clang reads them. That is why
these flags exist rather than why they don't — a build that works only because of one developer's
shell is one nobody else can reproduce, and it fails for the next person with a message naming a
library rather than the setting they lack.

### `sysl.sum` covers a package's C

It always did, but every case testing it used `.sysl` files, so a narrowing to known extensions would
have gone through green. That matters more now the C is compiled: it is the one file in a package
that reaches a system call with no line of sysl saying so.

### Upgrading

**The first run after upgrading rebuilds the standard-module artifact**, and says so on stderr. It
takes about a second and happens once — the cache is keyed by the compiler as well as the library, so
each release gets its own entry rather than reading back its predecessor's.

### Installing

```
brew install edadma/tap/sysl
```

Or take the tarball below and extract it into a prefix — it carries `bin/sysl` and
`share/sysl/lib`, and the compiler finds its standard library relative to its own resolved path.
A binary on its own is a compiler that cannot compile.

macOS arm64 only. Every other platform builds from source.

**`llvm` is a runtime dependency**, not just a build one: sysl emits textual LLVM IR and shells out
to `clang` to assemble and link, and to `llvm-ar` to build a `.syslib`. Apple's command-line tools
ship clang but no `llvm-ar`.

## 0.0.6 — 2026-08-05

### A compiled program links on Linux

0.0.5 and everything before it could not link a program that prints, on any platform whose linker
is GNU `ld`. This release fixes that, and it is the whole reason the release exists.

`sysl.stdout` returns a trait object built from a module-level val, which makes it the one library
function a program has to emit for itself — a val's storage is initialized by the entry point, and
a library has none. The standard-module artifact was correctly leaving it out of the list of
symbols it *advertises*, and emitting it anyway. Both definitions reached the linker.

```
/usr/bin/ld: std.syslib(sysl.code.o): in function `sysl$stdout':
        multiple definition of `sysl$stdout'; /tmp/sysl-….o: first defined here
```

`ld64` takes the first definition and says nothing, so this was invisible on macOS for as long as
anyone looked — nothing about the bug was macOS-specific, only the consequence was. It surfaced
within the hour of the documentation site moving to a repository whose CI runs on Linux, where 31
pages failed to link.

**There is now a Linux CI job**, because a macOS-only gate cannot see this class of bug at all.
sysl emits textual LLVM IR and hands it to whatever linker the machine has, so every question about
linkage and symbol visibility has two answers and one machine only ever gives one of them.

### Inline assembly with a label no longer crashes the compiler

Any `asm` block containing a label — a definition like `spot:` and a `jmp spot` to reach it — killed
the compiler outright:

```
java.util.regex.PatternSyntaxException: capturing group name does not start with a Latin letter
```

The label renaming that keeps two emitted copies of a block from defining one symbol twice was
written with regex lookaround. Scala Native's regex engine is a port of RE2, which supports neither
lookbehind nor lookahead, and the released compiler is the Native build — so this was broken in
every release from 0.0.1 while the JVM build the tests run on accepted the same pattern happily.

It is a hand-written scan now, with the boundary rule pinned by tests that run on every platform.

### The artifact cache now knows which compiler built it

Found while cutting this release, and without it the fix above would not have reached anybody who
had already run 0.0.5.

The standard module is compiled once into `<cache>/sysl/…/std.syslib` and reused. That cache was
keyed on a fingerprint of `lib/sysl`'s **source** — which is the right question for "is this the
same library", and the wrong one for "is this the same artifact". An artifact is compiled code, so
it depends on the compiler too. This release changes what the library lowers to and edits none of
its source, so it would have keyed to the same directory and read back the broken artifact 0.0.5
built.

The key now names the compiler as well as the library.

### Also in this release

- **A table program in the guide set** — the first that measures text for display rather than
  copying or comparing it. It comes with the finding that a `FormatSpec`'s width is a byte count,
  so it cannot lay out a column holding anything non-ASCII: `café` is five bytes and four columns,
  and the error is per non-ASCII character, so cells of one column come out wrong by different
  amounts.
- **The website moved to its own repository**, `sysl-lang/sysl.sh`. It documents the *released*
  language by depending on a published compiler rather than on the dev branch — a page demonstrating
  something only dev can do would be wrong for the person reading it. The specification stays in
  this repository at `design/`, because a chapter belongs in the same commit as the code
  implementing it.

### Upgrading

**The first run after upgrading rebuilds the standard-module artifact**, and says so on stderr. It
takes about a second and happens once. This is not the usual reason — `lib/sysl` is byte-identical
to 0.0.5's — it is the cache-key change above, and it is *how* the link fix reaches a machine that
has already run 0.0.5. An upgrade that reused the old artifact would have fixed nothing.

### Installing

```
brew install edadma/tap/sysl
```

Or take the tarball below and extract it into a prefix — it carries `bin/sysl` and
`share/sysl/lib`, and the compiler finds its standard library relative to its own resolved path.
A binary on its own is a compiler that cannot compile.

macOS arm64 only. Every other platform builds from source.

**`llvm` is a runtime dependency**, not just a build one: sysl emits textual LLVM IR and shells out
to `clang` to assemble and link, and to `llvm-ar` to build a `.syslib`. Apple's command-line tools
ship clang but no `llvm-ar`.

## 0.0.5 — 2026-08-05

### Dependencies on other people's code

A project's `package.hocon` can now name dependencies, and `sysl build`, `run` and `test` fetch
what it names — no separate step, and a project with no dependencies does none of it.

```hocon
dependencies {
  json  { git = "github.com/edadma/sysl-json", version = "1.4.0" }
  regex { git = "github.com/edadma/sysl-regex", version = "0.4.0", mount = "re" }
  local { path = "../experiment" }
}
```

A coordinate is a git repository and a version is a tag on it. There is no registry, no account to
create and no name to reserve.

**Versions are chosen by Minimal Version Selection** — the highest minimum anybody asked for, not
the newest that exists. Adding a dependency cannot silently upgrade an unrelated one, builds are
reproducible without a lockfile, and upgrading is an edit to `package.hocon` rather than a command
that walks everything forward.

**A package's own top-level modules come in under their own names**, so an import line is the one
the library's own documentation shows. Two packages wanting one name is an error rather than a
silent winner, and `mount` says what one of them is called in your project. The collision check
includes your own modules, which is the common case.

**`sysl.sum` records what each package hashed to** and refuses a fetch that does not match — a tag
moved to point somewhere else, a repository rewritten, a mirror serving something other than what
was published. It is not a lockfile: version selection is already a function of the manifests.

**A package cannot run code at build time.** Not a hook, not a script, not a plugin. Most of what
other ecosystems need build scripts for is compiling vendored C, and sysl already does that
declaratively.

Reference: https://sysl.sh/reference/packages/

### Also in this release

- `Stdlib.resolve` — getting the standard module is now something a program embedding the compiler
  can ask for, rather than something private to the driver.
- The numbered specification moved from `docs/design/` to `design/`. It was never part of the
  website.

### Upgrading

**The first run after upgrading rebuilds the standard-module artifact**, and says so on stderr. That
is expected: the artifact is keyed by a fingerprint of the library, `lib/sysl` changed in this
release (`a781307f5813d388` → `8c4a5d00c415ca9f`), so the old one no longer answers to the new key.
It takes about a second and happens once.

### Installing

```
brew install edadma/tap/sysl
```

Or take the tarball below and extract it into a prefix — it carries `bin/sysl` and
`share/sysl/lib`, and the compiler finds its standard library relative to its own resolved path.
A binary on its own is a compiler that cannot compile.

macOS arm64 only. Every other platform builds from source.

**`llvm` is a runtime dependency**, not just a build one: sysl emits textual LLVM IR and shells out
to `clang` to assemble and link, and to `llvm-ar` to build a `.syslib`. Apple's command-line tools
ship clang but no `llvm-ar`.

## 0.0.3 — 2026-08-04

**The standard library ships as source now, and the compiler reads it off disk.** You can open it,
and you can edit it.

### Where it lives

An install puts the library at `share/sysl/lib` under the install prefix — on a Homebrew Mac that is
`$(brew --prefix)/share/sysl/lib` — and the compiler finds it from its own location, the way `rustc`
computes a sysroot. Running out of a checkout, it is the `lib/` directory in the tree. Nothing has to
be configured and there is no variable to set.

Until now the library's source was generated *into* the compiler, as a Scala object full of string
literals. That guaranteed something real — a compilation could not fail to find its library — and it
was bootstrap scaffolding rather than the design. A library nobody can open is not one anybody can
learn from, and the library is meant to be the worked example of what sysl is for. A compiler that
cannot be pointed at an edited copy is one whose library cannot be worked on at all.

So you can now do this:

```
$ cp -R $(brew --prefix)/share/sysl/lib ./mylib
$ # edit mylib/sysl/print.sysl
$ SYSL_LIB=./mylib sysl run prog.sysl
```

The artifact is keyed by a fingerprint of the library's contents, so an edited library **gets a cache
entry of its own** rather than picking up the shipped one. Nothing needs invalidating, and the
compiler you installed is unaffected.

`SYSL_LIB` is an escape hatch, not the mechanism — it is for a broken install and for working on the
library itself. If you never set it, nothing changes.

### What a broken install says

This is the guarantee being spent, so it is worth showing what replaces it:

```
sysl: error: cannot find the standard module's source, which every program is compiled against.
Looked for a library root holding 'sysl' at:
  /opt/homebrew/Cellar/sysl/0.0.3/share/sysl/lib (beside this compiler)
  lib (from the working directory)
  ../lib (from the working directory)
  ../../lib (from the working directory)

A sysl installed from a package has it beside the binary; one run out of a checkout finds it in the
tree. Set SYSL_LIB to the library root to name it outright.
```

Every path it tried, in the order it tried them.

### Install

```
brew install edadma/tap/sysl
brew upgrade sysl        # if you already have 0.0.2
```

**This upgrade rebuilds the standard-module artifact once**, which 0.0.2 did not. The library's own
source changed in this release — a file's capability clauses are written as attributes now — so its
fingerprint moved and the compiler builds the artifact for the new one on first use. That is the
keying working as designed, not a regression; it takes well under a second and it happens once.

Still macOS on Apple silicon only. Every other platform builds from source — see
[Installation](https://sysl.sh/getting-started/installation/).

## 0.0.2 — 2026-08-04

sysl can now be asked to **prove** what its contracts say, rather than only to check them while
running.

### Verification

[Contracts](https://sysl.sh/tour/contracts/) already gave sysl `require`, `ensure` and `invariant`,
and every one of them was a branch and a trap. `sysl prove` translates a module to WhyML and hands
it to [Why3](https://www.why3.org/), which discharges the obligations with whichever prover is
configured:

```
$ sysl prove half.sysl
Goal half'vc.
Prover result is: Valid (0.01s, 38 steps).

every goal was discharged
```

With it comes the vocabulary a *specification* needs that an executable condition does not supply on
its own:

- **`for all` and `for some`** — quantifiers over an integer range. They are ordinary `bool`s, usable
  anywhere one is, and `all` and `some` remain ordinary identifiers everywhere else.
- **Loop invariants and termination measures** — what a loop preserves, and what it decreases.
- **`@pure`** — a function a specification may call, checked to be one.
- **`@ghost`** — the one thing that exists for the prover and is not compiled.

**One clause means one thing.** The prover and the running program read the same sentence: there is
no proof-only build, no specification subset the compiler declines to execute, and a check the prover
proves redundant is still compiled. `@ghost` is the single exception, and it is legible in the source
rather than in a flag.

Staying inside an integer's range is a proof obligation by default; `--overflow ignore` drops those
while you reason about the rest of a function. `--emit-whyml` prints the translation instead of
proving it.

Proving is opt-in and needs Why3 installed — it goes through opam, not Homebrew. Without it, `sysl
prove` says so and names the command; nothing else in the compiler is affected.

The full surface is on the new [Verification](https://sysl.sh/reference/verification/) page.

### `sysl --help` answers

0.0.1 shipped without it:

```
$ sysl --help
Error: Unknown option --help
```

The usage text was always reachable — running `sysl` with no subcommand prints it — but only by
*failing*, on stderr with a status of 2. Asked for, it is not an error: it goes to stdout with a zero
status, so `sysl --help | less` works and a script checking the status is not told something went
wrong. A bare `sysl` still fails, because that really is a mistake.

### The standard-module artifact is published by rename

Its path is keyed by a fingerprint of the library, so every compilation of the same library on a
machine finds the same file — which means two builds can be assembling it at once. It is now put in
place by a rename rather than written where it lies, so a reader gets the whole of one artifact or
the whole of the other, and a rebuild that fails leaves the one that was already there.

### Install

```
brew install edadma/tap/sysl
brew upgrade sysl        # if you already have 0.0.1
```

Still macOS on Apple silicon only. Every other platform builds from source — see
[Installation](https://sysl.sh/getting-started/installation/).

The standard-module artifact is keyed by the library rather than by the compiler version, and the
library did not change in this release, so upgrading does not rebuild it.

## 0.0.1 — 2026-08-04

The first binary release of sysl — a ref-counted systems language that compiles through LLVM to a
real native executable. No garbage collector, no borrow checker, and four ways to name storage.

### Install

```
brew install edadma/tap/sysl
```

That brings LLVM with it, which sysl needs at runtime: the compiler emits textual LLVM IR and hands
it to `clang` to assemble and link, and `llvm-ar` builds a library into a `.syslib`.

```
sysl --version
sysl run hello.sysl        # compile and run
sysl build hello.sysl -o hello   # compile to an executable and stop
```

### What is here

A whole compiler, and rather more language than a `0.0.1` usually implies — the version number is
about the *distribution* being new, not the language. Structs, enums with payloads and pattern
matching, traits with defaults and generics with bounds, closures, contracts (`requires` / `ensures`
/ struct invariants), inline assembly, a C FFI, and a standard library covering strings, text,
buffers, files, time, threads, synchronisation, regular expressions, and mathematics including a
complex-number module.

Reference-counted memory with escape analysis and no GC pause. Nine build targets across macOS,
Linux and Windows, three of them freestanding. An executable-documentation harness, so every program
printed on
[sysl.sh](https://sysl.sh/) is compiled and run by the test suite before it is published.

### Platform

**macOS on Apple silicon only.** That is what the author's machine can build; a Linux binary needs a
build runner and is not here yet. Every other platform builds from source, which is a clone and one
sbt invocation — see [Installation](https://sysl.sh/getting-started/installation/).

### Notes

The standard module ships inside the compiler as source and is built into a linkable artifact on
first use — announced on stderr, in well under a second. It is cached in `~/Library/Caches/sysl/`
under a fingerprint of the library it was built from, so it is built once per machine rather than
once per project, nothing is written into your source tree, and a future sysl carrying a different
library gets its own entry rather than a stale hit. Everything under that directory is derived:
deleting it costs one rebuild.

This is a first release of a young language. Expect sharp edges, and please report them.
