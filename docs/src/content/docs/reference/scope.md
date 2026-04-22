---
title: Reference overview
description: How the Sysl language reference is organised — types, values, syntax, semantics.
sidebar:
  order: 0
---

import { CardGrid, LinkCard } from '@astrojs/starlight/components';

The language reference is the normative description of Sysl. If the implementation disagrees
with a page here, one of the two is wrong — file a bug either way.

For a gentler introduction, see the [language tour](/guides/tour/). For the full source of
truth, the canonical reference text lives at
[`trisc/sysl-reference.md`](https://github.com/edadma/trisc/blob/dev/sysl-reference.md);
these pages are its friendlier, cross-linked twin.

## How it's organised

<CardGrid>
  <LinkCard
    title="Types"
    description="Scalar types, composites, structs, tagged unions, range-constrained types, type attributes, allocation modes, volatile."
    href="/reference/types/"
  />
  <LinkCard
    title="Expressions"
    description="Literals, operators, precedence, casts, compound assignment, chained comparisons, if-is, destructuring."
    href="/reference/expressions/"
  />
  <LinkCard
    title="Statements and control flow"
    description="if / elif / else, match, while, do-while, for (C-style and range-based), break / continue, loop labels."
    href="/reference/statements-and-control-flow/"
  />
  <LinkCard
    title="Functions"
    description="Bodies, defaults, named args, def, generics, contracts, closures, extern, calling convention."
    href="/reference/functions/"
  />
  <LinkCard
    title="Arrays, slices, and pointers"
    description="Fixed arrays, slices, ref-counted heap arrays, raw pointers, pointer arithmetic, chained indexing."
    href="/reference/arrays-slices-pointers/"
  />
  <LinkCard
    title="Strings"
    description="Fat-pointer layout, interpolation, format strings, str(), construction from bytes, escapes."
    href="/reference/strings/"
  />
  <LinkCard
    title="Generics"
    description="Type parameters on functions, structs, and enums. Trait bounds. Monomorphisation."
    href="/reference/generics/"
  />
  <LinkCard
    title="Traits and operators"
    description="Trait declarations, impl blocks, default methods, and the operator-to-trait mapping."
    href="/reference/traits-and-operators/"
  />
  <LinkCard
    title="Modules and imports"
    description="Module declarations, import forms, visibility, name mangling."
    href="/reference/modules-and-imports/"
  />
  <LinkCard
    title="Builtins and runtime"
    description="Builtin functions, trap codes, runtime safety, calling convention."
    href="/reference/builtins-and-runtime/"
  />
  <LinkCard
    title="Attributes and testing"
    description="#test, #pure, #deprecated, #inline, conditional compilation, test discovery."
    href="/reference/attributes-and-testing/"
  />
  <LinkCard
    title="Literate Sysl"
    description="`.lsysl` files — Markdown prose interleaved with indented Sysl code blocks."
    href="/reference/literate-sysl/"
  />
</CardGrid>
