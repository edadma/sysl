#!/usr/bin/env python3
"""Splits the test suites into the groups `run-gate.sh` runs, one `sbt` per group.

Why the suite is run in groups at all is in `run-gate.sh`'s header. This file answers what follows
from it: which suites need a group to themselves, and how the rest are divided.

**WHICH SUITES ARE HEAVY CANNOT BE DERIVED FROM THE SOURCE, and it was worth trying before
accepting that.** The property that matters is building or booting for every registered target --
parsing and compiling the standard library once per target -- and nothing in the text of a suite
says so. Iterating the registry is not the tell in either direction: `AbiAgainstClangTests` walks
`Target.all` in the same `for` as `CrossTargetBuildTests` and is perfectly cheap, because it only
emits IR and never reaches the toolchain; while `QemuRunTests` and `NoAllocEmissionTests` are heavy
and never mention `Target.all`, naming their two boards outright. Eight suites iterate the registry
and four are heavy, with no textual difference to separate them.

So `HEAVY` below is a **measured** list, and the guard against it going stale is not this file --
it is the watchdog in `run-gate.sh`. A suite that becomes heavy and is not listed here lands in a
chunk, wedges it, and is killed and named in the summary as the last suite that group announced.
That is the failure being designed for rather than prevented, which is the honest arrangement when
the classification cannot be computed.

**A suite that needs a group needs `HEAVY`, and there is deliberately no second kind.** A middle
setting was tried on 2026-08-28 -- a chunk of one at the light heap, for a suite whose cost looked
like retention rather than isolation -- and it OOM'd, because a suite big enough to want its own
group is big enough to want the big heap too. `HEAVY` is the answer; what the episode changed is
`run-gate.sh`, which now gives each of them a group rather than one between them.

What this file does do is print the suites that iterate the registry and are not classified either
way, as candidates worth measuring -- advisory only, and deliberately not acted on.

Emits `heavy.json` and `chunks.json` into the output directory.
"""

import json
import os
import re
import sys

# Measured on an 18-core, 64 GB machine, 2026-08-08: each of these wedges a chunk that four agents
# share at 12g, and passes alone at 24g. They are **serialized** as well as isolated -- one agent --
# which is what a suite that builds for every target needs.
#
# **THE "about thirty seconds" THIS COMMENT USED TO CLAIM IS SIX RELEASES STALE.** Measured
# 2026-08-28 at `SYSL_RELEASE=1`, one group each: `CrossTargetBuildTests` 118 tests in **9:00**,
# `NoAllocEmissionTests` 6 tests in **7:10** (seventy-two seconds a test), `QemuHarnessTests` 30 in
# **9:17**, `QemuRunTests` 61 in **2:09**. The four are ~27 minutes between them, against a "4:15 for
# all four" recorded at 0.0.79 three days earlier. Nothing here caused it and nothing here can see
# it; what the spread tracks is how often a suite compiles the standard library *per target*, which
# is the same property this list is defined by.
#
# `ConditionalTests` joined 2026-08-28, card `0324`. **It took two wrong answers to get here and both
# are worth knowing**, because each looked right:
#
#   - It was put here first, and the next gate's combined heavy group ran **19 minutes with `oom=0`**
#     against a 900-second limit. That looked like the wrong home and was the wrong *grouping*:
#     `run-gate.sh` ran all five in one `sbt`, so `LIMIT` was a budget for the set. Fixed there.
#   - It was then given a chunk of its own at the light settings, on the reading that its cost was
#     retention rather than a need to be alone. **That OOM'd**: 16g is not enough for it whatever
#     else is in the agent, so the light heap was never the answer.
#
# What it actually needs is this list's settings, in a group of its own: **89 tests, 11:58, at 24g
# and one agent**, measured as that chunk's retry. That fits the 900-second limit with about three
# minutes to spare, which is thin -- if it grows, raise `LIMIT` rather than re-deriving this.
# `StdCacheBoundTests` joined 2026-08-30, card `0375`, and it is the first entry measured **per
# suite** rather than inferred from a chunk. The chunk it was in produced no verdict twice and the
# reading taken at the time was that nine suites accumulate about 2 GB each -- which is what running
# them in groups of three, six and nine suggests, and it is wrong. Run one at a time at 24g and one
# agent, on an idle box, the nine are:
#
#   StdCacheBoundTests            24.0 GB   44 s
#   QuantifierTests                6.1 GB   12 s
#   AssociatedGenericRunTests      6.1 GB   10 s
#   ImplGenericRunTests            6.1 GB    9 s
#   EnumRunTests                   6.1 GB    8 s
#   TraitObjectCodegenTests        6.1 GB    8 s
#   CompoundConstrainedRunTests    4.8 GB    8 s
#   MemberAttrTests                4.7 GB    7 s
#   VersionCliTests                    --    3 s   (forks no agent)
#
# **One suite reaches the whole ceiling and the other eight are a quarter of it.** The group of
# three measured 6.1 GB because it was three of the light ones; the linear-looking curve was an
# artifact of which group happened to contain this suite. Eight unrelated suites landing on two
# values within 20 MB of each other is a heap size rather than a live set, which is the other half
# of why the per-suite number is the one to trust.
#
# The cost is the suite's whole purpose rather than a defect in it: it sweeps
# `Target.all.filter(_.supported).take(4)` across three tests, and `Std.parsed` clears its slot
# before each insert, so it forces repeated full re-parses of the standard library -- which is
# exactly the property this list is defined by, arrived at from the memo side instead of the
# toolchain side.
#
# **Lowering `SUITES_PER_CHUNK` would not have fixed it.** At 24.0 GB against a 24 GB ceiling, any
# chunk holding this suite and anything else is over, so the lever is isolation and not size.
HEAVY = {
    'sh.sysl.CrossTargetBuildTests',
    'sh.sysl.QemuRunTests',
    'sh.sysl.QemuHarnessTests',
    'sh.sysl.NoAllocEmissionTests',
    'sh.sysl.ConditionalTests',
    'sh.sysl.StdCacheBoundTests',
}

# Measured and found cheap despite walking the registry. Listed so that "it iterates and is not
# heavy" is a recorded finding rather than an omission.
LIGHT_SWEEPERS = {
    'sh.sysl.AbiAgainstClangTests',
}

ITERATES = re.compile(r'for\s+\w+\s*<-\s*Target\.all')
STYLE = re.compile(r'Any(FreeSpec|FunSuite|WordSpec|FlatSpec|FunSpec|PropSpec|FeatureSpec)')

# Measured 2026-08-21, cutting 0.0.66. Both numbers moved together and the reason is one fact:
# **the chunks were contiguous slices of an alphabetically sorted list, and in this tree a shared
# prefix means a shared kind.** So one chunk held all nine `Codegen*` suites and five `*RunTests`
# beside them -- every program-compiling suite in the run, in one group of 23, sharing four agents
# at 12g. It wedged, was killed at the watchdog, and the gate read RED with zero test failures.
#
# Dealing round-robin instead of slicing is what fixes it, and raising the count is not: a bigger
# count still puts `Codegen*` next to `Codegen*`, so the next family of twenty similarly-named
# suites clusters exactly as this one did.
#
# **Round-robin alone was measured and was not enough**, which is the second half of the finding: at
# 18 groups of 17-18 the run cleared five chunks and then wedged again on a group whose suites had
# nothing in common, so what remained was not lopsidedness but the pile itself. Growth is monotonic
# per agent within one `sbt`, so the lever that works is fewer suites per invocation and raising the
# heap cap only postpones it.
#
# The pile got heavier that release for a reason worth recording: `sysl.container` added five
# modules to `library/sysl`, and every program a suite compiles links the standard library. Nothing
# here could notice that, which is why the number is measured rather than derived.
#
# **THIS WAS A FIXED CHUNK *COUNT* UNTIL 2026-08-25, WHICH MEANT THE MEASURED NUMBER DECAYED.** The
# comment above it read "36 groups is ~9 suites each", and nine is the figure that was measured; the
# thirty-six is an artifact of how many suites existed the day it was written. Suites are added
# steadily, so a fixed count silently grows the chunks: 355 light suites over 36 groups is **~10**,
# and at 500 it would be 14 -- the gate walking back toward the cliff with nothing to say so.
#
# 0.0.79 is where that showed. Two chunks were killed and both passed alone, and the release before
# it had none. So the size is what is fixed now and the count is derived, which keeps the measured
# figure true as the tree grows instead of only on the day somebody last looked.
#
# User, 2026-08-25, arriving at the same lever from the symptom: *"maybe those chunks just need to
# be broken up"*.
SUITES_PER_CHUNK = 9


def suites(root):
    """Every concrete ScalaTest suite under `root`, as fully qualified name -> source.

    A suite is one whose extends-clause names a ScalaTest style **or a local base that does** --
    and the second half is not a nicety. `ResolveTests` and `PackageBuildTests` both extend
    `PackageCacheSupport`, a trait in this tree that extends `AnyFreeSpec` itself, and a pattern
    matching only the styles missed both of them: the whole package manager, end to end, was outside
    the gate and nothing said so. Found 2026-08-23 when a change to it made a suite fail that the
    gate had just called green.

    So the local bases are collected first and the match is closed over them, repeatedly, until it
    stops growing -- a support trait may extend another one, and one level of following would be the
    same bug one layer down.
    """
    sources = {}

    for dirpath, _, files in os.walk(root):
        for f in files:
            if not f.endswith('.scala'):
                continue

            src = open(os.path.join(dirpath, f), errors='replace').read()
            pkg = re.search(r'^package\s+([\w.]+)', src, re.M)

            if pkg:
                sources[os.path.join(dirpath, f)] = (pkg.group(1), src)

    # Every `trait`/`abstract class`/`class` declared here whose own extends-clause is suite-shaped,
    # by name, so that a subclass of one is recognised as a suite too.
    bases = set()
    # `[^{]*?` rather than `[^\n]*?`, so a declaration whose `extends` is on the next line is seen —
    # `ArgumentTests` and `ExternVarTests` are written that way, and the class matcher below spans the
    # newline because `\s+` does. A base collected by a stricter pattern than the thing it feeds would
    # be the same bug one shape along.
    decl = re.compile(r'^(?:abstract\s+)?(?:class|trait)\s+(\w+)[^{]*?\bextends\s+([^\n{]+)', re.M)

    while True:
        before = len(bases)

        for _, src in sources.values():
            for m in decl.finditer(src):
                if STYLE.search(m.group(2)) or any(re.search(rf'\b{b}\b', m.group(2)) for b in bases):
                    bases.add(m.group(1))

        if len(bases) == before:
            break

    found = {}

    for pkg, src in sources.values():
        for m in re.finditer(r'^class\s+(\w+)\s+extends\s+([^\n{]+)', src, re.M):
            if STYLE.search(m.group(2)) or any(re.search(rf'\b{b}\b', m.group(2)) for b in bases):
                found[f'{pkg}.{m.group(1)}'] = src

    return found


def deal(light):
    """The light suites dealt round-robin into chunks of at most `SUITES_PER_CHUNK`.

    Round-robin rather than contiguous slices, so that suites sharing a prefix -- which here means
    sharing a kind, and therefore a cost -- are spread across the groups instead of piled into one.

    **The count is derived from the size rather than the other way round.** A fixed count held the
    measured suites-per-chunk only on the day it was written; see `SUITES_PER_CHUNK`. `-(-a // b)` is
    the ceiling, so the last chunk is the short one and no chunk ever exceeds the measured size.

    It is a function rather than four lines in `main` so that the self-test can assert the real
    thing. Asserting a copy of the arithmetic would pass whatever `main` went on to do, which is the
    shape of check this file already has one lesson about.
    """
    count = max(1, -(-len(light) // SUITES_PER_CHUNK))

    return [light[i::count] for i in range(min(count, len(light)))]


def self_test():
    """The matcher, against the shape that defeated it.

    **A suite extending a base declared in this tree was invisible until 2026-08-23**, and sixteen of
    them were — the whole package manager among them. This asserts the two shapes that matter: a
    subclass of a local support trait is found, and a subclass of a base that is *not* suite-shaped is
    not. It runs before every gate because it costs nothing and the bug it pins reads as a smaller
    number in a line nobody checks.
    """
    import tempfile

    tree = tempfile.mkdtemp(prefix='gate-selftest-')
    write = lambda name, text: open(os.path.join(tree, name), 'w').write(text)

    write('Support.scala', 'package sh.probe\n\ntrait CacheSupport extends AnyFreeSpec with Matchers {\n}\n')
    write('Wrapped.scala', 'package sh.probe\n\ntrait Deeper extends CacheSupport {\n}\n')
    write('Plain.scala', 'package sh.probe\n\nclass PlainTests extends AnyFreeSpec with Matchers {\n}\n')
    write('Local.scala', 'package sh.probe\n\nclass LocalTests extends CacheSupport {\n}\n')
    write('Nested.scala', 'package sh.probe\n\nclass NestedTests extends Deeper {\n}\n')
    write('Helper.scala', 'package sh.probe\n\ntrait NotASuite extends AnyRef {\n}\n\nclass NotATest extends NotASuite {\n}\n')

    # A base whose `extends` is on the NEXT line, which is how `ArgumentTests` and `ExternVarTests`
    # are written — a base collected by a stricter pattern than the class matcher would be the
    # same bug one shape along.
    write('Wrapped2.scala', 'package sh.probe\n\ntrait Broken\n    extends AnyFreeSpec\n    with Matchers {\n}\n')
    write('Late.scala', 'package sh.probe\n\nclass LateTests extends Broken {\n}\n')

    got = set(suites(tree))
    want = {'sh.probe.PlainTests', 'sh.probe.LocalTests', 'sh.probe.NestedTests', 'sh.probe.LateTests'}

    if got != want:
        sys.exit(f'gate-groups self-test failed: found {sorted(got)}, wanted {sorted(want)}')

    print('  self-test: a suite extending a local base is found, and a non-suite is not')

    # **The sizing, which is the half that used to decay silently.** A fixed chunk *count* held the
    # measured suites-per-chunk only on the day it was written, and 0.0.79 was killed twice on a
    # tree that had grown into ~10 per chunk against a measured 9. This asserts the property that
    # replaced it -- no chunk exceeds the target, at any tree size -- which is exactly what a fixed
    # count cannot promise and what no gate run would report until it wedged.
    for n in (1, 8, 9, 10, 36, 355, 500, 5000):
        light = [f'sh.probe.S{i}' for i in range(n)]
        chunks = deal(light)

        widest = max(len(c) for c in chunks)
        placed = sorted(s for c in chunks for s in c)

        if widest > SUITES_PER_CHUNK:
            sys.exit(f'gate-groups self-test failed: {n} suites gave a chunk of {widest}, '
                     f'over the measured {SUITES_PER_CHUNK}')

        # A round-robin deal that dropped or duplicated a suite would be a hole in the gate of
        # exactly the kind the reconciliation in `run-gate.sh` exists to catch -- assert it here too,
        # where it costs nothing and does not need sbt.
        if placed != sorted(light):
            sys.exit(f'gate-groups self-test failed: {n} suites in, {len(placed)} out')

    print(f'  self-test: no chunk exceeds {SUITES_PER_CHUNK} suites, and none is lost, at any size')


def main():
    if len(sys.argv) == 2 and sys.argv[1] == '--self-test':
        return self_test()

    if len(sys.argv) != 3:
        sys.exit(f'usage: gate-groups.py <test-source-root>[{os.pathsep}<test-source-root>...] <output-dir>')

    self_test()

    # More than one root as of `native/src/test/scala`: a suite that reaches for `java.io.File` or a
    # real child process, as a test of a native-only mechanism against the linked binary genuinely
    # must, cannot live in `shared/src/test` at all -- Scala.js's `javalib` has neither, so a shared
    # spec that used them would fail to *compile* on JS rather than merely being skippable there. The
    # comment this replaces predicted exactly this: "the moment anybody adds native/src/test/scala
    # the proxy diverges quietly, in the direction that reads as fine" -- found true 2026-09-07 by
    # the reconciliation check two paragraphs down doing precisely the job it was built for.
    roots, out = sys.argv[1].split(os.pathsep), sys.argv[2]
    found = {}

    for root in roots:
        found.update(suites(root))

    heavy = sorted(HEAVY & found.keys())
    light = sorted(found.keys() - set(heavy))

    for name in sorted(HEAVY - found.keys()):
        print(f'  WARNING: {name} is recorded as heavy but no longer exists -- prune it.')

    unclassified = {n for n, src in found.items() if ITERATES.search(src)} - HEAVY - LIGHT_SWEEPERS

    for name in sorted(unclassified):
        print(f'  NOTE: {name} iterates the target registry and has not been measured.')

    if unclassified:
        print('        Not acted on: iterating is not what makes a suite heavy. If a chunk times')
        print('        out the summary names the suite; if it OOMs across successive runs the chunk')
        print('        is the finding, and the suite belongs in HEAVY.')

    chunks = deal(light)

    os.makedirs(out, exist_ok=True)
    json.dump(heavy, open(os.path.join(out, 'heavy.json'), 'w'), indent=1)
    json.dump(chunks, open(os.path.join(out, 'chunks.json'), 'w'), indent=1)

    # The largest chunk is printed because it is the number that actually bounds a group's peak, and
    # because it is what silently grew while the count was fixed. A reader who sees it climb past the
    # measured size has the warning that was missing for four releases.
    widest = max((len(c) for c in chunks), default=0)

    print(f'  {len(found)} suites: {len(heavy)} run alone, '
          f'{len(light)} in {len(chunks)} chunks of at most {widest} (target {SUITES_PER_CHUNK})')


if __name__ == '__main__':
    main()
