#!/bin/zsh
#
# The Native gate: the whole test suite, on the platform a release ships.
#
#     ./run-gate.sh
#
# WHY THIS IS A SCRIPT AND NOT `sbt syslNative/test`
#
# Because that command cannot finish here, at any setting, and the reason is structural rather than
# a matter of tuning. sbt forks one test agent per core and keeps each one for the whole run, so a
# single invocation accumulates the entire suite's retained memory in a handful of processes. Raise
# the heap ceiling and the total overruns the machine; lower it and an agent reaches the ceiling --
# where Scala Native's collector does not abort but **spirals**, collecting and reclaiming nothing,
# at around 80% CPU, for as long as it is left alone. One agent burned 93 minutes of CPU that way
# having printed nothing for two hours, which is indistinguishable from a slow test.
#
# Running one `sbt` per group of suites recycles the agents between groups, and that is the whole
# trick. Measured 2026-08-08 on an 18-core, 64 GB machine: 7798 tests, 0 failures, 7m17s.
#
# THIS IS A WORKAROUND AND SHOULD BE READ AS ONE. The underlying fact is that the suite needs
# roughly twelve times the memory on Native that it needs on the JVM, where the same 7798 tests fit
# in `-Xmx8g`. Nobody has explained the gap. If it is ever closed, delete this script.
#
# EVERY GROUP HAS A HARD TIME LIMIT, because the failure mode above is silence, and silence looks
# exactly like progress. A group that exceeds it is killed and recorded together with the last suite
# it announced, which is what names the offender.
#
# **A KILLED GROUP IS RE-RUN BEFORE IT IS BELIEVED, AND THAT IS NEW AS OF 2026-08-25 (card 0270).**
# A kill produces **no verdict** -- which is a reason to run the group again, not a reason to call
# the gate red. It is retried once, alone, at the heavy settings, and only a second failure is a
# `TIMEOUT`. A group that ran and *reported failures* is never retried: the empty result is the
# distinction, and retrying a real failure is how a gate stops meaning anything.
#
# Cutting 0.0.79 this read `GATE: RED` with **9926 succeeded, 0 failed** and two killed chunks, both
# of which passed untouched when re-run by hand. That is a false red, and a false red is worse than a
# slow gate because it teaches the next one to be discounted.

set -u

REPO=${0:a:h}
LOGS=$REPO/target/gate
# Two roots as of `HeapCeilingTests`: a suite exercising a native-only mechanism against the linked
# binary needs `java.io.File` and a real child process, neither of which Scala.js's `javalib`
# carries, so it cannot live in `shared/src/test` at all -- `gate-groups.py` now takes a
# colon-separated list and merges what each root finds.
SUITES=$REPO/shared/src/test/scala:$REPO/native/src/test/scala

HEAVY_HEAP=32g;  HEAVY_AGENTS=1     # a suite that builds for every target, on its own -- 24g was
                                    # not enough as of 2026-09-04; see below
LIGHT_HEAP=18g;  LIGHT_AGENTS=2     # 2 x 18g = 36 GB; agents lowered from 3 and heap raised
                                    # from 16g, both on 2026-09-04 -- see below
LIMIT=900                           # seconds per group; the groups take 15-75s
OOM_GRACE=60                        # after an agent announces an OOM -- see `attempt_group`

# **WHY 3 x 16g RATHER THAN 4 x 12g, measured 2026-08-22 cutting 0.0.66.** A wedged chunk was caught
# in the act: four agents sat at **4.3 GB** each and the fifth at **exactly 12.0 GB**, which is the
# cap. The machine was nowhere near full at the time. So the agent that died did not run out of
# memory -- it ran out of *permission*, while tens of gigabytes sat unused beside it.
#
# The ceiling is unchanged at 48 GB, which is what protects the machine. What changed is how it is
# divided: an agent that balloons now has 16 GB to balloon into rather than 12, and the ordinary
# agents never wanted more than about 4.3 anyway. Fewer agents also means fewer of them able to
# balloon at once, which is the case the ceiling exists for.
#
# **Neither this nor the grouping is a fix.** Something makes one agent occasionally need three times
# what its neighbours need, and nobody has explained that -- see the note above about Native wanting
# twelve times the JVM's memory for the same code. The most promising account is that Scala Native's
# collector scans the stack conservatively, so garbage that a stack word happens to look like a
# pointer to cannot be proven dead; a fresh process is then the only thing that reclaims it, which is
# exactly what this script provides and why it works at all.
#
# **WHY HEAVY IS 32g RATHER THAN 24g, measured 2026-09-04.** `ConditionalTests` produced no verdict
# twice in one gate and was recorded `TIMEOUT/KILLED`. Re-run alone on a box with no other JVM and no
# orphaned agent, at 24g and one agent, it reproduced rather than passing -- so it is the ceiling and
# not contention, which is the reading the retry mechanism exists to rule out:
#
#     [ScalaNative GC|Error] Out of heap space grow heap
#       at Heap_exitWithOutOfMemory / LargeAllocator_Alloc
#       at scala.collection.mutable.HashMap.growTable
#       at scala.util.parsing.combinator.PackratParsers$$anon$3.apply
#
# It dies growing the **packrat memo table**, which is the parser's and scales with what it parses --
# and this suite parses the standard library once per target. RSS sampled every 15 s ran 2.0 -> 16.2
# -> a plateau near 18.0 -> 20.5 GB. Immix grows by x1.414 and has no shrink path, so the grow from
# ~17 GB asks for ~24 and overshoots. At 32g the same suite passes: **90 tests, 0 failed, 12:01**.
#
# **The failure presents as a HANG rather than as an OOM**, which is why the summary says
# `TIMEOUT/KILLED` and not `oom`. `Heap_exitWithOutOfMemory` printed and the process did not exit --
# sbt sat waiting on a 32 MB corpse until `LIMIT` cut it. So a hand-run of a heavy suite needs its
# own watchdog; the gate has one and a bare `testOnly` does not.
#
# One agent on a 64 GB machine, so 32g is safe on its own.
#
# **WHY LIGHT IS NOW 2 x 16g RATHER THAN 3, measured 2026-09-04 on the same day HEAVY went to 32g.**
# The gate was killed by the machine **three times, at the same place each time**: entering
# `chunk-0`, which is the first group to run the full `syslNative` test binary at the light
# settings. Every run got all six heavy groups green first, and the `doc` group with them — 350
# tests, 0 failed — so the ask that fails is 3 x 16g and the ask that survives, repeatedly, is
# HEAVY's 1 x 32g.
#
# **The 48 GB ceiling stopped being affordable rather than stopping being correct.** It was measured
# on 2026-08-22 against a library that has since grown, and the same growth is what pushed HEAVY
# from 24g to 32g in the paragraph above — an agent that used to want 4.3 GB wants more now, so
# three of them reach for the ceiling together where they used to sit well under it. 2 x 16g is 32
# GB, the same peak the heavy groups demonstrably survive on this machine.
#
# **What it costs is wall clock and nothing else**, spread over the 43 chunks — the chunks are
# independent, so two agents finish the same work in about half again the time.
#
# **The diagnosis took three runs because the first two attributions were wrong**, and both were
# guesses that fitted the evidence rather than things anybody had shown: `ConditionalTests` (it was
# green every time) and desktop memory pressure (freeing ~22 GB of compressor got the run seven
# groups further and it died in the same place). What settled it was reading which groups had
# **finished** — the summary names them — rather than reasoning about which one looked expensive.
#
# **THEY DID NOT PASS WITH ROOM TO SPARE, AND THE ANSWER IS THE OPPOSITE WAY ROUND: THE HEAP GOES
# UP, NOT THE AGENT COUNT BACK UP.** The paragraph this replaces asked for the peak so that 3 x 12g
# could be judged rather than guessed at. It was sampled across a full gate on 2026-09-04, and what
# it showed is that 16g is itself marginal now:
#
#   - **nine of the 43 chunks announced an OOM at 16g and every one passed alone at 32g** --
#     chunk-0, 2, 3, 7, 8, 10, 12, 13 and 33. `RETRIED-OK`, `failed=0`, so the retry mechanism was
#     carrying nine chunks rather than the one or two it exists for;
#   - a single agent under a **16g** heap was seen at **17.5 GB resident**, so heap plus the
#     runtime's own overhead is about **1.1x** the cap and an agent at the ceiling is really asking
#     the machine for a sixth again as much;
#   - the largest concurrent total of all `sysl-test` processes at any point was **42.1 GB**.
#
# So 3 x 12g is answered: it would give each agent *less* room than the setting that is already
# OOMing, and buy a third agent with it. 2 x 18g is 36 GB projected resident and leaves the ceiling
# near 44 GB alone -- the smallest rise that is certainly affordable, chosen over 2 x 20g for that
# reason. A chunk that still wants more has the retry, which demonstrably works.
#
# **The sampler counts PROCESSES rather than agents and cannot separate a light chunk from a
# retry-alone one** -- it read `n=2` during a heavy group configured `agents=1` -- so the per-agent
# and total figures above are phase maxima rather than a clean light peak. The nine-chunk count is
# the load-bearing evidence and needs no sampler at all: it is read off `summary.txt`.

mkdir -p "$LOGS"
SUMMARY=$LOGS/summary.txt
rm -f "$SUMMARY"

# **THE WATCHDOGS, AGAINST A FAKE `sbt`, BEFORE EVERY GATE.** `gate-groups.py` self-tests its matcher
# for the same reason and it is the stronger reason here: a watchdog that has quietly stopped working
# announces itself by the gate taking fifteen minutes longer, which is indistinguishable from a slow
# machine. Ten seconds, and it pins both directions.
#
# `local` is dynamically scoped in zsh, so the shortened limits and the fake PATH are visible to
# `attempt_group` without it knowing it is being tested -- the real function is what runs.
# **THE TWO HALVES MUST USE DIFFERENT LIMITS, AND THE FIRST VERSION OF THIS DID NOT — SO IT PASSED
# AGAINST THE VERY BEHAVIOUR IT WAS WRITTEN TO CATCH.** It ran both halves at `LIMIT=4` and asserted
# the OOM case finished within it. But the OUTER watchdog also fires at 4s, so a build with the inner
# one deleted came back at exactly 4s and satisfied `elapsed > LIMIT` being false. Verified by
# deleting it: the test said PASS.
#
# So the OOM half runs with the outer limit set far away, and asserts the cut happened nowhere near
# it. The silent half keeps a short limit, and asserts the run was NOT cut before it.
watchdog_self_test () {
  local tree=$(mktemp -d -t gate-watchdog)
  local LOGS=$tree/logs PATH=$tree/bin:$PATH
  local LIMIT OOM_GRACE start elapsed

  mkdir -p "$tree/bin" "$LOGS"

  # Announces an OOM, then spirals: never exits, never prints a result line.
  print '#!/bin/zsh\nprint "[info] ProbeTests:"\nprint "\tat Heap_exitWithOutOfMemory"\nsleep 600' > "$tree/bin/sbt"
  chmod +x "$tree/bin/sbt"

  LIMIT=60 OOM_GRACE=2       # the outer limit is a long way off, so only the inner one can explain a fast cut
  start=$SECONDS
  attempt_group "oom.log" 16g 3 syslNative "sh.probe.Whatever"
  elapsed=$(( SECONDS - start ))

  (( elapsed > 15 )) && {
    print "gate watchdog self-test failed: an announced OOM took ${elapsed}s, so it waited for the ${LIMIT}s limit"
    rm -rf "$tree"; return 1
  }

  # The control, and it is the half that makes the first one mean something: a group that hangs
  # SILENTLY must still cost the outer limit. Without this, a watchdog that killed everything on
  # sight would pass the test above and destroy the gate.
  print '#!/bin/zsh\nprint "[info] QuietTests:"\nsleep 600' > "$tree/bin/sbt"

  LIMIT=5 OOM_GRACE=2
  start=$SECONDS
  attempt_group "quiet.log" 16g 3 syslNative "sh.probe.Whatever"
  elapsed=$(( SECONDS - start ))

  (( elapsed < LIMIT )) && {
    print "gate watchdog self-test failed: a silent hang was cut after ${elapsed}s, before the ${LIMIT}s limit"
    rm -rf "$tree"; return 1
  }

  rm -rf "$tree"
  print "  self-test: an announced OOM is cut at once, and a silent hang still costs the limit"
}

echo "grouping the suites" | tee -a "$SUMMARY"
python3 "$REPO/gate-groups.py" "$SUITES" "$LOGS" | tee -a "$SUMMARY" || exit 1

# **RECONCILE WHAT THE GROUPER FOUND AGAINST WHAT sbt SAYS EXISTS.** The grouper reads source text,
# and a count printed against nothing is not evidence: it said 334 suites for months while there were
# 350, because a suite extending a base declared in this tree did not match its pattern — the whole
# package manager among the sixteen it could not see. A fix alone would hold until the next support
# trait; this is what makes it unable to recur.
#
# **Asked of the platform being gated**, not of the JVM. Two test source roots as of
# `native/src/test/scala` (`SUITES`, above) rather than one, so `syslJVM/Test/definedTestNames` would
# now be a cheaper proxy that quietly missed everything native-only — exactly the divergence this
# paragraph used to warn about as a someday, and exactly what this check caught the day it happened:
# `HeapCeilingTests` existed in `syslNative` and nowhere the grouper looked, until `SUITES` grew a
# second root to match.
#
# It costs one sbt start; the test classes it needs compiled are ones the first group compiles anyway.
print "reconciling the suite list against sbt" | tee -a "$SUMMARY"

sbt -batch --error "print syslNative/Test/definedTestNames" > "$LOGS/defined.log" 2>&1

# The dot is in the class because a package deeper than `sh.sysl` is a matter of time: every test
# file is `package sh.sysl` today, and a suite under `sh.sysl.foo` would otherwise be captured as
# `sh.sysl.foo` and reconcile as a mismatch. That fails loudly rather than silently, which is the
# right way round — and costs nothing to not do at all.
grep -oE 'sh\.sysl\.[A-Za-z0-9_.]+' "$LOGS/defined.log" | sort -u > "$LOGS/defined.txt"

python3 -c "
import json, sys
chunks = json.load(open('$LOGS/chunks.json'))
heavy  = json.load(open('$LOGS/heavy.json'))
found  = sorted({s for g in chunks for s in g} | set(heavy))
print('\n'.join(found))
" | sort -u > "$LOGS/grouped.txt"

MISSING=$(comm -23 "$LOGS/defined.txt" "$LOGS/grouped.txt")
EXTRA=$(comm -13 "$LOGS/defined.txt" "$LOGS/grouped.txt")

if [[ -n "$MISSING" || -n "$EXTRA" ]]; then
  {
    print "GATE: RED -- the suite list does not reconcile with sbt"
    print "  sbt has $(wc -l < "$LOGS/defined.txt" | tr -d ' '), the grouper found $(wc -l < "$LOGS/grouped.txt" | tr -d ' ')"
    [[ -n "$MISSING" ]] && print "  never run:" && print "$MISSING" | sed 's/^/    /'
    [[ -n "$EXTRA" ]] && print "  grouped but unknown to sbt:" && print "$EXTRA" | sed 's/^/    /'
    print "  fix gate-groups.py rather than this list -- see its self-test"
  } | tee -a "$SUMMARY"

  # **`exit 1`, unlike every other way this script ends, and the distinction is not a slip.** A RED
  # *verdict* exits 0 on purpose — the summary is the verdict and a caller reads it, which the header
  # says at length. This is not a verdict: it is the same class of failure as the grouper dying at
  # line 60, which exits 1 for the same reason. The suite list being wrong means no verdict was
  # produced at all, and a check that fires exactly when the gate is not covering what it claims is
  # the last place to hand back a success status — `./run-gate.sh && <merge>` would sail through it.
  exit 1
fi

print "  $(wc -l < "$LOGS/defined.txt" | tr -d ' ') suites, and sbt agrees" | tee -a "$SUMMARY"

# **THE DOC SUITES ARE THE SECOND SOURCE ROOT, AND UNTIL 2026-08-27 THE GATE COULD NOT SEE THEM.**
# `doc/shared/src/test/` holds the sixteen `DocCliTests` that pin the `sysl doc` command's exit codes
# -- where its only real bug so far lived -- and `SlugConformanceTests`, which is the only thing that
# renders through juicer's jar and so the only thing that would notice a per-page `slugStyle`
# regression. A release ran both by hand; a gate that says GREEN without them was saying less than
# the reader assumed. Card `0273`.
#
# They are DISCOVERED rather than listed: `gate-groups.py` reads the compiler's source root and knows
# nothing about this one, and a hand-written list here would be the same staleness one file over.
# There is therefore nothing to reconcile the list against -- so what is checked is that it is not
# EMPTY, which is the failure that would otherwise read as a clean run of nothing.
print "listing the doc suites" | tee -a "$SUMMARY"

sbt -batch --error "print syslDocNative/Test/definedTestNames" > "$LOGS/defined-doc.log" 2>&1

grep -oE 'sh\.sysl\.doc\.[A-Za-z0-9_.]+' "$LOGS/defined-doc.log" | sort -u > "$LOGS/defined-doc.txt"

DOC_SUITES=$(tr '\n' ' ' < "$LOGS/defined-doc.txt")

if [[ -z "${DOC_SUITES// }" ]]; then
  {
    print "GATE: RED -- sbt named no doc suites, so the doc group would run nothing"
    print "  see $LOGS/defined-doc.log"
  } | tee -a "$SUMMARY"
  exit 1
fi

print "  $(wc -l < "$LOGS/defined-doc.txt" | tr -d ' ') doc suites" | tee -a "$SUMMARY"

# One attempt at one group, into `$LOGS/$log`. Two watchdogs, and the inner one is why a kill no
# longer costs the whole limit.
#
# **THE OUTER LIMIT IS FOR SILENCE; THE INNER ONE IS FOR AN OOM THAT ANNOUNCED ITSELF.** At the
# ceiling Immix does not abort, it spirals -- so the 15-minute limit exists for a group that says
# nothing at all, and it must stay. But an agent that reaches the cap usually *prints* first, and
# waiting out the limit after that is spending fourteen minutes on an outcome already decided:
# cutting 0.0.79, chunk-30 announced `Out of heap space grow heap` **1:46 in** and was killed at
# 15:00, and 26 of the 30 minutes that release's two kills cost were spent after the fact.
#
# The grace period is because an OOM in one agent does not always end the group -- sbt can lose that
# agent and still report on the others -- so a minute is given for a verdict to appear before the
# group is cut. A minute against fourteen is worth paying for the case where the run recovers.
attempt_group () {
  local log=$1 heap=$2 agents=$3 project=$4 suites=$5

  GC_MAXIMUM_HEAP_SIZE=$heap SYSL_RELEASE=1 sbt -J-XX:-DoEscapeAnalysis -batch \
    "set Global/concurrentRestrictions += Tags.limit(Tags.Test, $agents)" \
    "$project/testOnly $suites" > "$LOGS/$log" 2>&1 &
  local sbt_pid=$!

  ( sleep $LIMIT; kill -9 $sbt_pid 2>/dev/null ) &
  local outer=$!

  ( while kill -0 $sbt_pid 2>/dev/null; do
      if grep -q "Heap_exitWithOutOfMemory" "$LOGS/$log" 2>/dev/null; then
        sleep $OOM_GRACE
        kill -9 $sbt_pid 2>/dev/null
        break
      fi
      sleep 2
    done ) &
  local inner=$!

  wait $sbt_pid
  kill $outer 2>/dev/null
  kill $inner 2>/dev/null
}

# Reads one attempt's log. The log is the verdict. Never the exit status: a backgrounded pipeline
# reports the wrong one, and a killed run reports a plausible one.
read_result () { grep -E "^\[(info|error)\] Tests: succeeded" "$LOGS/$1" | tail -1 }
read_last ()   { grep -E "^\[info\] [A-Z][A-Za-z]*Tests:" "$LOGS/$1" | tail -1 }

run_group () {
  local label=$1 heap=$2 agents=$3 project=$4 suites=$5

  print "=== $label start $(date +%H:%M:%S)  heap=$heap agents=$agents project=$project" >> "$SUMMARY"

  attempt_group "$label.log" $heap $agents "$project" "$suites"

  local result=$(read_result "$label.log")
  local failed=$(grep -c "\*\*\* FAILED \*\*\*" "$LOGS/$label.log")
  local oom=$(grep -c "Heap_exitWithOutOfMemory" "$LOGS/$label.log")

  # **A GROUP WITH NO RESULT LINE PRODUCED NO VERDICT, AND NO VERDICT IS A REASON TO RUN IT AGAIN.**
  # That is the shared rule about a killed run, applied by the script instead of by whoever reads the
  # summary an hour later. Retried alone at the heavy settings, which is exactly the re-run that was
  # done by hand after 0.0.79's two kills -- both passed, 229 tests in 2:43 and 372 in 4:02.
  #
  # It covers both causes without having to tell them apart: a chunk that piled up more than three
  # agents could hold, and a cold standard-module cache pushing a per-target artifact build into an
  # ordinary chunk. One agent with the heavy heap answers each.
  #
  # **ONLY WHERE THERE IS NO VERDICT -- a group that RAN and reported failures is never retried.**
  # Retrying a real failure is how a flake-tolerant gate stops being a gate, and the whole value of
  # this script is that a red means something. The distinction is the empty `result`, not the count.
  if [[ -z $result ]]; then
    print "$label  no verdict $(date +%H:%M:%S)  oom=$oom  last suite: $(read_last "$label.log")" >> "$SUMMARY"
    print "$label  RETRY alone $(date +%H:%M:%S)  heap=$HEAVY_HEAP agents=$HEAVY_AGENTS" >> "$SUMMARY"

    attempt_group "$label-retry.log" $HEAVY_HEAP $HEAVY_AGENTS "$project" "$suites"

    result=$(read_result "$label-retry.log")
    failed=$(grep -c "\*\*\* FAILED \*\*\*" "$LOGS/$label-retry.log")
    oom=$(grep -c "Heap_exitWithOutOfMemory" "$LOGS/$label-retry.log")

    if [[ -z $result ]]; then
      print "$label  TIMEOUT/KILLED $(date +%H:%M:%S)  oom=$oom  last suite: $(read_last "$label-retry.log")" >> "$SUMMARY"
    else
      # **A group that only passes alone is a HEAVY candidate, and this is the measurement the
      # docstring says cannot be computed.** `gate-groups.py` is honest that heaviness cannot be
      # derived from source text; a group needing the retry across successive runs is the evidence,
      # and the suite to promote is named in the `no verdict` line above.
      print "$label  RETRIED-OK $(date +%H:%M:%S)  failed=$failed oom=$oom  $result" >> "$SUMMARY"
    fi
  else
    print "$label  done $(date +%H:%M:%S)  failed=$failed oom=$oom  $result" >> "$SUMMARY"
  fi

  # An agent can outlive a -9'd sbt, and a stranded one is charged to the next group's budget.
  # Reported rather than killed: `pkill` would take another session's build with it.
  local stranded=$(pgrep -x sysl-test | wc -l | tr -d ' ')
  [[ $stranded != 0 ]] && print "  STRANDED AGENTS: $stranded -- kill by PID before continuing" >> "$SUMMARY"
}

# Runs here rather than at the top because it drives the real `attempt_group`, which is defined
# above. Captured rather than piped: a function on the left of a `| tee` runs in a subshell, so its
# failure would be swallowed and the pipeline would report `tee`'s success -- which is the exact
# shape of the exit-status trap this script's header warns about twice.
WATCHDOG_OUT=$(watchdog_self_test)
WATCHDOG_RC=$?
print "$WATCHDOG_OUT" | tee -a "$SUMMARY"
(( WATCHDOG_RC == 0 )) || exit 1

# **ONE GROUP PER HEAVY SUITE, WHICH IS WHAT `gate-groups.py` HAS ALWAYS SAID AND NOT WHAT THIS DID.**
# Its docstring answers "which suites need a group to **themselves**"; this ran all of them in one
# `sbt`, which is a group *between* them. That was invisible while the four totalled about thirty
# seconds, and it made `LIMIT` a budget for the whole set rather than for one suite -- so promoting a
# suite into `HEAVY` could push the group past a limit meant to catch a wedge.
#
# It fired the first time a fifth was added. `ConditionalTests` was put in `HEAVY` on 2026-08-28
# (card `0324`) and the combined group ran **19 minutes with `oom=0`** before the watchdog cut it,
# then went to a retry that could only do the same -- a `TIMEOUT/KILLED` reporting nothing wrong with
# the tree. A heavy suite is heavy because it builds for every target; five of them serially at one
# agent is five times that, and there is no number for `LIMIT` that is right for both one and five.
#
# **It does belong in `HEAVY`, and this is the change that makes that true** -- see the measurements
# beside that list. On its own it is 11:58, which fits the limit; sharing one with four suites that
# now cost 27 minutes between them, it could not. The four already there were most of the way to the
# limit before a fifth was ever proposed.
#
# Per suite, the limit means what it says again, and a sixth costs its own budget rather than
# everyone else's.
for suite in $(python3 -c "import json;print(' '.join(json.load(open('$LOGS/heavy.json'))))"); do
  run_group "heavy-${suite##*.}" $HEAVY_HEAP $HEAVY_AGENTS syslNative "$suite"
done

run_group doc $LIGHT_HEAP $LIGHT_AGENTS syslDocNative "$DOC_SUITES"

NCHUNKS=$(python3 -c "import json;print(len(json.load(open('$LOGS/chunks.json'))))")

for i in $(seq 0 $((NCHUNKS - 1))); do
  run_group "chunk-$i" $LIGHT_HEAP $LIGHT_AGENTS syslNative \
    "$(python3 -c "import json;print(' '.join(json.load(open('$LOGS/chunks.json'))[$i]))")"
done

print "=== all groups done $(date +%H:%M:%S)" >> "$SUMMARY"

python3 - "$SUMMARY" >> "$SUMMARY" <<'PY'
import re, sys

total = failed = 0
stalled = []
retried = []

for line in open(sys.argv[1]):
    if m := re.search(r'Tests: succeeded (\d+), failed (\d+)', line):
        total += int(m.group(1))
        failed += int(m.group(2))
    if 'TIMEOUT' in line:
        stalled.append(line.split()[0])
    if 'RETRIED-OK' in line:
        retried.append(line.split()[0])

print(f'TOTAL: {total} succeeded, {failed} failed')
print('TIMED OUT: ' + (', '.join(stalled) if stalled else 'none'))

# **A retry is reported and is NOT a red.** The group produced no verdict, was run again alone, and
# answered -- which is the mechanism working. It is printed rather than swallowed because a group
# that needs it repeatedly is the evidence for promoting one of its suites into HEAVY, and that
# evidence exists nowhere else.
print('RETRIED (passed alone): ' + (', '.join(retried) if retried else 'none'))
# **THE VERDICT NAMES ITS OWN SCOPE, AND THAT IS THE WHOLE OF WHAT THIS LINE IS FOR.** Three words
# on their own invite the reader to infer the tree: two sessions independently read `GATE: GREEN` as
# covering everything the repository builds. It covers the two Native projects and nothing else --
# `syslJS` has never been reached, because `syslJS/Test/fastLinkJS` exhausts the heap at 8g and again
# at 16g and the bundle has never linked at all (card `0272`). Naming the gap is what makes leaving
# it out honest rather than silent; a green verdict that states its scope cannot be over-read.
print('GATE: ' + ('GREEN' if failed == 0 and not stalled else 'RED')
      + '  -- syslNative and syslDocNative; NOT syslJS, which has never linked (card 0272)')
PY

tail -3 "$SUMMARY"
print "\nfull summary: $SUMMARY"
