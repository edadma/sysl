package sh.sysl

import io.github.edadma.cross_platform.*

/** `sysl test` — builds the `@test` functions of a source tree and runs them (`getting-started/cli.md § test`).
 *
 * **One build, one process per test.** The whole tree is compiled once, into a binary whose entry
 * point takes a test's name and runs that test alone (`Codegen.genTestMain`); the runner then starts
 * it once per test. A test that fails does so by trapping, and a trap takes its process with it — so
 * a run that shared one process would report the first failure and nothing after it. Paying for a
 * process per test buys every test a verdict, and it is the cheap half: the compile is the slow one
 * and there is only ever one of those.
 *
 * **The verdict is the exit status, and nothing else.** A test passes by returning. That is the whole
 * protocol, and it is what lets a test assert with the language it is testing rather than with a
 * framework: a broken `require`, a bounds violation, an `unwrap` of `None` — each ends the process,
 * and none of them needed to know it was running under a test. `@test(should_trap)` inverts the
 * reading for a test whose subject *is* the check.
 *
 * **A module's hooks are read against that protocol rather than beside it**
 * (`reference/attributes.md § The hooks a module may write`). `@setup` and `@teardown` run in the
 * test's own process, which is what lets a setup leave something for the test to find — and is why
 * a teardown after a test that trapped runs in a process of its own instead, there being nothing
 * left of the first one. `@setup_all` and `@teardown_all` are their own invocations, so a module
 * reaches its tests from one of them only through what outlives a process. Which part of a run
 * ended it cannot be read from an exit status, so the dispatcher marks each boundary it crosses
 * (`Tests.setupMark`).
 */
object TestRunner {

  /** What was asked of a run, beyond which tree to run.
   *
   * `filter` selects by substring against both the reported name and the key, so a module's tests can
   * be named by their module and one test by its own name. `failFast` stops at the first failure,
   * which is what someone iterating on one wants and the opposite of what someone reading a report
   * does — hence a flag rather than a default either way.
   */
  case class Options(filter: Option[String] = None, failFast: Boolean = false)

  /** One test, run. `detail` is absent exactly when the test passed; `output` is everything the run
   * printed, on either stream, which a failure shows and a pass keeps to itself.
   *
   * **A row for a `@setup_all` or `@teardown_all` that did not come back is one of these too**, with
   * the hook standing in for the test: a hook that runs alone has no test to hang its failure on,
   * and what a row needs is a name, a file and a line, which a hook has. So the report grows a row
   * rather than a second kind of row, and the header counts the tests instead of counting the rows.
   */
  case class Outcome(test: TTest, detail: Option[String], output: String, millis: Long) {
    def passed: Boolean = detail.isEmpty
  }

  /** `sysl test <path>`: compile the tree as a test build, run what it holds, report.
   *
   * A test build is **for this machine**, for the same reason `run` is — the binary is executed and
   * only this machine can execute it. A cross target is refused here rather than built and then found
   * to be unrunnable.
   *
   * `objects` is the C the walked trees carried, already compiled (`NativeSources`). A test build
   * gets it on the same footing as a `build` does, and has to: a package's tests are exactly what
   * exercises the shims it ships, so a test run that dropped them would fail at the link on the one
   * tree most likely to hold C.
   *
   * **`--std` is what lets the standard library's own tests run**, and it is the same word
   * `build-lib` uses for the same reason: the tree in front of the compiler *is* the library rather
   * than a program compiled against one. Testing sysl's own library is what a `@test` written in
   * sysl is for, and without this the tree collides with the copy the compiler supplies — every
   * declaration already declared, because it is the same declaration twice.
   *
   * `librarySources` is a library handed over as source, kept apart from `sources` rather than
   * concatenated into it because which modules are this tree's own is what decides whether a
   * dependency's export, handler, placed definition or destructor is a root
   * (`Reachability.contributing`). A test build links a `main` of its own, so it is one of the builds
   * that collision costs.
   */
  def run(cfg: Config, sources: List[Source], libraries: List[Program], target: Target,
          precompiled: Set[String], std: Stdlib, archives: List[String],
          objects: List[String] = Nil, paths: SearchPaths = SearchPaths.none,
          allocator: Allocator = Allocator.c, librarySources: List[Source] = Nil,
          cacheKey: Option[String] = None, devModules: Set[String] = Set.empty): Int = {
    if !Target.host.contains(target) then
      return fail(s"'test' runs what it builds, and '${target.name}' is not this machine")

    val building = if cfg.std then LibraryArtifact.std else Set.empty[String]

    // `paths` reaches the compilation as well as the link below. A `c const` is evaluated by the C
    // compiler while the tree is analyzed, so a test build of a tree that has one needs the include
    // directories exactly as an ordinary build does — and not passing them here is what once made
    // `test` the one subcommand a package built on `c const` could not run.
    val (built, tests) =
      Compiler.compileTests(sources, libraries, target, precompiled, Some(std), building, paths,
                            allocator, librarySources, devModules) match
      case Left(err)     => return report(err)
      case Right(result) => result

    val opts     = Options(cfg.filter, cfg.failFast)
    val selected = tests.filter(matches(_, opts.filter))

    // Nothing to run is not nothing to say. A tree with no tests at all and a filter that matched
    // none of the tests there are lead to the same empty report and are different mistakes, so each
    // is named. Neither is a failure: a program is allowed to have no tests.
    if tests.isEmpty then
      Console.err.println(s"no '@test' functions in ${cfg.file}")
      return 0

    if selected.isEmpty then
      Console.err.println(s"no test matches '${opts.filter.getOrElse("")}' — ${tests.length} to choose from")
      return 0

    // Linked straight into the cache slot where there is one, for `run`'s reason: a copy would have
    // to reproduce the executable bit, and the linker already knows how.
    val keeping = cacheKey.flatMap(RunCache.reserve)
    val exe     = keeping.getOrElse(createTempFile("sysl-test-", ""))

    Toolchain.build(built.ir, exe, target, archives, cfg.optimize, built.links, objects, paths) match
      case Left(err) => Project.discard(exe); fail(err)
      case Right(_) =>
        // **The sidecar is written after the binary exists**, so a hit that finds both finds a pair
        // that was made together. A failure to write it costs a rebuild next time and nothing else.
        for key <- cacheKey if keeping.isDefined; path <- RunCache.tests(key) do
          try writeFile(path, RunCache.encode(tests))
          catch case _: Exception => ()

        val outcomes = execute(exe, selected, opts, tests.length - selected.length, emitLine)

        if keeping.isEmpty then Project.discard(exe)
        if outcomes.forall(_.passed) then 0 else 1
  }

  /** A cached test build, run without compiling anything (`RunCache`).
   *
   * It is the tail of `run` above with the compile taken out: the same filtering, the same
   * reporting, the same two ways of saying that nothing matched. Written here rather than in the
   * driver so that the two paths cannot drift — what a reader sees for a cached suite has to be what
   * they see for a fresh one, and the report is most of what a test run *is*.
   */
  def rerun(cfg: Config, exe: String, tests: List[TTest]): Int = {
    val opts     = Options(cfg.filter, cfg.failFast)
    val selected = tests.filter(matches(_, opts.filter))

    if tests.isEmpty then
      Console.err.println(s"no '@test' functions in ${cfg.file}")
      return 0

    if selected.isEmpty then
      Console.err.println(s"no test matches '${opts.filter.getOrElse("")}' — ${tests.length} to choose from")
      return 0

    val outcomes = execute(exe, selected, opts, tests.length - selected.length, emitLine)

    if outcomes.forall(_.passed) then 0 else 1
  }

  /** Every selected test, run in the order it was written — which is the order it is reported in, so
   * that a reader following a failure down a file finds the tests where the file put them.
   *
   * `failFast` stops the loop rather than the report: what has run is still reported, and the tests
   * that never ran are simply absent. Reporting them as skipped would be a third verdict for
   * something that is not a verdict at all.
   *
   * **`emit` is called as the run goes, not once at the end** — the header first, then a file's
   * heading before its first row, then each row the moment its test or hook finishes, then the
   * summary once every test has. A suite that takes half an hour is watched from a log file while
   * it runs rather than staying silent until it exits, and a run that is killed partway still has
   * a report of everything up to the kill. The default does nothing, for the callers that only want
   * the finished list — mostly other tests, which build the report from what is returned instead.
   */
  def execute(exe: String, tests: List[TTest], opts: Options, filtered: Int = 0,
              emit: String => Unit = _ => ()): List[Outcome] = {
    val done  = List.newBuilder[Outcome]
    var stop  = false
    var shown = Option.empty[String]

    // Fixed before the first test runs, from every name a row could carry — a test's own, or the
    // bare name of a hook that might stand in for one — because the alternative is not printing a
    // row until the last test is known to be the widest, which is the whole run.
    val width = widthOf(tests)

    def streamOne(o: Outcome): Unit = {
      if !shown.contains(o.test.file) then
        emit(fileHeader(o.test.file))
        shown = Some(o.test.file)
      emit(row(o, width))
    }

    emit(header(tests.length, filtered))

    for (_, group) <- byModule(tests) if !stop do
      val hooks = group.head.hooks

      // `@setup_all` is a run of its own, before anything else in the module. Where it does not come
      // back it gets a row of its own and the module's tests do not run: there is no test to hang
      // the failure on, and reporting tests that never started would be a third verdict for
      // something that is not one — the same reading `failFast` above is given.
      val booted = hooks.setupAll.map(h => h -> runHook(exe, h))

      booted match
        case Some((h, r)) if r.failed =>
          val outcome = hookOutcome(h, r)
          done += outcome
          streamOne(outcome)
          if opts.failFast then stop = true
        case _ =>
          for t <- group if !stop do
            val outcome = one(exe, t)

            done += outcome
            streamOne(outcome)
            if opts.failFast && !outcome.passed then stop = true

      // `@teardown_all` runs whatever became of the tests, and whatever became of `@setup_all`:
      // what it is for is releasing what the module took, and a module that failed halfway has
      // taken some of it.
      for h <- hooks.teardownAll do
        val r = runHook(exe, h)

        if r.failed then
          val outcome = hookOutcome(h, r)
          done += outcome
          streamOne(outcome)
          if opts.failFast then stop = true

    val outcomes = done.result()
    emit(summary(outcomes))
    outcomes
  }

  /** The widest name a row in this run could carry, fixed before anything is run so the first row
   * can be streamed immediately. Covers both what `tests` will show and the bare name of every hook
   * their modules declare, since a failing `@setup_all` or `@teardown_all` reports under that name
   * instead (`hookOutcome`) — so the padding a live row gets does not shift once a hook fails.
   */
  private def widthOf(tests: List[TTest]): Int = {
    val names = tests.map(_.display) ++ tests.flatMap(_.hooks.all).map(h => Modules.bare(h.func))
    if names.isEmpty then 0 else names.map(_.length).max
  }

  /** Prints one piece of a report immediately and flushes, so a run followed through a log file
   * shows each verdict as it lands rather than only once the process exits.
   */
  private def emitLine(s: String): Unit = {
    stdout(s)
    Console.flush()
  }

  /** The selected tests grouped by the module that declared them, in the order the modules were
   * first met.
   *
   * The grouping is what makes an `_all` hook run once, and the order is the tests' own so that a
   * report still follows the source. A module with no hooks pays nothing for this: the group runs
   * exactly the processes it would have run without one.
   */
  private def byModule(tests: List[TTest]): List[(String, List[TTest])] = {
    val groups = tests.groupBy(t => Modules.moduleOf(t.func))

    tests.map(t => Modules.moduleOf(t.func)).distinct.map(m => m -> groups(m))
  }

  /** One test, run — and its module's per-test hooks around it.
   *
   * **`@setup` runs inside the test's process** (`Codegen.genTestMain`), so what it leaves in module
   * storage is what the test finds. That is also why a fault in it cannot be told from a fault in
   * the test by the exit status alone, and why the dispatcher leaves a mark on its way past
   * (`Tests.setupMark`): no mark means the run never got out of setup.
   *
   * **`@teardown` runs in that process where the test returned, and in one of its own where it did
   * not.** A trap takes the process with it, so there is nothing left there to run a teardown in —
   * and honouring "teardown runs even when the test failed" any other way would mean not running it
   * at all. What a fresh process can release is what outlives a process, which is what the hook is
   * documented to be for.
   */
  private def one(exe: String, t: TTest): Outcome = {
    val started = System.currentTimeMillis()
    val result  = exec(List(exe, t.func))
    val raw     = result.stdout + result.stderr
    val output  = Tests.unmarked(raw)

    val setupRan    = t.hooks.setup.isEmpty || raw.exists(_ == Tests.setupMark)
    val enteredDown = t.hooks.teardown.isDefined && raw.exists(_ == Tests.testMark)

    val inProcess =
      if !setupRan then t.hooks.setup.map(h => guarded(h, result.exitCode))
      else if enteredDown && result.exitCode != 0 then
        t.hooks.teardown.map(h => hookFailed(h, result.exitCode) + ", after the test returned")
      else verdict(t, result.exitCode, output)

    // The teardown the process could not reach. Its own output joins the test's, since a reader
    // following one failure should not have to run the suite again to see the other.
    val after = for h <- t.hooks.teardown if !enteredDown yield h -> runHook(exe, h)

    Outcome(
      t,
      inProcess.orElse(after.collect { case (h, r) if r.failed => hookFailed(h, r.status) }),
      output + after.map(_._2.output).getOrElse(""),
      System.currentTimeMillis() - started,
    )
  }

  /** What one hook's own process did. */
  private case class HookRun(status: Int, output: String, millis: Long) {
    def failed: Boolean = status != 0
  }

  private def runHook(exe: String, h: THook): HookRun = {
    val started = System.currentTimeMillis()
    val result  = exec(List(exe, h.func))

    HookRun(result.exitCode, Tests.unmarked(result.stdout + result.stderr),
            System.currentTimeMillis() - started)
  }

  /** A hook's failure as a report row, since a hook that runs alone has no test to hang one on.
   *
   * It borrows `TTest` rather than growing the report a second kind of row: what a row needs is a
   * name, a file and a line, and a hook has all three. It is shown under the name it was declared
   * with, which is what a reader greps for.
   */
  private def hookOutcome(h: THook, r: HookRun): Outcome =
    Outcome(TTest(h.func, Modules.bare(h.func), false, None, h.file, h.line), Some(hookFailed(h, r.status)),
            r.output, r.millis)

  private def hookFailed(h: THook, status: Int): String =
    s"the module's '@${h.kind.word}', '${Modules.show(h.func)}', did not return — exit status $status"

  private def guarded(h: THook, status: Int): String =
    s"${hookFailed(h, status)}, so this test did not run"

  /** Whether a test's run was what the test said it would be, and if not, what it was instead.
   *
   * The status is read as the platform leaves it: zero for a function that returned, and anything
   * else for a process that did not get there. **A trap is not told from an `exit`**, deliberately —
   * both are a test that did not come back, the language offers no way to distinguish them from
   * inside, and a rule that read one as a pass and the other as a failure would rest on which signal
   * a given machine's `llvm.trap` happens to raise.
   */
  def verdict(t: TTest, status: Int, output: String): Option[String] =
    if !t.shouldTrap then
      Option.when(status != 0)(s"did not return — exit status $status")
    else if status == 0 then
      Some("returned, and was expected to trap")
    else
      t.expected.flatMap(want =>
        Option.when(!output.contains(want))(
          s"trapped, but printed nothing holding \"$want\""))

  /** A test is selected when the pattern is a substring of either name it has: the one a report shows
   * and the key its module gives it. Two, because the two are what a reader has to hand — a name read
   * off a failing report, and a module named to run everything under it.
   */
  def matches(t: TTest, filter: Option[String]): Boolean =
    filter.forall(f => t.display.contains(f) || Modules.show(t.func).contains(f))

  /** The report, grouped by the file each test was written in.
   *
   * The file is the grouping because that is where a reader goes next, and the tests under it keep
   * their source order. A failure's own line says what happened; anything the run printed follows it,
   * indented, and is shown **only** for a failure — output from a test that passed is what the test
   * was doing, not something anyone asked to read.
   */
  def rendered(outcomes: List[Outcome], filtered: Int, selected: Int): String = {
    val out = new StringBuilder
    stream(outcomes, filtered, selected, out ++= _)
    out.toString
  }

  /** `rendered`, taken apart into the pieces a live run emits one at a time — the header, then
   * each file's heading before its first row, then each row, then the summary. Given the same
   * outcomes in the same order, the concatenation of every `emit` call is exactly what `rendered`
   * returns as one string, because both are built from these same four pieces.
   */
  def stream(outcomes: List[Outcome], filtered: Int, selected: Int, emit: String => Unit): Unit = {
    val width = if outcomes.isEmpty then 0 else outcomes.map(_.test.display.length).max

    emit(header(selected, filtered))

    for (file, group) <- outcomes.groupBy(_.test.file).toList.sortBy(_._1) do
      emit(fileHeader(file))
      for o <- group.sortBy(_.test.line) do emit(row(o, width))

    emit(summary(outcomes))
  }

  /** The header counts the **tests** that were selected, which is not the number of rows below it:
   * a hook that did not come back gets a row of its own, and a module whose `@setup_all` failed
   * has tests that were selected and never ran. Reading the count off the rows would say "running
   * 4 tests" over three tests and a hook.
   */
  private def header(selected: Int, filtered: Int): String = {
    val out = new StringBuilder

    out ++= s"running $selected ${if selected == 1 then "test" else "tests"}"
    if filtered > 0 then out ++= s" of ${selected + filtered}"
    out ++= "\n"
    out.toString
  }

  /** The heading a file's rows are shown under — once, before the first of them. */
  private def fileHeader(file: String): String = s"\n$file\n"

  /** One outcome's row, padded to `width` so every status column lines up regardless of which
   * row's name happens to be the widest. A failure's own line says what happened; anything the run
   * printed follows it, indented, and is shown **only** for a failure — output from a test that
   * passed is what the test was doing, not something anyone asked to read.
   */
  private def row(o: Outcome, width: Int): String = {
    val out = new StringBuilder

    out ++= s"  ${if o.passed then "ok  " else "FAIL"}  ${o.test.display.padTo(width, ' ')}  ${o.millis}ms\n"

    for detail <- o.detail do
      out ++= s"        $detail\n"
      out ++= s"        at ${o.test.file}:${o.test.line}\n"
      for line <- o.output.linesIterator do out ++= s"        > $line\n"

    out.toString
  }

  /** The closing line: how many of the rows passed, against how long the whole run took. */
  private def summary(outcomes: List[Outcome]): String = {
    val failed = outcomes.count(!_.passed)
    val total  = outcomes.map(_.millis).sum

    s"\n${outcomes.length - failed} passed, $failed failed — ${total}ms\n"
  }

  private def fail(msg: String): Int = {
    Console.err.println(s"sysl: error: $msg")
    1
  }

  private def report(diagnostic: String): Int = {
    Console.err.println(diagnostic)
    1
  }
}
