package sh.sysl

/** The `@test` functions a program declares, and what separates a test build from every other one
 * (`reference/attributes.md § What is dropped, and when`).
 *
 * A test is an ordinary function with an attribute on it. That is the whole of the language part:
 * nothing about the body is special, it may call anything the module can reach, and it is analyzed
 * and type-checked exactly as it would be without the line above it. What the attribute buys is a
 * *caller* — `sysl test` builds an entry point that calls one of them by name, and the program's own
 * entry point is not built at all.
 *
 * **A test is not part of the program it is written in.** `sysl run`, `sysl build` and
 * `sysl emit-llvm` all drop them, and drop them *after* analysis, so a test that does not compile is
 * still a compilation error in a build that would never have run it. That is what lets a test sit
 * beside what it tests: a library's tests do not travel in the library, and a program's do not run
 * when it runs.
 *
 * **`sysl build-lib` is the exception, and drops them before analysis instead** — `stripSource`
 * rather than `strip`. An artifact is the one output that outlives the compilation that made it, and
 * analyzing a test body is enough to change what it holds: a test over a `Buf[int]` monomorphizes
 * the whole of `Buf` at `int`, and those instantiations are ordinary library functions by the time
 * `strip` runs, so nothing downstream can tell them from ones the library asked for. Stripping the
 * declarations first is what keeps an artifact's contents a fact about the library rather than about
 * its tests.
 *
 * **The line this draws is between PARSING and ANALYSIS, and it is worth stating exactly**, because
 * "`build-lib` no longer checks a library's tests" is wrong in both directions. `LibraryArtifact.build`
 * parses every source and returns on the first `Left` before `compileLibrary` is reached, so a
 * **syntax** error in a `@tests` file still stops the build. What such a file no longer gets is
 * everything *after* the parse — name resolution, types, visibility, capabilities, the `@test`
 * well-formedness checks `problem` and `resultProblem` make from `Hoisting`, generic instantiation,
 * escape analysis, the tail-call check. Not merely type-checking.
 *
 * So what is given up is narrower than it sounds and sharper: a library test that is *well-formed
 * text* and wrong in every other way builds clean. `sysl test --std` compiles the library's tests
 * properly and runs them, and the suite runs that — a better place for the check than a command
 * whose subject is the artifact.
 */
object Tests {

  /** The byte the test dispatcher writes to standard error once `@setup` has returned
   * (`Codegen.genTestMain`).
   *
   * **A dead process cannot report which part of it died**, and that is the whole of why a byte is
   * written at all: `setup(); test(); teardown()` runs in one process, a fault anywhere in it ends
   * that process, and the exit status is the same however far it got. So the dispatcher leaves a
   * mark as it crosses each boundary, and the runner reads how far the run reached off what arrived.
   *
   * The two marks are control characters no source text can hold — neither survives the lexer, so
   * a test's own output cannot forge one. Both are stripped from what a failure shows: they are the
   * runner's channel and not the test's.
   */
  val setupMark: Char = 1

  /** The byte the dispatcher writes once the test itself has returned, before `@teardown` is called.
   * Present means teardown was entered — so a process that then died died in teardown, and one
   * that never wrote it left teardown to a process of its own (`TestRunner.execute`).
   */
  val testMark: Char = 2

  /** A run's output as a reader should see it: the marks above taken out. */
  def unmarked(output: String): String =
    if output.exists(marked) then output.filterNot(marked) else output

  private def marked(c: Char): Boolean = c == setupMark || c == testMark

  /** The requirements a `@test` function meets, checked at the declaration.
   *
   * They say the same thing from different sides: **the runner must be able to call it with nothing
   * and learn the answer from whether it returned.** A parameter is something the runner has no value
   * for, and a type parameter leaves nothing to call at all, since a generic has no compiled form
   * until a caller fixes its arguments.
   *
   * A variadic tail needs no case of its own: `reference/ffi.md § Variadic functions` already
   * requires a named parameter in front of one, so a function that could take a tail has taken a
   * parameter and is refused by that.
   *
   * Each is reported where the attribute is rather than where the signature is, because the attribute
   * is the part that is wrong: the function is a perfectly good function, and it is `@test` that made
   * a promise about it that it cannot keep.
   *
   * `what` is the attribute the message names, since the four hooks are held to exactly these rules
   * and for exactly this reason: the runner calls a hook with nothing, in a process of its own or in
   * a test's, and reads the answer off whether it came back.
   */
  def problem(f: FuncDecl, what: String = "test"): Option[String] =
    if f.params.nonEmpty then
      Some(s"a '@$what' function takes no parameters, and '${Modules.bare(f.name)}' takes " +
        (if f.params.length == 1 then "one" else s"${f.params.length}") +
        " — 'sysl test' calls it with nothing, so there is nowhere for an argument to come from")
    else if f.tparams.nonEmpty then
      Some(s"a '@$what' function has no type parameters, and '${Modules.bare(f.name)}' declares " +
        s"'${f.tparams.mkString(", ")}' — a generic is compiled for the arguments a caller fixes, and " +
        "the runner supplies none")
    else None

  /** A test's result type, which must be `unit` — written as `-> unit` or, as almost every test will,
   * not written at all.
   *
   * Checked against the *resolved* type rather than the syntax so that a result reached through an
   * alias is refused with everything else, and so the message can name what the function actually
   * returns. A test's verdict is whether it came back, and a value returned beside that would be one
   * nothing is going to look at — which is a mistake about how the test reports, and the sort that
   * ends with someone believing an assertion ran.
   */
  def resultProblem(f: FuncDecl, retTy: Type, what: String = "test"): Option[String] =
    Option.when(!Type.noValue(retTy))(
      s"a '@$what' function returns nothing, and '${Modules.bare(f.name)}' returns " +
        s"'${Type.show(retTy)}' — " +
        (if what == "test" then "a test's result is whether it came back"
         else "a hook is called for what it does rather than for what it answers") +
        ", so there is nothing to read a value with")

  /** What the runner is told about one test: the key that calls it, the name that reports it, and
   * where the attribute was written.
   *
   * The reported name defaults to the function's own **bare** name — the module is already the file
   * the report groups under, so repeating it in every line would be noise. A `@test("…")` string
   * replaces it outright rather than decorating it, which is the point of writing one.
   */
  def describe(key: String, attr: TestAttr): TTest =
    TTest(
      key,
      attr.display.getOrElse(Modules.bare(key)),
      attr.shouldTrap,
      attr.expected,
      attr.pos.map(_.source.name).getOrElse("<unknown>"),
      attr.pos.map(_.line).getOrElse(0),
    )

  /** What the runner is told about one hook: which moment it is for, the key that calls it, and
   * where it was written.
   *
   * There is no reported name to choose, unlike a test's: a hook is shown by the name it was
   * declared under, because the only reason a report ever mentions one is that it did not come back,
   * and the reader's next move is to find that declaration.
   */
  def describeHook(key: String, attr: HookAttr): THook =
    THook(
      attr.kind,
      key,
      attr.pos.map(_.source.name).getOrElse("<unknown>"),
      attr.pos.map(_.line).getOrElse(0),
    )

  /** The refusal of a second hook of one kind in one module.
   *
   * **Two of them would both have to run, and nothing decides in which order** — which is not a
   * detail a program could work around, since the whole point of a setup is that what it leaves
   * behind is what the test finds. So the second is refused, and the message names the first: the
   * reader's question is "where is the other one", and a diagnostic that made them grep for it would
   * have been holding the answer.
   */
  def duplicateHook(kind: HookKind, module: String, first: THook, second: String): String =
    s"a module writes at most one '@${kind.word}', and " +
      (if module == Modules.root then "this one" else s"'${Modules.show(module)}'") +
      " already has one — " +
      s"'${Modules.bare(first.func)}' at ${first.file}:${first.line}, and now " +
      s"'${Modules.bare(second)}'. Both would run at the same moment, and nothing says in which " +
      "order; write one hook and have it call what it needs"

  /** The same program with every test and every test file dropped — the tree a build that is not
   * `sysl test` lowers.
   *
   * Dropping the functions is what keeps a test out of the output; dropping the list is what keeps
   * anything downstream from believing there are tests to dispatch to. Both, because either alone is
   * a tree that contradicts itself.
   *
   * **A `@tests` file goes with them, and this is the one place that can drop it.** A test's callees
   * needed no help while every build that could reach one was a program: a helper only a test calls
   * becomes unreachable the moment the test does, and `Reachability.prune` notices. A **library**
   * prunes nothing — it has no `main` to lower outwards from, so every public declaration is emitted
   * (`Compiler.compileLibrary`) — and a helper would ride into the artifact and be advertised out of
   * it. Naming the file is what answers that, since a file is what the author marked.
   *
   * Dropping it here is safe rather than lucky: `TestScope` has already held every reference into
   * such a file to coming from something dropped in the same builds, so what is left behind can
   * hold no reference to what went.
   *
   * The **types** it declared are left, exactly as `Reachability.prune` leaves them: a type is
   * emitted for its layout rather than for anything that runs, so an unused one costs a definition
   * nothing reads and no code at all.
   *
   * **A method table is not**, and it is the one thing here that has to go with a function rather
   * than outlive it. A closure lowered inside a test body is dropped with the test (`testOnlyDecls`
   * carries the name the compiler gave it), and the table registering it as an implementation of
   * `Fn` would otherwise be left pointing at a function no longer in the tree — which `prune` cannot
   * repair, since a table is one of its *roots*: it would follow the slot and keep the body, and the
   * body names the helpers that have just gone. Dropping the table is what makes the removal
   * complete, and it can catch nothing else: an `impl` may not sit in a `@tests` file, so the only
   * slot a dropped name can fill is a closure's own.
   */
  def strip(program: TProgram): TProgram = {
    val tests = program.tests.map(_.func).toSet
    // A hook goes with the tests it brackets, and is read from the program rather than from the
    // tests for the reason `TProgram.hooks` records: a module may declare one and no tests, and a
    // hook left behind is a function nothing calls in a build that runs nothing.
    val gone  = tests ++ program.hooks.map(_.func) ++ program.testOnly

    if gone.isEmpty then program
    else
      program.copy(
        vtables = program.vtables.filterNot(_.slots.exists(s => gone(s.target))),
        funcs = program.funcs.filterNot(f => gone(f.name)),
        vals = program.vals.filterNot(v => gone(v.symbol)),
        externs = program.externs.filterNot(e => gone(e.name)),
        tests = Nil,
        hooks = Nil,
        testOnly = Set.empty,
      )
  }

  /** The same removal made on the **untyped** tree, for the one build whose output outlives it.
   *
   * `strip` above cannot serve a library, and the reason is that analysis is not a passive reading:
   * a test body that names `Buf[int]` *creates* the whole of `Buf` at `int`, and a monomorphization
   * is an ordinary function by the time it reaches `strip` — nothing in it records which declaration
   * demanded it. Dropping the test after the fact therefore drops the test and keeps everything it
   * caused, and the artifact ships instantiations no caller of the library ever asked for.
   *
   * Two shapes to remove, because `@tests` and `@test` mark different things. A file with the header
   * is scaffolding **whole** — its ordinary helpers exist only for the tests below them, and it is
   * exactly what `Reachability.prune` could not answer for in a library, since a library prunes
   * nothing. A `@test` written in an ordinary file is one declaration, and the rest of that file is
   * the library.
   */
  def stripSource(units: List[Program]): List[Program] =
    units.filterNot(_.testOnly).map(u => u.copy(body = u.body.filter(kept)))

  /** Whether a top-level statement survives into a library. Only a `@test` function and the hooks
   * around one do not — an
   * `impl` may not sit in a `@tests` file at all (`reference/attributes.md § @tests — a file of scaffolding`), so nothing here has to reason about
   * a method table with a slot filled by something that is about to go.
   */
  private def kept(stmt: Stmt): Boolean = stmt match
    case f: FuncDecl => f.test.isEmpty && f.hook.isEmpty
    case _           => true

  /** The same program lowered **as** a test build: the tests kept, and the program's own entry point
   * put aside.
   *
   * A program's top-level statements and its `main` are what it does when it is run, and running it
   * is not what `sysl test` does — so they are dropped, and what remains of the entry point is the
   * dispatcher `Codegen` lays down instead. The module-level `val`s stay: they are storage the
   * program reads rather than work it performs, and a test that reads one would otherwise see it
   * empty.
   *
   * The tests are the **roots**, in place of the `main` that is no longer there, so that everything
   * one of them calls survives the pruning and nothing else does. A program whose tests reach half
   * of it compiles half of it, which is the same bargain every other build gets.
   *
   * **`Reachability.entryPoints` is a root here for the same reason it is one there**, and leaving it
   * out is a bug this build had: a handler, an export and a destructor are each reached from
   * somewhere this walk cannot see, so replacing the roots does not make them reachable from the
   * tests instead — it makes them reachable from nothing. A destructor pruned that way still has the
   * release hook calling it, so a package with one could not link its own suite; an export pruned
   * that way goes quietly, and the package's C is what discovers it.
   *
   * `own` carries through to the same place for the same reason: a test build links a `main` of its
   * own, so a dependency's unreached `@export("main")` would fight it exactly as it fights a
   * program's.
   */
  def only(program: TProgram, own: Option[Set[String]] = None): TProgram = {
    val kept    = program.copy(main = Nil, entry = None)
    val entries = Reachability.entryPoints(kept, own)
    // The hooks are roots beside the tests, and for the same reason: the dispatcher lays down an arm
    // that calls each by name, so a hook the walk could not reach from a test — which is every one
    // of them, since nothing calls a hook — would be pruned out from under its own arm.
    val roots   = List(kept.vals, kept.vtables,
                       (kept.tests.map(_.func) ::: kept.hooks.map(_.func)).map(TEntry(_, None)),
                       entries)
    val live    = Reachability.reachedFrom(roots, kept.funcs, kept.vtables).calls ++ entries.map(_.name)

    kept.copy(
      externs = kept.externs.filter(e => live(e.name)),
      funcs = kept.funcs.filter(f => live(f.name)),
    )
  }

  /** Refuses a `dev_dependencies` package imported from source a consumer would compile
   * (`reference/packages.md § Dependencies a test alone needs`).
   *
   * **The question is asked of the STRIPPED tree, which is the whole of the check.** A dev
   * dependency is resolved for `sysl test` and pruned from a consumer's graph, so what has to be
   * true is that nothing surviving into a library names one — and `stripSource` is already the
   * function that says what survives. An import inside a `@tests` file or a `@test` function is
   * gone by the time this looks, so neither needs a case of its own and neither can be got wrong
   * separately.
   *
   * Without it the mistake is a package that builds its own suite perfectly and cannot be used:
   * `sysl test` resolves the dev dependency and the ordinary module importing it compiles, while
   * every consumer is refused at a module that was never fetched for them. The author is the last
   * person to see it.
   *
   * `devModules` are the paths **as a file writes them** — `sh.sysl.quickjs`, not the coordinate,
   * which has a hyphen in it and is no module path at all (`PackageSources.devModules`).
   *
   * **It reads a file's own `import` statements and not an import written inside a block.** A block
   * import binds for the rest of that block, so one buried in an ordinary function's body would
   * reach a consumer unreported — and would arrive there as an ordinary undefined-module error
   * naming the package, which is a worse diagnostic rather than a silent one. Widening this to a
   * full walk of every statement is the fix if that ever happens in practice.
   */
  def checkDevImports(units: List[Program], devModules: Set[String])
      : Either[List[Diagnostic], Unit] =
    if devModules.isEmpty then Right(())
    else
      val offenders =
        for
          unit  <- stripSource(units)
          decl  <- unit.body.collect { case i: ImportDecl => i }
          named <- devModules.find(m => decl.show == m || decl.show.startsWith(s"$m."))
        yield Diagnostic(
          s"'${decl.show}' comes from '$named', which this project declares in " +
            "'dev_dependencies' — that is not fetched for anything depending on this project, so " +
            "only a '@tests' file or a '@test' function may import it. Move the import into the " +
            "tests, or declare the package in 'dependencies' if the library itself needs it",
          decl.pos)

      if offenders.isEmpty then Right(()) else Left(offenders)
}
