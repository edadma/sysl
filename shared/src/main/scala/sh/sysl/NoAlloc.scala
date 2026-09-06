package sh.sysl

/** What a module that declared `no alloc` may not do: make heap storage (`reference/modules.md §
 * Capabilities are a module property`).
 *
 * **It is asked of the typed tree rather than of each construction as it is built**, and that is the
 * whole design here. The constructions that allocate are written down in a dozen places across the
 * analyzer — a value boxed by its context, a slice whose elements are counted, every operation that
 * builds a fresh `string` — and a guard at each of them is a list nobody can read and that a
 * thirteenth site would quietly not join. The typed tree has the opposite property: a node that
 * allocates is a node, so the list below is the whole answer and a new one either appears in it or
 * is visibly absent.
 *
 * What is refused is **making** storage, never holding it. A `&T` handed in is retained and released
 * with no allocator in sight, because every object carries its own deallocation hook (`03`) — so an
 * allocator-free module can take a reference, keep it in a field, and drop it, and only the moment
 * the object came into being is gated.
 *
 * A module's own declarations are what it is held to. A call into a module that allocates is a
 * question about the module *graph*, and is answered where the graph is.
 */
trait NoAlloc extends AnalyzerBase {

  /** Whether `module` is allocator-free — because it gave the allocator up, or because the target
   * it is being built for has none (`reference/modules.md § The target's half needs no clause at
   * all`, `reference/packages.md § What a project is called`).
   *
   * The two arrive at the same place by design: a module's effective set is the target's
   * intersected with its own narrowing, so a target that provides no allocator makes every module
   * allocator-free without a clause being written anywhere. That is what a bare-metal build wants —
   * `no alloc` on every file of a kernel is ceremony, and forgetting it on one file is the bug the
   * clause existed to prevent.
   *
   * **The library is exempt from the target's half, and that is not a loophole.** Its files are
   * compiled into every program, so applying this to them would report the allocating half of the
   * standard module as a mistake in source the program's author did not write and cannot change.
   * What a program does with that half is still refused, one step later and in the right place: the
   * call into it is in the program's own body, and `Allocators.blame` lands the diagnostic there.
   */
  protected def noAlloc(module: String): Boolean =
    (!targetProvides(Capability.Heap) && !std.carries(module)) ||
      moduleNarrows.get(module).exists(_.contains(Capability.Heap))

  /** The same question asked of a module's **tests**, which have an answer of their own
   * (`reference/modules.md § A @tests file states its own capabilities`).
   *
   * **Only the module's half moves. The target's does not**, and the asymmetry is the honest part of
   * this: a `@tests` file may take back what the module gave up, because the module's clause is a
   * promise about what ships and the file does not ship. It cannot take back what the machine never
   * had. So a test that allocates is refused on a target with no allocator whatever its file says,
   * which is what keeps `sysl test --target thumb-freestanding` from reporting that vectors passed
   * where they could not have run.
   */
  protected def noAllocTests(module: String): Boolean =
    (!targetProvides(Capability.Heap) && !std.carries(module)) ||
      testNarrows(module).contains(Capability.Heap)

  /** Which of the two governs a declaration: the module's clause, or its tests'.
   *
   * **A test is scaffolding wherever it is written**, so the set this asks is the one every build
   * but `sysl test` drops — a declaration in a `@tests` file, a closure lowered inside one, and a
   * `@test` function itself. A `@test` in an ordinary file answering to the module while the
   * closure inside it answered to the tests would be one rule with a seam through the middle of a
   * single body.
   */
  private def noAllocFor(module: String, name: String, scaffolding: Set[String]): Boolean =
    if scaffolding(name) then noAllocTests(module) else noAlloc(module)

  /** Reports every construction that makes heap storage in a module that declared `no alloc`, and
   * every call out of one that arrives somewhere that does.
   *
   * The `main` statements are checked under the module of the file that carries them, since they are
   * that file's code however little they look like a declaration.
   *
   * **A generic is answered for by the body it was written as, and an instantiation of one by
   * nobody** (`reference/modules.md § A generic answers for what it wrote, not for what its caller
   * chose`). The clause is a promise about a module's own conduct, and a generic has no conduct
   * until a type is chosen — by somebody else, in a module of their own. So `abstracts` carries
   * what the definition-time pass of `reference/generics.md § Bounds` analyzed, where the declaring
   * module's own calls still have their names and a call through a bound is the trait's, and the
   * instantiations are passed over.
   *
   * They are passed over rather than dropped: an instance is still part of the program every other
   * walk goes through, so a module calling a generic that allocates is reported at *its* call, which
   * is the line its author can change.
   */
  protected def checkNoAlloc(
      funcs: List[TFunc],
      abstracts: List[TFunc],
      vals: List[TVal],
      vtables: List[TVtable],
      main: List[TStmt],
      mainModule: String,
      testFuncs: Set[String],
  ): Unit = {
    // Everything a test build keeps and every other build drops. `testOnlyDecls` is the `@tests`
    // files' declarations and the closures lowered inside any test body; the `@test` functions and
    // the hooks around them are named nowhere else, and are what the second half adds.
    val scaffolding = testOnlyDecls.toSet ++ testFuncs

    // Lazily, because building it walks every body in the program: a compilation with no clause
    // anywhere — which is almost all of them — should pay nothing at all for this pass.
    lazy val allocator = new Allocators(funcs, vtables)

    // A generic's body is walked against a pool of its own: the program's functions, plus every
    // abstract body under the name a call in *another* abstract body gave it. A generic calling a
    // generic names an instantiation nothing links, so without those entries the call leads nowhere
    // and a module could reach an allocator through a one-line generic of its own. The pools are
    // two rather than one because those names belong to no program — a real body that happened to
    // call something spelled the same would be answered with a body it never called.
    lazy val abstractly = new Allocators(aliased(abstracts) ::: funcs, vtables)

    for f <- funcs if !genericInsts(f.name) && noAllocFor(Modules.moduleOf(f.name), f.name, scaffolding) do
      val why = because(Modules.moduleOf(f.name), scaffolding(f.name))

      scan(f.body, why)
      allocator.blame(f.body, why)
    // Asked of **where the declaration was written** rather than of its key, which is the one place
    // the two part company. A member of a structural type is hoisted under a bare `tuple.display`,
    // whose key names no module — so reading the key puts the library's own tuple renderer in the
    // anonymous module, which is the *program's*, and a freestanding program is then refused for a
    // string the library builds. Every other kind of declaration answers the same either way.
    for f <- abstracts; module = scopeFor(f.name).module if noAllocFor(module, f.name, scaffolding) do
      val why = because(module, scaffolding(f.name))

      scan(f.body, why)
      abstractly.blame(f.body, why)
    for v <- vals if noAllocFor(Modules.moduleOf(v.symbol), v.symbol, scaffolding); init <- v.init do
      val why = because(Modules.moduleOf(v.symbol), scaffolding(v.symbol))

      scan(init, why)
      allocator.blame(init, why)
    // The entry file's statements are never scaffolding: a `@tests` file has no body to run, and a
    // program's own top-level code is what every build keeps.
    if noAlloc(mainModule) then
      scan(main, because(mainModule, scaffolding = false))
      allocator.blame(main, because(mainModule, scaffolding = false))
  }

  /** Each abstract body again under every name a call in another one gave it, so that a walk
   * following such a call arrives somewhere.
   *
   * A generic instantiated at a type parameter is spelled from the parameter — `lib$grow.T` — and
   * one instantiated at a real type during that same walk is spelled like an instantiation the
   * program might also have made. Both are answered here with the body the declaration wrote, which
   * is the only body either name has: `sandboxed` dropped everything that walk registered, so
   * nothing was ever analyzed under them.
   */
  private def aliased(abstracts: List[TFunc]): List[TFunc] = {
    val byName = abstracts.map(f => f.name -> f).toMap

    abstractInsts.toList.flatMap((mangled, decl) => byName.get(decl).map(_.copy(name = mangled)))
  }

  /** Which of the two made this module allocator-free, said the way the diagnostic needs it.
   *
   * The distinction is the whole reason it is carried rather than assumed: a reader told their
   * module "declared 'no alloc'" when no file of it says any such thing would go looking for a
   * clause that is not there. Where the target is what has no allocator, the thing to change is the
   * config or the target, and the message has to point at that instead.
   */
  private def because(module: String, scaffolding: Boolean): String =
    // **Told apart by the position the clause was read at**, which is the only thing that answers
    // it: a `@tests` file writing nothing inherits its module's entry, pos and all, so an entry
    // carrying a *different* position is one that file wrote. Naming the wrong file here sends a
    // reader to delete a line that is not there.
    val ours   = testNarrows(module).get(Capability.Heap)
    val theirs = moduleNarrows.get(module).flatMap(_.get(Capability.Heap))

    if scaffolding && ours.isDefined && ours != theirs then "this module's '@tests' file declared '@no_alloc'"
    else if (if scaffolding then ours else theirs).isDefined then "this module declared '@no_alloc'"
    else s"'${target.name}' provides no allocator"

  /** Which functions make heap storage, and which trees reach one (`reference/modules.md §
   * Capabilities are a module property` — *propagation is over the module graph*).
   *
   * **The diagnostic lands at the call rather than at the import**, and the reason is the standard
   * library: `sysl` is one module and is half allocator-free, so a rule stated over modules would
   * refuse every `no alloc` module that names anything at all — `print` and `from_utf8` are the
   * same module, and only one of them allocates. What `reference/modules.md § Capabilities are a
   * module property` asks for is that an allocator-free module "can only import and call things
   * that are themselves no-alloc-compatible", and calls are what this is stated over.
   *
   * The reachable set is the one `Reachability` computes **in its `written` mode**, which answers a
   * run-time target with the tables this code erased a value into rather than with every table for
   * the trait. The distinction is the whole of `reference/modules.md § Capabilities are a module
   * property`: the clause is a promise about a module's own conduct, and which `impl Writer` is
   * behind a `*Writer` parameter is its caller's choice, made in a module of their own.
   *
   * **The default walk over-approximates and is right to** — it exists for emission, where keeping a
   * function nobody calls costs a symbol and dropping one the program reaches is a link error. Asked
   * for a capability the same answer refuses a module for what somebody else's code does: writing
   * into a caller's sink was judged against `sysl.buf`'s `ByteSink`, so a module rendering into a
   * `*Writer` was legal or refused according to what the *program* linked, and no `Display` in
   * `library/` could carry the clause at all.
   *
   * **What is not relaxed is making the sink yourself.** A body that builds a growable buffer and
   * hands it over as a trait object wrote the erasure, so the table is named in its own tree and the
   * walk follows it — which is the case that would otherwise escape, and the reason this is a
   * narrowing rather than a hole.
   */
  private class Allocators(funcs: List[TFunc], vtables: List[TVtable]) {

    /** The functions whose own bodies make heap storage. A library's do; a program's do wherever it
     * was allowed to write one.
     */
    private val direct: Set[String] = funcs.filter(f => firstAllocation(f.body).isDefined).map(_.name).toSet

    private val walk = new Reaches(funcs, vtables, direct)

    /** Whether this tree arrives at an allocating function at all. */
    def reaches(x: Any): Boolean = walk.reached(x).nonEmpty

    /** One message per smallest sub-tree that still reaches an allocator — see `Reaches.blame`, which
     * is where the descent and its reasons live.
     */
    def blame(x: Any, why: String): Unit =
      walk.blame(x) { (pos, who) =>
        recover(())(at(pos)(err(s"this reaches '${Modules.show(who)}', which makes heap " +
          s"storage, and $why — an allocator-free module may only call what is allocator-free itself")))
      }
  }

  /** The first construction under `x` that makes heap storage, wherever it is. */
  private def firstAllocation(x: Any): Option[TExpr] = x match
    case _: Type                            => None
    case e: TExpr if allocates(e).isDefined => Some(e)
    case xs: Iterable[?]                    => xs.iterator.flatMap(firstAllocation).nextOption()
    case p: Product                         => p.productIterator.flatMap(firstAllocation).nextOption()
    case _                                  => None

  /** What a node allocates, said the way a reader would say it, or nothing for a node that does not.
   *
   * `TDowngrade` is deliberately absent: a weak reference is a count inside the box the strong one
   * already made (`reference/memory.md § What a heap object costs`), so weakening allocates
   * nothing. Making a `weak T` is gated all the same, one step earlier — the `&T` it has to come
   * from is `TBox`, and there is no other route to one.
   */
  private def allocates(e: TExpr): Option[String] = e match
    case _: TBox                       => Some("a reference")
    // An **empty** one makes none: a view of nothing is `{null, null, 0}`, which is the zero value
    // of its own representation and reaches no allocator. `[]` is therefore ordinary in an
    // allocator-free module, which is what a function returning "no results" wants to write.
    case TBufLit(Nil, _)               => None
    case _: TBufLit | _: TBufFill      => Some("a slice with storage of its own")
    case _: TStr | _: TRender          => Some("the string a value renders as")
    case _: TFormat                    => Some("the string a formatted value renders as")
    case _: TFromBytes                 => Some("a string built from bytes")
    case TBinary(_, _, _, Type.Str)    => Some("the string two strings join into")
    case _                             => None

  /** Walks a tree, reporting the outermost allocation on each path and going no deeper into one.
   *
   * Stopping at the outermost is what keeps `str(a) + str(b)` from being three messages about one
   * line. What a reader has to change is the expression, and the expression is the node that was
   * reported; the pieces inside it go away with it.
   *
   * The descent is through the shape of the tree rather than a case per node, for the reason
   * `Reachability`'s is: a node added later is walked without anyone remembering to come back here.
   */
  private def scan(x: Any, why: String): Unit = x match
    case _: Type => ()
    case e: TExpr if allocates(e).isDefined =>
      recover(())(at(e.pos)(err(s"${allocates(e).get} needs an allocator, and $why — it may hold and " +
        "release storage made elsewhere, and may make none of its own")))
    case xs: Iterable[?] => xs.foreach(scan(_, why))
    case p: Product      => p.productIterator.foreach(scan(_, why))
    case _               => ()
}
