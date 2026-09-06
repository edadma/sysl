package sh.sysl

import scala.collection.mutable

/** The order the passes run in.
 *
 * The driver that walks a module's files and drains the work each pass queues (`analyze`), and the
 * two questions about a program's beginning that only it can answer: that there is one of them, and
 * what a declared `main` may look like.
 *
 * The order is the content. Nearly every line of `analyze` is a pass that could not have run
 * earlier, and the comments say which fact it was waiting on — a trait's supers wait for every
 * trait to be registered, a bound's check waits for the `impl` blocks below it, the module graph
 * waits for every reference to have resolved. Reading it top to bottom is reading why the compiler
 * has the phases it has.
 *
 * What each pass then *does* sits in the traits underneath, in the order the driver reaches them:
 * `ModuleFiles` (what a file contributes), `ModuleStorage` (the `val`s and `var`s it lays down),
 * `AbstractBodies` (`reference/generics.md § Bounds`'s definition-time check) and `FunctionBodies`
 * (running one, which is what the drain is draining towards).
 *
 * `units` — the files being analyzed together — is supplied by the class and declared alongside the
 * other things a walk is told about its compilation (`DeclTables`).
 */
trait ProgramWalk extends OpaqueResults with DropReturnCheck {

  /** The imports a file starts with, before it has written any of its own (`AutoImport`).
   *
   * A module is brought in only where this compilation actually has it, so a program built without
   * the library carries nothing. A file of the auto-imported module itself is left alone: its own
   * declarations already resolve first, and a file wildcard-importing the module it is in would
   * read as though it needed to.
   */
  private def autoImported(module: String): Imports =
    Imports(wildcards = AutoImport.modules.filter(m => m != module && moduleNames(m)))

  def analyze(): TProgram = {
    checkLocations()
    checkLibraryModules()
    // What a module may do is settled before anything in it is read, because the first construction
    // the walk reaches is already a question about it.
    readCapabilities()
    readLinkDirectives()
    moduleNames ++= units.map(moduleOf)
    // The library's own modules are modules like any other, and are known for the same reason a
    // file's header is: what they are called is settled before a single name is resolved. They are
    // read off the standard module this compilation was handed rather than off the source in the tree, because
    // naming one is reaching declarations, and the declarations that are there are the ones that
    // arrived.
    moduleNames ++= std.modules

    // Every declaration is read in the terms its file set up — the module it contributes to and
    // what it imported — so each one is carried alongside those rather than flattened into one
    // list. They are what a name in its signature, its fields, and its body resolves against
    // (`reference/modules.md § Imports`), and the imports can be read now because which module a
    // path names is settled by the headers alone. The imports are gathered in the file's own terms,
    // since what an import may reach is a question about where it was written
    // (`reference/modules.md § Visibility`) as much as about what it names.
    def scopeOf(u: Program): Scope = {
      val here = moduleOf(u)
      val base = Scope(here, Imports.empty, Some(u.source))

      base.copy(imports = inScope(base)(gatherImports(u.body, autoImported(here))))
    }

    val files = units.map(u => u -> scopeOf(u))
    // The library's files go through the same construction, because they are files of modules like
    // any other — one of them may import a sibling module of the library, and until there was more
    // than one library module there was nothing for such an import to name.
    val library = std.contributed(building).map(u => u -> scopeOf(u))

    // Which file the program starts in is settled **before** anything is hoisted, because it decides
    // what the hoisting passes are given: that file is a body, so its `val`s and its functions are
    // local to it and are not the module's to register. Every other file hands over everything it
    // has.
    val entry = entryFile(files)

    refuseTwoBeginnings(files, entry)

    // `static` is how a declaration in that body opts back into the module, so past this point
    // nothing else in the compiler has a case for one: the inner declaration is handed to the
    // hoisting passes exactly as another file's would be.
    def unstatic(s: Stmt) = s match
      case StaticDecl(inner) => inner
      case other             => other

    // A function at the top of the entry file is the module's unless it reads something the body
    // binds. `reference/declarations.md`'s limits — no generic, no address, not a value — are what
    // holding a frame costs, so a function holding none keeps everything an ordinary one has.
    val captures = entry.map((u, _) => Bodies.capturing(u.body)).getOrElse(Set.empty)

    def belongsToModule(s: Stmt) = s match
      case f: FuncDecl => !captures(f.name)
      case other       => Bodies.isModuleMember(other)

    def contributed(u: Program) =
      if entry.exists(_._1 eq u) then u.body.filter(belongsToModule).map(unstatic)
      else u.body.map(unstatic)

    // Everywhere else it says nothing, because everywhere else there is no body for a declaration to
    // belong to instead — so it is refused rather than quietly accepted as a no-op. A file with no
    // statements is one of those places, which is why this reads "the file the program starts in"
    // rather than "another file": a program with no entry file at all has nowhere to write one.
    for (u, _) <- files if !entry.exists(_._1 eq u); s <- u.body do
      s match
        case StaticDecl(_) =>
          currentPos = s.pos
          recover(())(err("'static' says a declaration belongs to the module rather than to the body " +
            "of the file the program starts in — and this file has no such body, so everything it " +
            "declares is the module's already"))
        case _ =>

    val written = (library ::: files).flatMap((u, s) => contributed(u).map((s, _)))

    // A `derives` clause is answered here, before anything is hoisted: what it names becomes an
    // ordinary `impl` block standing directly after the declaration that asked for it, and every
    // pass below sees a program in which somebody wrote those blocks out (`Deriving`).
    //
    // The clause is checked first and separately, because the two questions are different. What is
    // wrong with the *clause* — a trait the compiler cannot write, arguments on one, the same trait
    // twice — is about the words in front of the reader, and each entry is its own recovery region
    // so a clause with two mistakes in it reports both. What is wrong with a derived *block* is
    // about the type's fields, and it surfaces exactly where a written block's would.
    for (_, stmt) <- written; entry <- Deriving.clause(stmt) do
      currentPos = entry.pos
      for message <- Deriving.problem(entry, stmt) do recover(())(at(entry.pos)(err(message)))

    for (_, stmt) <- written; entry <- Deriving.duplicates(Deriving.clause(stmt)) do
      currentPos = entry.pos
      recover(())(at(entry.pos)(err(s"'${entry.show}' is named twice by one 'derives' clause")))

    val body = written.flatMap((scope, stmt) => (scope, stmt) :: Deriving.expand(stmt).map((scope, _)))

    // What a `@tests` file declares, before anything is hoisted, because hoisting is where a
    // declaration stops remembering which file wrote it (`reference/attributes.md § @tests — a file of scaffolding`). The library's files go
    // through it too: they are files of modules like any other, and the standard module having no
    // test files today is a fact about today.
    for (u, s) <- library ::: files if u.testOnly do
      testOnlyDecls ++= contributed(u).flatMap(Tests.declaredNames).map(Modules.qualify(s.module, _))

      // An `impl` block is the one declaration a test file may not write, and the reason is that it
      // does not declare a *name* — it puts an entry in a method table, which is a claim about a
      // trait and a type that the rest of the program reads without naming anything at all. Kept in
      // a test build and dropped everywhere else, it would mean a trait resolved one way while the
      // tests ran and another way in the program that shipped. The impl belongs beside the type.
      for i <- contributed(u).collect { case i: ImplDecl => i } do
        currentPos = i.pos
        recover(())(err("an 'impl' block may not sit in a file that said '@tests' — a test build " +
          "would keep it and every other build would drop it, so a trait would answer one way while " +
          "the tests ran and another way in the program that ships. It belongs beside the type"))

    // Each declaration, each function body, and each statement is a **recovery region**: a
    // failure inside one is recorded and the region abandoned, and the walk resumes at the next.
    // That is what turns one error per compilation into one error per mistake.
    //
    // Hoisting runs at the top level, where there is no enclosing position to return to, so each
    // pass simply moves the cursor to the declaration it is registering.
    // The reserved shape is refused before anything is registered, so a declaration that tried to
    // take one is answered by *that* rather than by the consequences of a name the rest of the
    // compiler then could not resolve (`ReservedNames`). One diagnostic per offending name rather
    // than one per declaration: a struct with two reserved fields made two mistakes.
    for (_, stmt) <- body; (name, what, pos) <- ReservedNames.declaredIn(stmt) do
      currentPos = pos
      recover(())(at(pos)(refuseReserved(name, what)))

    for (scope, stmt) <- body do
      currentPos = stmt.pos
      inScope(scope)(recover(())(hoistType(stmt)))

    // What each trait requires of the types implementing it is answerable now that every trait is
    // registered, and not before: `trait Ord: Eq` is ordinary whichever of the two is written first.
    checkTraitSupers()

    // And what each generic type's defaults stand in for, which is answerable for the same reason
    // and not before: a default may name a type declared below the one that defaults to it.
    checkTypeDefaults()

    // Every constant is folded now, whether or not anything reads it. Folding is lazy so that one
    // may be written in terms of another declared below it, and an unused constant would otherwise
    // never be looked at — leaving a value that does not fit its type, or is not constant at all, as
    // a mistake nobody is told about. It is the declaration that is wrong, so the declaration is
    // where it is reported, and each one is its own recovery region.
    for (key, _) <- constDecls do
      currentPos = constDecls(key).pos
      recover(())(constLiteral(key))

    // The asserts go directly after, and the order is what makes them useful: every constant has
    // been folded by now, so a condition may name one wherever it was declared. Each is its own
    // recovery region for the reason a constant is — a program with two bad asserts should be told
    // about both rather than about whichever came first.
    for (a, scope) <- assertDecls do
      currentPos = a.pos
      recover(())(inScope(scope)(checkAssert(a)))

    // A non-generic type is instantiated eagerly, so it is emitted whether or not it is used;
    // a generic one only exists once something asks for a concrete instantiation.
    //
    // A bad one is reported where the constants above are: at the declaration, since that is the
    // line that is wrong. Without the position these loops set, the complaint landed at whatever the
    // walk had last looked at — for a program whose only mistake was its own first line, that was a
    // trait in the library. Failing here also marks the name broken, so that the mentions of the type
    // further down abandon their own statements without repeating what the declaration was already
    // told.
    def eagerly(key: String, decl: Positioned)(build: => Type): Unit =
      currentPos = decl.pos
      if !recover(false) { build; true } then brokenDecls += key

    for (n, d) <- enumDecls if d.tparams.isEmpty do eagerly(n, d)(instantiateEnum(n, Nil))
    for (n, d) <- structDecls if d.tparams.isEmpty do eagerly(n, d)(instantiateStruct(n, Nil))

    // Every constrained subtype is resolved now, whether or not anything uses it — so an out-of-range
    // or inverted bound is a mistake reported at the declaration, exactly as a constant's is.
    //
    // **A plain alias is resolved here too and by a different road**, since it builds no constrained
    // type to be checked: what it owes at its declaration is that the name it stands for exists, and
    // that following it terminates. A cycle is caught by the walk rather than by the resolve, which
    // would otherwise recurse until the stack ran out.
    for (key, d) <- constrainedDecls do
      currentPos = d.pos
      inScope(declScope(key))(recover(()) {
        if plainAlias(key) then
          aliasedKey(key)
          resolveAlias(key)
        else resolveConstrained(key)
      })

    for (scope, stmt) <- body do
      currentPos = stmt.pos
      inScope(scope)(recover(())(hoistFunc(stmt)))

    // Every declaration exists now, so what an `import` claimed a module declares can be looked up
    // — and every import from here on, a block's, is checked as it is read.
    checkImportTargets()

    // How far each declaration reaches is settled too, so a signature can be held to naming nothing
    // that reaches less far than it does (`reference/modules.md § Visibility`). It waits until here
    // because the question is about two declarations at a time, and either may be written below the
    // other.
    checkExposedTypes()

    // And a trait's members are resolved, which nothing else does: they lower to no function, so
    // this is the only pass that reads them before something implements the trait
    // (`reference/traits.md § A default may assume exactly what its own trait declares`).
    checkTraitSignatures()

    // And what each `&sync T` promises about its pointee, which waits for the same reason a bound
    // does: a type that reaches itself through a `&sync` field is resolved while its own field list
    // is still being filled, so the question is held until every field is in (`reference/memory.md
    // § Crossing a concurrency domain`).
    typesHoisted = true
    for (inner, pos) <- sharedChecks.toList do
      currentPos = pos
      recover(())(Sharing.complaint(inner).foreach(err))
    sharedChecks.clear()

    // A type's members lower to ordinary functions under mangled names, registered here so a
    // method call and an associated-function call resolve exactly as a free call does.
    val members = mutable.ListBuffer.empty[FuncDecl]
    for (tname, sdecl) <- structDecls do
      at(sdecl.pos)(inDecl(tname)(recover(())(hoistMembers(tname, sdecl.members, members))))
    for (tname, edecl) <- enumDecls do
      at(edecl.pos)(inDecl(tname)(recover(())(hoistMembers(tname, edecl.members, members))))
    for (scope, impl) <- implDecls.toList do
      at(impl.pos)(inScope(scope)(recover(())(hoistImpl(impl, members))))

    // Whether a type argument implements what the type asked of it is answerable only now: the
    // signatures resolved above were read before the `impl` blocks below them were registered, so
    // the question was held rather than answered against a table still being filled.
    implsHoisted = true
    for b <- boundChecks.toList do
      currentPos = b.pos
      // In the terms the bound was written in, not in whatever the walk was last reading: a bound is
      // a reference like any other, and a short name means what the file that wrote it imported.
      inScope(b.scope)(recover(())(checkParamBounds(b.what, b.tparams, b.bounds, b.targs, noun = b.noun)))
    boundChecks.clear()

    // And whether each `impl` of a trait that requires others supplies those too, which is the same
    // question about the same table and so waits for the same moment.
    checkImplSupers()

    // And whether each associated type an `impl` supplied is a member of what its trait asked of
    // it, which is that question one step over: the block making the *supplied* type a member may
    // be written below the block that chose it.
    checkAssocBounds()

    // And whether each block marked `override` has something to override, which waits for the same
    // moment and for the same reason: the block being replaced may be hoisted after it.
    checkOverrides()

    // And whether any declaration hands back a type whose destructor could then never run, which
    // waits for the same moment for the same reason — `dropsDeclared` is filled by the hoisting
    // above (card `0372`).
    checkDropReturns()

    // Every `some` result is settled here — the one pass that reads a body in order to learn a
    // *type*. It waits for this moment because such a body may name anything the program declares,
    // and it runs before the pass below because that one, and every body after it, may ask what an
    // associated type is (`OpaqueResults`).
    settleOpaqueResults()

    // Every generic body is checked once here, against its bounds alone, before any instantiation
    // is looked at. That is what makes `sum[T](a: T, b: T) = a.plus(b)` fail on its own line
    // instead of at whichever call site first supplied a type without a `plus`.
    checkAbstractBodies()

    // Every `val` is laid down here, before the first body that might read one and in a state
    // belonging to no function — which is the point. A `val`'s initializer is a *module member's*
    // expression, so the locals of whichever function first mentioned it must not be in scope while
    // it is read, and analyzing them all in one place ahead of time is what guarantees they are not.
    // Like a constant's, an unused one is still checked: it is the declaration that is wrong.
    val tvals = mutable.ListBuffer.empty[TVal]

    resetFunction()
    tsubst = Map.empty
    // An `extern` variable's type is resolved here, before any body that might name one, and whether
    // or not anything does: it is the declaration that is wrong, exactly as an unused `val`'s is.
    // Nothing is built from it — the storage is somebody else's — so this is the check and no more.
    for (key, decl) <- externVarDecls do
      currentPos = decl.pos
      recover(())(at(decl.pos)(checkExternVar(key)))
    // A module `var` is laid down with the `val`s and in the same state, for the same reason: its
    // initializer is a module member's expression, so no function's locals may be in scope while it
    // is read. It goes first only so that a `val` initialized from one reads storage already there.
    for (key, decl) <- staticVarDecls do
      currentPos = decl.pos
      tvals ++= recoverOpt(analyzeStaticVar(key))
    for (key, decl) <- valDecls do
      currentPos = decl.pos
      tvals ++= recoverOpt(analyzeVal(key))

    // And every parameter's default, for the same reason and in the same state: a default is a
    // module member's expression too, filled at a call but written here, so it is checked where it
    // is written and whether or not any call takes it (`reference/declarations.md § Default
    // parameters and named arguments`).
    checkValueDefaults()

    val tfuncs = mutable.ListBuffer.empty[TFunc]

    // A function whose body did not analyze is left out of the program rather than stood in for:
    // there is nothing to emit, and nothing will be emitted at all while an error stands.
    //
    // The library's own functions are held back: they are analyzed below, and only if something
    // reaches them. That keeps a program that never prints from carrying the printing surface.
    //
    // The hoisted declarations are what is walked, rather than the source statements, because
    // hoisting is where each one was renamed to the key its module gives it — an `extern` is left
    // out by the table that says which names have no body rather than by its declaration form. Read
    // off the declarations rather than the bodies, so that a handler nothing instantiates and
    // nothing reaches is judged exactly as one that does (`reference/ffi.md § interrupt`).
    checkConventions()
    // Read off the declarations for the same reason (`reference/ffi.md § @export`).
    checkExports()

    // And a doc comment's tags against the signature it sits above — the same reason a third time,
    // plus one of its own: a doc comment belongs to the *file* the lexer found it in and is placed
    // by a line number, and hoisting is where a declaration stops remembering which file wrote it.
    // So this walks `units` rather than the tables. Only what the tags NAME is checked; a parameter
    // with no `@param` is not an error and must not become one.
    for u <- units; (tag, message) <- DocComments.problems(u) do
      recover(())(at(Some(Pos(u.source, tag.line, 1, tag.line, 1)))(err(message)))

    val (fromLibrary, ours) = funcDecls.values.toList
      .filter(f => f.tparams.isEmpty && !externDecls.contains(f.name))
      .partition(suppliedByLibrary)

    for f <- ours do
      tfuncs ++= recoverOpt(analyzeFuncBody(f.name, f, Map.empty))

    // A member is held back on the same terms a free function is, and for the same reason: a
    // library *type* with members would otherwise put every one of them into every program, which
    // is the cost the rule above exists to avoid, one declaration form further in. Only the ones
    // with nothing left to instantiate qualify — a generic member is already reached through the
    // queue rather than from here.
    val (libraryMembers, ourMembers) = members.toList
      .filter(f => !defaultOrigin.get(f.name).exists(brokenDefaults))
      .partition(f => f.tparams.isEmpty && suppliedByLibrary(f))

    for f <- ourMembers do
      tfuncs ++= recoverOpt(analyzeFuncBody(f.name, f, Map.empty))

    val (mainScope, mainStmts) = entryPoint(entry, captures)

    resetFunction()
    tsubst = Map.empty
    retTy = Type.Int
    currentModule = mainScope.module
    currentImports = mainScope.imports
    currentFile = mainScope.file
    val tmain = inBlock(mainStmts)(mainStmts.flatMap(recoverStmt))

    // The `main` the program runs after those statements, if it declared one. Read here rather than
    // during hoisting because the conversion it may want is a library function, and marking one as
    // reached has to happen before the loop below decides which of them are worth analyzing.
    val tentry = recover(Option.empty[TEntry])(declaredEntry())

    // Draining the queue may itself discover further instantiations, so it runs to a fixpoint. An
    // instantiation of a member the definition-time pass already reported is dropped rather than
    // analyzed, for the reason that pass exists: the diagnostic naming the missing bound is the one
    // worth reading, and every instantiation would add another about a consequence of it.
    def drain(): Unit =
      while pending.nonEmpty do
        val (mangled, decl, subst) = pending.dequeue()
        val reported = brokenMembers(decl.name) || defaultOrigin.get(decl.name).exists(brokenDefaults)

        if !reported then tfuncs ++= recoverOpt(analyzeFuncBody(mangled, decl, subst))

    drain()

    // A library function is analyzed only once something has called it, and analyzing one may call
    // another — `printi` reaches `putbytes`, `printb` reaches `prints` — so this runs to a fixpoint
    // too. Nothing reaches them in a program that never prints, and none of them is emitted.
    val available = (fromLibrary ::: libraryMembers).map(f => f.name -> f).toMap
    val analyzed  = mutable.HashSet.empty[String]
    var reached   = true

    while reached do
      reached = false
      for name <- funcsUsed.toList if available.contains(name) && !analyzed(name) do
        analyzed += name
        reached = true
        tfuncs ++= recoverOpt(analyzeFuncBody(name, available(name), Map.empty))
      drain()

    // Every reference the program makes has been resolved, so which module depends on which is
    // finally settled and the graph can be held to being acyclic (`reference/modules.md § The
    // module graph is acyclic`).
    checkModuleGraph()

    // And which module a reference lands in is what decides whether a module that gave up an
    // environment capability was allowed to make it, so this asks the same settled graph
    // (`reference/modules.md § Capabilities are a module property`).
    checkGatedModules()

    // Which structs can lie inside one that carries invariant clauses is likewise only settled now,
    // so the rule about what a `*self` method may let out of the call is asked here
    // (`reference/errors.md § Struct invariants`).
    checkSelfAliasing((tfuncs ++ closureFuncs).toList)

    val externs = externsUsed.toList.map { name =>
      val (params, rtype) = funcInsts(name)
      val e               = externDecls(name)
      TExtern(name, e.symbol, params.map(_._2), rtype, e.variadic)
    }

    // Every function the program declares is here, whether or not anything can reach it — analysis
    // is eager, and a body nobody calls is checked and reported exactly as one that is called.
    // Dropping the unreachable ones is `Reachability.prune`, which runs after the passes that read
    // the whole program and just before it is lowered.
    //
    // An instantiation at a **type parameter** is a diagnostic type, not a laid-out one: a `Box[U]`
    // written as a trait's argument in a generic `impl` names the family the block covers, and no
    // value at run time has that type. It has no layout to emit — `Type.Abstract.llvm` says so by
    // refusing — so it is dropped here rather than reaching a backend that would have to invent one.
    // Which `val` needs which is settled by what their initializers reach, so the order they are
    // filled in can only be worked out once every body those initializers call has been analyzed —
    // the same reason the module graph is held to being acyclic here rather than earlier.
    val allFuncs = (tfuncs ++ closureFuncs).toList

    // Only the hooks whose bodies survived analysis, for the reason the same filter is applied to
    // the tests below: a declaration that was reported is not one the dispatcher can lay an arm
    // down for. Everything that asks "is this test scaffolding?" is asked of the two together —
    // a hook is dropped by the same builds, may name what a `@tests` file declares, and is held to
    // its module's test-side capability clause exactly as a test is.
    val liveHooks   = hooks.filter(h => allFuncs.exists(_.name == h.func)).toList
    val scaffolding = (tests.map(_.func) ++ liveHooks.map(_.func)).toSet

    // Asked of the finished tree, because what allocates is a node rather than a place in the
    // analyzer — and asked here rather than after `analyze` returns, so that a module doing what it
    // declared it would not is one of this walk's diagnostics like any other.
    checkNoAlloc(allFuncs, abstractFuncs.toList, tvals.toList, vtables.values.toList, tmain, mainScope.module,
      scaffolding)

    // And what a declaration that wrote `@needs(...)` costs whoever reaches it — the same question
    // one granularity down, asked of the same tree (`reference/modules.md § A declaration may name
    // what reaching it needs`).
    checkDeclCapabilities(allFuncs, tvals.toList, vtables.values.toList, tmain, mainScope.module,
      scaffolding)

    // And what a `@pure` function promised, asked of the same tree for the same reason
    // (`reference/verification.md § @pure`).
    checkPurity(allFuncs, externs)

    // And what a `@reads`/`@writes` frame promised (`reference/verification.md § @reads and @writes
    // — what a call may touch`). It runs beside purity rather than inside it because the two answer
    // different questions about the same nodes: purity asks whether a caller could observe anything
    // at all, a frame asks which storage in particular.
    checkFrames(allFuncs, externs)

    // And where a `@ghost` function may be called from (`reference/verification.md § @ghost — what
    // costs nothing to say`), which is the rule that makes erasing one sound.
    checkGhost(allFuncs, tmain)

    // And who may name what a `@tests` file declared (`reference/attributes.md § @tests — a file of scaffolding`), which is the rule that makes
    // dropping one sound in the same way.
    checkTestScope(allFuncs, tmain, testOnlyDecls.toSet, scaffolding)

    // A closure lowered while a **generic body** was analyzed carries that body's type parameters in
    // its own signature, and no value at run time has such a type. It is the same case as the struct
    // and enum instantiations dropped two lines below, and it is dropped for the same reason:
    // `Type.Abstract.llvm` has no layout to give `T` and says so by throwing, so a backend must never
    // be handed one.
    //
    // **The checks above still see it**, which is why this filters here rather than narrowing
    // `allFuncs`: a closure written inside a generic is a body to report allocation, purity and frame
    // violations against exactly like any other, and only the *emission* is wrong.
    //
    // **`orderVals` is given `allFuncs` and not this**, which is the one place the distinction has
    // teeth rather than being tidiness. It settles the order module-level `val`s are initialized in,
    // from what each initializer *reaches* — so narrowing what it can see would drop edges out of that
    // graph and could reorder initialization, which is a silent, program-wide behaviour change and
    // exactly what this fix must not be. Analysis sees everything; only the backend sees less.
    //
    // **It escapes on the library path alone**, which is why it went unnoticed. A program
    // instantiates the enclosing generic and gets a concrete copy beside the abstract one, so the
    // abstract one is dead weight the backend never asks about; `build-lib` strips `@tests`
    // *before* analysis (`Compiler.compileLibrary`), so in a library nothing instantiates the
    // generic at all and the abstract closure is the only copy there is. `sysl.slices`' `sort[T:
    // Ord](xs) = sort_by(xs, (a, b) -> a < b)` is the shape that found it, and `reference/types.md
    // § Function types` names a comparator passed to a sort as the bare arrow's motivating case —
    // so this is a shape the language invites.
    // Every test is handed its own module's hooks, which is what lets the runner work from a list of
    // tests alone — a cached suite arrives from a sidecar with no program behind it at all.
    val hooksByModule = liveHooks.groupBy(h => Modules.moduleOf(h.func))
      .view.mapValues(THooks.of).toMap.withDefaultValue(THooks())

    val emitted = allFuncs.filterNot(f =>
      f.params.exists((_, t) => Type.mentionsAbstract(t)) || Type.mentionsAbstract(f.retTy))

    TProgram(
      structInsts.values.filterNot(abstracted).toList,
      enumInsts.values.filterNot(e => e.simple || abstracted(e)).toList,
      vtables.values.toList,
      externs,
      orderVals(tvals.toList, allFuncs, vtables.values.toList),
      emitted,
      tmain,
      tentry,
      noAllocModules = moduleNarrows.collect { case (m, caps) if caps.contains(Capability.Heap) => m }.toSet,
      // The same set for the module's *tests*, which may have taken the allocator back. Read
      // through `testNarrows`, so a module with no `@tests` file answers with its own clause and
      // the two sets agree — which is what they did before a test file could differ.
      noAllocTestModules = (moduleNarrows.keySet ++ moduleTestNarrows.keySet)
        .filter(m => testNarrows(m).contains(Capability.Heap))
        .toSet,
      mainModule = mainScope.module,
      // Only the tests whose bodies survived analysis. A test whose body was reported is not a test
      // the runner could run, and listing it would put a name in the report that no dispatcher arm
      // matches — which reads as a test that vanished rather than as the error already printed.
      tests = tests.filter(t => allFuncs.exists(_.name == t.func))
        .map(t => t.copy(hooks = hooksByModule(Modules.moduleOf(t.func)))).toList,
      hooks = liveHooks,
      externVars = externVarsUsed.toList.map(k => TExternVar(externVarDecls(k).symbol, externVarType(k))),
      // Everything a `@tests` file declared, whether or not analysis kept it — unlike the tests
      // above, this is asked of a *tree that is about to be dropped*, so a name that reached no
      // typed declaration costs a set entry nothing will match rather than a dispatcher arm nothing
      // answers.
      testOnly = testOnlyDecls.toSet,
      destructors = destructorsOf,
      warnings = warnings,
      // The graph `checkModuleGraph` and `checkGatedModules` have just read, carried out of the
      // analyzer because one question about it is asked after this walk: which of a dependency's
      // exports this program reaches (`Reachability.prune`).
      moduleDeps = moduleEdges.keys.toList.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap,
    )
  }

  /** Every type this compilation instantiated that has a destructor, paired with the symbol of it
   * (`reference/memory.md § A destructor`).
   *
   * Asked of the **instantiated** types rather than of the `impl` blocks, because an `impl` for a
   * generic type is one block covering a family and the hook is emitted per concrete payload. A
   * type the program never made needs no entry: nothing can release a box of it.
   */
  private def destructorsOf: Map[String, String] = {
    val made = structInsts.values.toList ::: enumInsts.values.toList

    made.collect {
      case t: Type if dropsDeclared(memberOwner(t)._1) =>
        Type.mangle(t) -> recover(s"${Type.memberSymbol(t)}.drop")(memberFuncName(t, "drop"))
    }.toMap
  }

  /** The `main` the program declared, and how its arguments are made.
   *
   * A program's top-level statements are its entry point (`reference/modules.md § Where a program starts`) and go on being that. What a
   * declared `main` adds is a *named* place to put the work those statements would otherwise do, and
   * the one thing the statements cannot get at: the arguments the program was started with. So it is
   * additive — the statements run first, in the order they were written, and `main` runs after them.
   *
   * **`main` names one function in a program**, wherever it is written, so a module may not have one
   * of its own beside the one the program starts at. That is the same reservation C makes and for the
   * same reason: it is not a name a program calls, it is the name the platform calls, and two of them
   * would leave which one the program *is* to whichever was emitted last.
   *
   * The two signatures are `main()` and `main(args: []string)`. Nothing else: a result would be an
   * exit status, which nothing in the language spells yet, and a parameter list of the platform's own
   * `argc`/`argv` is exactly what `args_of` exists so that no program has to write.
   */
  /** A program starts in **one** place, and statements at the top of a file and a `main` are two ways
   * of writing that place (`reference/modules.md § Where a program starts`).
   *
   * A program writing both would have two entry points and an order between them to remember, which
   * is what it reads as to anybody who opens it. Whichever of the two the program means, the other
   * belongs inside it — and what a `main` has that statements do not is a parameter list, so a
   * program wanting the arguments writes `main` and puts inside it what it would have written above.
   *
   * Reported here rather than where `main` is resolved, because it is the *cause* of whatever else
   * goes wrong: a `main` written beside statements reaches for their bindings and is told about each
   * one of them, and those complaints are consequences of a program having two beginnings.
   */
  private def refuseTwoBeginnings(files: List[(Program, Scope)], entry: Option[(Program, Scope)]): Unit =
    for
      (u, _) <- entry
      (f, _) <- files
      m      <- f.body.collectFirst { case d: FuncDecl if d.name == "main" => d }
    do
      currentPos = m.pos
      // The file's name goes last so that a page quoting this can quote the sentence and leave the
      // name out — a name that is whatever the reader called their file, and never part of the point.
      recover(())(err("a program starts in one place, and this 'main' is a second — whichever of the " +
        "two the program means, the other belongs inside it. The statements this program starts " +
        s"with are in ${u.source.name}"))

  private def declaredEntry(): Option[TEntry] = {
    val declared = funcDecls.keys.filter(k => Modules.bare(k) == "main" && !externDecls.contains(k)).toList

    declared match
      case Nil => None
      case key :: rest =>
        val decl = funcDecls(key)

        for extra <- rest do
          at(funcDecls(extra).pos) {
            recover(())(err(s"'main' is where a program starts, so there is one — " +
              s"${Modules.show(key)} already declares it"))
          }

        at(decl.pos) {
          if decl.tparams.nonEmpty then
            err("'main' is called by the platform, which has no type arguments to give it")

          val (params, ret) = funcInsts(key)

          // **A `main` either yields nothing or answers with a `Result[unit, E]`**, and the second
          // is what lets `?` reach the top of a program. Without it every fallible call in `main`
          // ends in `.unwrap()`, which reports a failure as a panic naming the line that gave up
          // rather than the thing that went wrong.
          //
          // Nothing else is admitted. A status is one byte and a return type is a value: mapping
          // one onto the other is the program's business, and `exit` is how a program that wants to
          // choose its own says so.
          val fallible = ret match
            case e: Type.Enum if e.base == Library.key("Result") && Type.noValue(e.targs.head) =>
              Some(e)
            case _ =>
              if !Type.noValue(ret) then
                // **And the refusal names the answer**, which is the whole of what it was missing:
                // a reader here wants to choose a status, `exit` is how, and being told only what a
                // signature may not do leaves them looking for a language feature that is already
                // built. Reported as card `0326` by somebody who read this message and concluded
                // sysl could not do it.
                err(s"'main' yields nothing or a 'Result[unit, E]', so it may not result in " +
                  s"${show(ret)} — a program's exit status is not something a signature can say. " +
                  s"'exit(code)' is how a program chooses its own, and prints nothing")
              None
          if decl.variadic then err("'main' is called with the arguments the platform has, not a list it reads")

          // The reporter is instantiated at the error type `main` named, and its bound is what
          // holds that type to being renderable — an error nobody can print would otherwise exit
          // non-zero with nothing said.
          val reporter = fallible.map { r =>
            val fd = funcDecls(Library.key("main_result"))

            checkParamBounds(Modules.show(fd.name), fd.tparams, fd.bounds, List(r.targs(1)))

            val sym = instantiateFunc(fd, List(r.targs(1)))

            funcsUsed += sym
            sym
          }

          params.map(_._2) match
            case Nil                      => Some(TEntry(key, None, reporter, fallible))
            // Written either way: the arguments are the platform's and a program that only reads
            // them may say so, which costs the entry point nothing since the two views are one
            // layout and `args_of` yields the one that may stand in for either.
            case Type.Slice(Type.Str, _) :: Nil =>
              val argsFn = Library.key("args_of")

              funcsUsed += argsFn
              Some(TEntry(key, Some(argsFn), reporter, fallible))
            case ts =>
              err(s"'main' takes either nothing or one '[]string' of the program's arguments, " +
                s"not (${ts.map(show).mkString(", ")})")
        }
  }

  /** Whether a named type was instantiated at something that is not a type — a parameter standing
   * in for itself, or a type built out of one.
   */
  private def abstracted(t: Type.Named): Boolean = t.targs.exists(Type.mentionsAbstract)
}
