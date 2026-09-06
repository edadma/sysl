package sh.sysl

/** Typed statements, and the declarations a compiled module is made of.
 *
 * Split from `tast.scala`, which holds `TExpr`, for the reason `sealed` gives: a hierarchy lives in
 * one file, so the hierarchies live in three. `astStmts.scala` divides the untyped tree the same
 * way, and says why a declaration is a statement there; here the two are genuinely separate — by the
 * time a tree is typed, what a module holds has been decided.
 */

sealed trait TStmt

/** A binding and the slot it needs.
 *
 * `align` is `@align(n)` folded to the boundary it named, and absent where nothing asked — LLVM's
 * own choice is then the natural alignment, which is what a slot that asked for nothing wants. It is
 * carried here rather than looked up the way a struct's is (`Emitter.alignSuffix`) because that
 * lookup is by *emitted type name*, and a binding's storage has no name of its own.
 */
case class TVarDecl(name: String, ty: Type, init: TExpr, align: Option[Int] = None) extends TStmt
case class TExprStmt(expr: TExpr)                         extends TStmt

/** A loop's `invariant`, at the head of its body (`reference/verification.md § invariant and
 * variant on a loop`) — a condition that traps on false, which is what every other clause in `16`
 * already is. It carries no machinery of its own for that reason.
 */
case class TInvariant(cond: TExpr, msg: Option[String]) extends TStmt

/** A loop's `variant`, at the head of its body (`reference/verification.md § invariant and variant
 * on a loop`): the measure is evaluated, compared against the previous iteration's, and stored.
 *
 * `slot` names the pair of allocas the enclosing `TCheckedLoop` set up — `%<slot>.prev` holding the
 * last value and `%<slot>.armed` saying whether there has been one. The armed flag is what makes the
 * first iteration pass with nothing to compare against, and it is stored **at loop entry** rather
 * than in the function's prologue, which is the whole reason this statement cannot stand alone: a
 * loop inside another loop is entered many times, and a flag armed once per call would compare the
 * second entry's first measure against the first entry's last.
 */
case class TVariantCheck(slot: String, varTy: Type, expr: TExpr) extends TStmt

/** `ref name = place` (`reference/memory.md § ref — a name for a place`) — a name bound to the
 * storage `place` found, rather than to a copy of what was in it.
 *
 * It declares **no slot**. Where a `TVarDecl` allocates storage and stores into it, this binds the
 * name's address to the address the place already has, so the walk that reaches an element is made
 * once and every later use is the load or store it would have been anyway. That is also why nothing
 * releases it at scope end: the ref took no count, and the storage belongs to whatever the place was
 * rooted at.
 *
 * The place is kept in the node as well as consumed by codegen because the analyzer's own record of
 * it (`Scoping.refPlaces`) does not survive into the tree, and a later pass reading this statement
 * should see the same place the binding was made from.
 */
case class TRefDecl(name: String, ty: Type, place: TExpr) extends TStmt

/** One write of a multi-assignment: the place, the operator that was written, the value, the trait
 * method a compound operator lowers to when it is not an instruction (`reference/expressions.md §
 * Operator dispatch`), and the `invariant` re-check the receiver needs once the write lands (`05`).
 *
 * The check is carried here rather than wrapped around a store node, as `TRecheck` wraps one,
 * because these writes are not expressions and there is nothing for a node to wrap.
 *
 * `constraint` is the compound arm's counterpart of `TUpdate.check` — a plain arm's value carries
 * its own check, having been analyzed against the place's type, while a compound arm computes one
 * here and so is checked here (`reference/errors.md § Where a constraint is checked`).
 */
case class TWrite(place: TExpr, op: String, value: TExpr, dispatch: Option[TDispatch],
                  check: List[(TExpr, Type.Struct, String)],
                  constraint: Option[Type.Constrained] = None)

/** `a, b = b, a` — several places written from several values (`reference/expressions.md § Several places at once`).
 *
 * The order of events is the whole content of the form, and it is phases rather than one write at a
 * time. Every place's own subexpressions are computed first, and once, so an index that calls
 * something calls it a single time. Then everything the statement *reads* is read — what a compound
 * arm finds in its place, and then the whole right side — which is what makes a swap a swap instead
 * of two statements that leave both variables holding the same thing, and what makes every operand
 * of every arm see the values the statement started with. Only then does anything land.
 */
case class TMultiAssign(writes: List[TWrite]) extends TStmt

case class TReturn(value: Option[TExpr])                  extends TStmt

/** `become f(…)` — the call is emitted with LLVM's `musttail` and the `ret` that must follow it
 * (`reference/declarations.md § become — a call that replaces the frame`).
 *
 * The whole of what the analyzer settled is behind it: the callee's prototype matches this
 * function's, no parameter carries a reference count, nothing is deferred, and there is no
 * postcondition to check on a frame that will not be there. So codegen has one decision left, which
 * is the order — arguments, then the frame's own releases, then the jump.
 */
case class TBecome(call: TExpr)                          extends TStmt

/** `break [expr]` and `continue`. `break` carries the loop's value when the loop yields one.
 * `depth` names the target loop by its distance out from the innermost — `0` is the nearest,
 * a larger number a loop reached through a `'label` — and indexes the codegen loop stack directly.
 */
case class TBreak(value: Option[TExpr], depth: Int) extends TStmt
case class TContinue(depth: Int)                    extends TStmt

/** `defer stmt` — the statements to run on the way out of the block this sits in
 * (`reference/memory.md § Where defer sits`).
 *
 * It is a list because one written statement can analyze to several, the way a binding that names
 * more than one thing does. Reaching this node emits nothing at the point it stands: it hands the
 * statements to the enclosing scope, which emits them at each edge that leaves it. So a `defer`
 * control never reaches schedules nothing, and one in a loop body schedules for that iteration.
 */
case class TDefer(stmts: List[TStmt]) extends TStmt

/** The one assembly arm that answered for the processor being built for
 * (`reference/inline-assembly.md`).
 *
 * The other arms are gone by the time this exists — an architecture is chosen once, in the
 * analyzer, so nothing downstream carries a branch that was never going to be taken. The
 * instructions are text nothing here reads; the operands are the whole of what the compiler knows
 * about the block, and they are what the constraint string is built from.
 */
case class TAsm(lines: List[String], operands: List[TAsmOperand], clobbers: List[String]) extends TStmt

/** An operand bound to the local it names.
 *
 * `name` is what the template says, and `slot` is the local's unique name — the storage an `in` is
 * loaded from and an `out` is stored to. Both are kept because they are two different jobs: the
 * template is the text a person wrote, and the slot is where the value lives after a scope has
 * renamed it. `reg` is the machine register the operand must occupy, or `None` for any
 * general-purpose one.
 */
case class TAsmOperand(dir: AsmDir, name: String, slot: String, ty: Type, reg: Option[String])

/** A user function. Parameters carry their unique names (the codegen allocates a slot for
 * each so the body can read and mutate them uniformly). `requires`/`ensures` are the
 * design-by-contract clauses: each precondition is checked on entry, each postcondition before
 * every return, with a `TResult` in an `ensure` standing for the returned value.
 *
 * `internal` says every caller of this function is in the module that defines it, so its symbol
 * needs no external linkage (`reference/modules.md § Visibility`). It is set for a declaration
 * whose reach is the file that wrote it, and it is the only thing the emitted linkage depends on.
 */
case class TFunc(
    name: String,
    params: List[(String, Type)],
    retTy: Type,
    body: TBlock,
    variadic: Boolean = false,
    requires: List[(TExpr, Option[String])] = Nil,
    ensures: List[(TExpr, Option[String])] = Nil,
    olds: List[TExpr] = Nil,
    internal: Boolean = false,
    conv: Option[CallConv] = None,
    /** `@tailrec` was written above it: an assertion that its self-call is the last thing it does,
     * and a demand to be told at the compile rather than at the stack overflow when an edit stops
     * that being true (`reference/declarations.md § Tail calls`). The jump itself does not wait on
     * this — it applies wherever it applies — so what the flag reaches is `TailCalls.check` and
     * nothing in codegen.
     */
    tailrec: Boolean = false,
    /** The `variant` its contract block declared: an integer measure over the **parameters** that
     * must strictly decrease at every direct recursive call (`reference/verification.md § variant
     * on a function`).
     *
     * That it reads only parameters is what makes the check local, and it is what this field is
     * enough for on its own. At a self-call the emitter has both the current parameter values and
     * the argument values about to replace them, so it evaluates this expression twice — once as it
     * stands, once with the arguments stored into the parameters' own slots — and needs neither a
     * substitution pass nor a hidden argument travelling with the call.
     */
    variant: Option[TExpr] = None,
    /** `@pure` was written above it: an assertion that a caller can observe nothing about this call
     * but its result (`reference/verification.md § @pure`).
     *
     * It is checked rather than believed, and what it excludes is written out in `Purity`. It is not
     * inferred: a function is pure because it says so, which is what keeps an edit to a leaf from
     * breaking a caller three levels up with no annotation anywhere naming the promise it broke.
     */
    pure: Boolean = false,
    /** `@ghost` was written above it: the function exists for the specification and is erased
     * before codegen (`reference/verification.md § @ghost — what costs nothing to say`).
     *
     * Its body is ordinary code and may read real state freely — that is the whole point of an
     * `is_sorted` — and what the mark buys is the pair of rules that make erasing it sound: nothing
     * executable may call it, and a clause that does is a clause that does not run. So a loop
     * invariant costing O(n) inside an O(n) loop costs nothing at all, without a switch that would
     * give one program two meanings.
     */
    ghost: Boolean = false,
    /** `@reads(…)` and `@writes(…)`: the module-level storage this function may touch
     * (`reference/verification.md § @reads and @writes — what a call may touch`).
      *
      * **`None` and `Some(Nil)` are different claims and the distinction carries the whole design.**
      * A function with no annotation has effects nobody has written down — it may call and be called
      * by anything, exactly as before frames existed — while `@reads()` is the positive assertion
      * that it touches no module storage at all. That is what lets adoption run from the leaves up:
      * the first leaf to gain a frame forces its annotated callers to gain one, and everything
      * unannotated is undisturbed.
      *
      * The names are resolved symbols rather than what was written, so a frame means the same thing
      * from inside the module and from outside it.
      *
      * `writes` is included in the readable set rather than being disjoint from it — `count += 1`
      * is a read and a write of one variable, and a form that common should not have to say so
      * twice. SPARK's `Output`/`In_Out` split, which this collapses, is `reference/verification.md
      * § @reads and @writes — what a call may touch`.
      */
    reads: Option[Set[String]] = None,
    writes: Option[Set[String]] = None,
    /** `@export` — the symbol this definition is C-callable under, mangling suppressed
     * (`reference/ffi.md § @export`).
      *
      * It carries the **resolved** symbol rather than what was written, so `@export` and
      * `@export("mylib_parse")` are one thing by the time anything downstream looks: the first
      * resolves to the function's declared name, and neither codegen nor the header writer has a
      * second case to get wrong.
      *
      * Like an interrupt handler, an exported function is a **root** for reachability — nothing in
      * the program calls it, and the whole point is that something outside will (`Reachability`).
      */
    exported: Option[String] = None,
    /** `@section("…")` — the linker section this definition is placed in (`reference/attributes.md
     * § @section("...")`).
      *
      * It makes the function a **root** for the third version of the export's reason: what finds a
      * definition by its placement is a linker script, and no call in this program need name it. The
      * symbol is written into `llvm.used` for the same reason one step further down, where the
      * optimizer would otherwise drop a definition nothing calls.
      */
    section: Option[String] = None,
) extends Positioned

/** A function the linker supplies, which the module declares rather than defines. Only the ones
 * the program actually calls reach here, so an `extern` the library offers and nobody uses costs
 * the output nothing.
 *
 * `name` is what the program calls it by and `symbol` is what the linker resolves; they differ only
 * where the declaration gave a link name. Two declarations may share one symbol — the library's
 * `snprintf` and a program's own — so the module declares each *symbol* once.
 */
case class TExtern(name: String, symbol: String, params: List[Type], retTy: Type,
                   variadic: Boolean = false)

/** Storage the linker supplies, which the module declares rather than lays down (`reference/ffi.md § An extern also declares a variable`).
 *
 * The same accounting the externs above get: only the ones something reads or writes reach here, and
 * two declarations may share one symbol, so the module declares each *symbol* once. What it carries
 * is the symbol and the type, because that is the whole of an `external global` line — there is no
 * initializer, and nothing about the storage for this module to decide.
 */
case class TExternVar(symbol: String, ty: Type)

/** One method table — the constant a trait object's first word points at, holding one function
 * pointer per method the trait declares, in declaration order.
 *
 * There is one table per (trait, implementing type, **memory mode**), and the mode is why `boxed`
 * is here: a `*Trait`'s data word is the value's own address, while a `&Trait`'s is the address of
 * the reference-counted box the value sits inside, so the two reach the same implementation through
 * different arithmetic.
 */
case class TVtable(name: String, traitName: String, forType: Type, boxed: Boolean, slots: List[TVSlot],
                   /** The implementing type's compile-time identity, which the table carries as its
                     * first word so that `o::Id` on an erased value can be read (`02`, `TypeId`).
                     *
                     * It is a property of `forType` rather than of the table, and there are two
                     * tables per type where both memory modes are erased — so both carry the same
                     * number, which is what makes `*Shape` and `&Shape` over one type compare equal.
                     */
                   typeId: BigInt)

/** One slot of a method table: the function it ends at, how that function wants its receiver, and
 * the signature a call site sees. Between the data word and the receiver the function declared
 * there may be a header to step over and a value to load, which is what the mode decides.
 */
case class TVSlot(target: String, recv: RecvMode, params: List[Type], retTy: Type,
                  /** Which of the target's parameters the **trait** declared `@borrows`, by the
                    * implementation's own parameter positions — so the receiver is 0 and the first
                    * written parameter is 1 — against the name the trait gave each one.
                    *
                    * The name is carried because it is what a refusal has to say: an implementation
                    * is told which of *its* parameters it kept, and the reader finds that word in
                    * the trait rather than counting places.
                    *
                    * It is on the slot rather than looked up per call because that is where the two
                    * questions meet: the escape analysis checks each implementation against it, and
                    * a call site reads it to know which arguments it may pass a frame-backed slice
                    * for. Every table for one trait carries the same answer, since the promise is
                    * the trait's.
                    */
                  borrows: Map[Int, String] = Map.empty)

/** One module-level `val`: read-only storage laid down whole, under the key its module gives it.
 *
 * `computed` says which of the two ways it is filled. A constant tree is written straight into the
 * object file and nothing runs; anything else is code, evaluated once before the program's own
 * statements and stored, in an order the initializers' dependencies settle (`reference/modules.md § val — a thing`).
 */
case class TVal(
    symbol: String,
    ty: Type,
    init: Option[TExpr],
    computed: Boolean,
    writable: Boolean = false,
    /** `@align(n)` folded to the boundary it named. Absent means the natural alignment, which is
      * what LLVM gives a global that asked for nothing.
      */
    align: Option[Int] = None,
    /** `@section("…")` — the linker section this storage is placed in (`reference/attributes.md §
      * @section("...")`). Present also means the symbol is kept: nothing in the program reads a
      * table the linker script gathers, so the object joins `llvm.used` rather than being dropped
      * by the optimizer that finds no reader.
      */
    section: Option[String] = None,
)

/** The `main` a program declared, which runs after its top-level statements (`reference/modules.md § Where a program starts`).
 *
 * `func` is the key the function is filed under, which is what makes it reachable; `argsFn` names the
 * library function that turns the platform's `argc`/`argv` into the `[]string` it wants, and is
 * absent for a `main` that takes no parameters — so a program that does not ask for its arguments
 * carries none of the conversion.
 */
/** The program's entry point: which function it is, how its arguments are made, and -- where it
  * results in a `Result` -- the instantiated `sysl.main_result` that reports the failure and chooses
  * the exit status, together with the type the call answers with.
  */
case class TEntry(func: String, argsFn: Option[String],
                  resultFn: Option[String] = None, resultTy: Option[Type] = None)

/** One `@test` function, as the runner needs it (`reference/attributes.md § The runner`).
 *
 * `func` is the key the function is filed under, which is what makes it reachable and what the
 * dispatcher matches an argument against. Everything else is for the report: what to call the test,
 * whether returning is the outcome it was after, and where to point a reader whose test failed.
 *
 * The position is carried here because it is the *attribute's*, not the function's, and it is the
 * only place a reader can be sent that is certainly about the test rather than about the code under
 * it. A test that failed has no diagnostic of its own — it has an exit status — so this stands in
 * for one.
 */
case class TTest(
    func: String,
    display: String,
    shouldTrap: Boolean,
    expected: Option[String],
    file: String,
    line: Int,
    /** The hooks the test's own module declared, resolved onto every test in it
      * (`reference/attributes.md § The hooks a module may write`).
      *
      * Carried per test rather than per program because the runner is handed a list of tests and
      * nothing else — a cached suite comes back from a sidecar with no program behind it at all —
      * and because what a test needs to know is which hooks bracket *it*.
      */
    hooks: THooks = THooks(),
)

/** One hook function, as the runner needs it: what to call, and where to point a reader when the
 * call does not come back.
 *
 * A hook has no `display` of its own — a report shows it by the name it was declared under, which is
 * what a reader greps for. The file and line are the attribute's, for `TTest`'s reason.
 */
case class THook(kind: HookKind, func: String, file: String, line: Int)

/** The four hooks a module may declare, each present or not.
 *
 * A record of four options rather than a list, because the four are not interchangeable: the runner
 * asks for one of them by name at four different moments, and a list would make every one of those
 * a search that could come back with the wrong kind.
 */
case class THooks(
    setup: Option[THook] = None,
    teardown: Option[THook] = None,
    setupAll: Option[THook] = None,
    teardownAll: Option[THook] = None,
) {
  def all: List[THook] = List(setup, teardown, setupAll, teardownAll).flatten
}

object THooks {

  /** The four, picked out of everything one module declared. */
  def of(hooks: List[THook]): THooks =
    THooks(hooks.find(_.kind == HookKind.Setup), hooks.find(_.kind == HookKind.Teardown),
           hooks.find(_.kind == HookKind.SetupAll), hooks.find(_.kind == HookKind.TeardownAll))
}

/** A whole program: hoisted struct, enum, and function declarations, the method tables its trait
 * objects dispatch through, the externs it calls, the module-level `val`s it reads, plus the
 * top-level statements that make up `main`. Only data enums appear in `enums` — a simple enum
 * lowers to `i32` and needs no type declaration.
 */
case class TProgram(
    structs: List[Type.Struct],
    enums: List[Type.Enum],
    vtables: List[TVtable],
    externs: List[TExtern],
    vals: List[TVal],
    funcs: List[TFunc],
    main: List[TStmt],
    entry: Option[TEntry] = None,
    /** Functions a **library** already compiled, which this module calls but must not define
     * (`LibraryArtifact`). They are declared rather than emitted, and the object file the library
     * shipped supplies the body at link time.
     *
     * They are named here rather than turned into `TExtern`s because an `extern` is declared under
     * the **C** convention, and these are sysl functions: the declaration has to be built from the
     * same signature the definition would have had, or the caller passes its arguments the wrong
     * way and the mistake is a corrupt run rather than a link error.
     */
    precompiled: Set[String] = Set.empty,
    /** Whether this module carries the program's entry point. A library does not: it is lowered on
     * its own to be linked into something else, and a `main` of its own would collide with the one
     * belonging to whatever links it.
     */
    entryPoint: Boolean = true,
    /** Whether the artifact this compilation produces is one a **C project** links
     * (`reference/ffi.md § @export`) — `sysl build-c`, and `emit-header` beside it.
     *
     * It is a second question rather than `entryPoint` read backwards, because **two** kinds of
     * build have no entry point and they answer differently about who fills the module storage
     * (`reference/modules.md § val — a thing`):
     *
     *   - a `.syslib` is linked by a sysl **program**, which has an entry point and lays the `val`s
     *     down at the top of it — so `Compiler.compileLibrary` defers every function reaching one to
     *     that program rather than emitting it here, and a constructor emitted alongside would fill
     *     a copy of the storage nothing reads;
     *   - a **C** archive is linked by a project that supplies its own `main`, so nothing else is
     *     ever going to fill it and `Codegen.genModuleInit` registers a constructor that does.
     *
     * Reading `!entryPoint` as the second of those was card `0263`'s first shape and would have put
     * a constructor into every `.syslib`.
     */
    cArtifact: Boolean = false,
    /** The modules that declared `no alloc` (`reference/modules.md § Capabilities are a module
     * property`). The analyzer has already held each of them to making no heap storage of its own;
     * what this carries the answer forward for is the one allocation no expression in the tree
     * spells — the **promotion** of a local array whose slice outlives its frame, which escape
     * analysis decides after the walk has finished (`05`).
     */
    noAllocModules: Set[String] = Set.empty,
    /** The same, for what each module's **tests** may do
     * (`reference/modules.md § A @tests file states its own capabilities`).
     *
     * It is a second set rather than a flag because a `@tests` file may take the allocator back — a
     * module can ship allocating nowhere and still be tested by rendering what it produced. A
     * module with no `@tests` file is in this set exactly when it is in the one above, so the two
     * part company only where a file said they should.
     */
    noAllocTestModules: Set[String] = Set.empty,
    /** The module whose terms the statements in `main` were written in — the file that carries the
     * program's entry point (`reference/modules.md § Where a program starts`). Every other body says which module it belongs to in its own
     * key; these have no key, so the answer is carried here.
     */
    mainModule: String = Modules.root,
    /** The `@test` functions the sources declared, in the order they were written (`reference/attributes.md § @test — a function with a caller nothing else has`).
     *
     * They are carried on the program rather than looked up from `funcs` because what a test *is* —
     * its reported name, what it expects — lives in the attribute, and the typed function is the
     * ordinary function it would have been without one. A compilation that is not a test build
     * drops both this and the functions it names: `Tests.strip`.
     */
    tests: List[TTest] = Nil,
    /** Every hook the sources declared, across every module
      * (`reference/attributes.md § The hooks a module may write`).
      *
      * `tests` carries the same functions again, grouped onto the tests they bracket; this is the
      * flat list, and it is what the questions asked of the *tree* need. A module may declare a hook
      * and no tests, so a walk that read the hooks off `tests` would leave those behind — which is a
      * hook surviving into a build that runs no tests, and it is exactly what `Tests.strip` is for.
      */
    hooks: List[THook] = Nil,
    /** The `extern` variables the program reads or writes (`reference/ffi.md § An extern also declares a variable`). Declared beside the `val`s
     * rather than up with the `extern` functions, because a named aggregate type has to be defined
     * before anything names it — the same ordering the precompiled declarations need.
     */
    externVars: List[TExternVar] = Nil,
    /** Everything declared in a file that said `@tests` — the scaffolding a module's tests are
     * written against (`reference/attributes.md § @tests — a file of scaffolding`).
     *
     * Keys rather than declarations, because what is carried here is asked of several lists at once:
     * a test file may declare functions, storage and `extern`s, and `Tests.strip` drops all three by
     * the same question. The names are the module-qualified ones every other table uses.
     *
     * A test build keeps them and every other build drops them, which is the whole of what the
     * header said. The analyzer has already held them to the other half — that nothing outside a
     * test names one — so by the time a tree reaches here, dropping them can leave nothing dangling.
     */
    testOnly: Set[String] = Set.empty,
    /** The **destructor** each type that has one is reached by, keyed by the type as `Type.mangle`
     * spells it — which is exactly how the release hook names itself (`reference/memory.md § A
     * destructor`).
     *
     * It is carried rather than looked up because the two ends are in different phases and neither
     * can ask the other. The analyzer knows which types an `impl Drop` covered and what the lowered
     * member is called; the emitter knows only a payload type, at a release site with no name in it.
     * A map from one to the other is the whole of what has to cross.
     */
    destructors: Map[String, String] = Map.empty,
    /** What the analyzer wants to **say** about this program without refusing it — currently the
     * one rule in `DropReturnCheck` (card `0372`).
     *
     * It travels on the tree for the same reason `destructors` does: the two ends are in different
     * phases and neither can ask the other. A warning is found while declarations are being walked
     * and is printed by the driver, which has no analyzer to ask by then — and `Analyzer.analyze`
     * answers `Either[List[Diagnostic], TProgram]`, whose left is *failure*. A warning is not
     * failure, so it cannot go there, and a third channel through six call sites would be a wider
     * change than one field on the value they already hand back.
     */
    warnings: List[Diagnostic] = Nil,
    /** Which module refers to which, as name resolution settled it (`reference/modules.md § The
     * module graph is acyclic`) — the same edges `ModuleGraph` holds to being acyclic, kept for the
     * one question that is asked after the analyzer has finished.
     *
     * That question is `Reachability.prune`'s: whether an `@export` in a module a **dependency**
     * supplied is a root. Nothing in the typed tree answers it, because the whole point of an export
     * is that no body names one — so what decides it is whether the program refers to that module at
     * all, which is exactly an edge here. An empty map means nothing was recorded rather than that
     * nothing was referenced, which is why the caller says which modules are its own instead of the
     * walk inferring it from a graph that may simply not have been built.
     */
    moduleDeps: Map[String, Set[String]] = Map.empty,
)

