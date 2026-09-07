package sh.sysl

import scala.collection.mutable

/** The shared substrate of the analyzer, mixed into the feature traits (`TypeResolution`,
 * `Literals`, `CallAnalysis`, `PatternAnalysis`) and the `Analyzer` class itself.
 *
 * Three layers are underneath it, each holding one concern of what "the analyzer's state" means:
 * `Reporting`, which is how a mistake is raised and recovered from; `DeclTables`, which is what the
 * program is made of; and `Scoping`, which is what a name written at a given place means. This one
 * adds the state of the **body being analyzed right now** — its result type, its type substitution,
 * the loops it is inside, the nested functions and captures it can see — all of which is put back
 * at every function boundary, and none of which outlives the body it belongs to.
 *
 * It also declares the recursive entry points that live in the `Analyzer` class but are called from
 * every trait, as abstract hooks — exactly as `Emitter` declares `genExpr` for the codegen traits.
 *
 * What none of the four holds is the reading of the tables. Asking what members a type has, whether
 * it satisfies a bound and through which implementation, and what to say when it does not, is
 * `TraitLookup` — which extends this and which everything else reaches through.
 */
trait AnalyzerBase extends Scoping {

  /** The two operands of a binary form, with a scalar beside a vector splatted into one.
   *
   * This is where `a * 2.0` and `a < 0.0` become lane-wise: the scalar is the same value in every
   * lane, which is what the reader means and what every SIMD API in any language provides. It runs
   * after the literal has been read at the lane type, so what arrives here is either already the
   * lane type or a mismatch worth reporting in the operator's own message rather than as a failed
   * broadcast.
   *
   * Two vectors of different widths are left alone: `arithType` reports them as the mismatched
   * types they are, which names both widths, where a splat here could only fail silently.
   */
  protected def balanceLanes(l: TExpr, r: TExpr): (TExpr, TExpr) = (Type.repr(l.ty), Type.repr(r.ty)) match
    case (v: Type.Vector, s) if !s.isInstanceOf[Type.Vector] && s == Type.repr(v.elem) =>
      (l, TSplat(r, v).setPos(r.pos))
    case (s, v: Type.Vector) if !s.isInstanceOf[Type.Vector] && s == Type.repr(v.elem) =>
      (TSplat(l, v).setPos(l.pos), r)
    case _ => (l, r)

  /** Inside a closure's body, the captured names — keyed by the unique name the scope gave each —
   * paired with the field read that reaches one (`reference/expressions.md § Closures`).
   *
   * A capture is declared in the closure's scope like any other name, so shadowing and assignment
   * need no rule of their own; what this adds is where the storage is. Reading a name consults it
   * after the scope has answered, which is why an inner binding of the same name still wins.
   */
  protected var capturedFields: Map[String, TExpr] = Map.empty

  /** The nested functions in scope, by the name a program calls one by
   * (`reference/declarations.md`).
   *
   * A block's nested functions are lowered as a group, so what is in here is every one of them from
   * the moment the first is written until the block ends — which is the "names are hoisted per
   * block" half of §5a. It follows the block, so it is put aside and given back at a scope boundary
   * the way the scope itself is.
   */
  protected var nestedFuncs: Map[String, Nested] = Map.empty

  /** The nested functions this block has still to lower, which is what lets the group be found from
   * the first of them and what lets a call written above them all say so.
   */
  protected var pendingNested: List[FuncDecl] = Nil

  /** The block's own bindings the pending group reads and that are **not bound yet** (`0224`).
   *
   * A nested function may read anything its block binds, wherever in the block it is written — so
   * the group cannot be lowered at the first of them, because the environment it builds holds the
   * *address* of every capture and a slot whose declaration has not run has no name to point at
   * yet. The group therefore waits here until the last of these is bound, and is lowered directly
   * after it.
   *
   * **Empty is the ordinary case** — a group reading only what is already above it lowers where it
   * always did, at the first of its functions.
   */
  protected var pendingNeeds: Set[String] = Set.empty

  /** Whether the walk has reached the group's first function and is waiting on `pendingNeeds`.
   *
   * It is what keeps the group from being lowered *above* where it is written: a binding that
   * completes the set before any of the functions has been reached leaves the group pending, to be
   * lowered at the first of them as usual.
   */
  protected var awaitingNeeds: Boolean = false

  /** Nested functions of a body this one is written *inside*, kept so that naming one is refused for
   * the reason it is refused rather than as an undefined name.
   *
   * A body nested in another reaches its own group and its own captures, and no further: a closure
   * may outlive the frame around it, and a nested function's environment is that frame.
   */
  protected var outerNested: Set[String] = Set.empty

  /** Every name the block being analyzed binds, reached or not, so that a use written above the
   * declaration is told which of the two mistakes it is.
   *
   * **Each name carries where it was bound**, because there are two mistakes rather than one and
   * the position is what tells them apart: a use genuinely above the binding, and a use inside a
   * nested function whose group was formed above it. The second reads as a contradiction otherwise
   * — a name declared six hundred lines higher up reported as "declared below this" (card `0221`).
   */
  protected var blockDeclares: Map[String, Option[Pos]] = Map.empty

  /** A name the block binds that is not in scope where it is used — **two** mistakes wearing one
   * shape, told apart by where the binding is (card `0221`).
   *
   * The ordinary one is a use written **above** the declaration, and the rule is the one a reader
   * would state: a name is in scope from where it is bound onward.
   *
   * The other reads as a contradiction and is not one. A block's nested functions share **one**
   * environment, built where the first of them is written (`Closures.lowerNestedGroup`), so what
   * any of them may capture is what the block had bound by that point — and a use inside one of
   * them is refused however far below the declaration it is written. Reporting that as "declared
   * below this" is simply false, and it was: a module `val` bound at line 155 was reported that way
   * against a use at line 827, which cost a session five failed reductions before the position was
   * compared rather than assumed.
   *
   * **The comparison is guarded on both ends.** A synthesized node has no position and a body
   * nested through a macro-like path could be reading a different file, so anything that cannot be
   * ordered falls to the first message — which is the one that was always given and is right
   * whenever the second cannot be established.
   */
  protected def notYetBound(name: String): Nothing = {
    val below =
      for
        d <- blockDeclares(name)
        u <- currentPos
        if d.source == u.source
      yield u.line > d.line || (u.line == d.line && u.col > d.col)

    if below.contains(true) then
      err(s"'$name' is bound after the nested functions of this block begin, so it is not in this " +
        s"one's environment — they share a single environment, formed where the first of them is " +
        s"written, and what it holds is what the block had bound by that point. Bind '$name' above " +
        s"them, or make it module storage with 'static'")
    else
      err(s"'$name' is declared below this, and a name is in scope from where it is bound onward")
  }

  // Per-function state, reset at each function boundary.
  protected var retTy: Type               = Type.Unit
  protected var tsubst: Map[String, Type] = Map.empty

  /** What the body's type parameters were **bounded** by, alongside what they were substituted with.
   *
   * `tsubst` says a parameter has become `int` at this instantiation; this says the signature asked
   * it for `Zero`. Both are needed to reach a member: the substitution finds the type's table, and
   * the bound says which of the members that table holds under one name was promised — a question
   * the instantiation would otherwise have no way to answer, since two traits may declare one name
   * for one type and neither the call's arguments nor its scope is the thing that settled it.
   */
  protected var tbounds: Map[String, List[BoundRef]] = Map.empty

  /** Which type parameter each **parameter written as a bare one** was written as. `tbounds` says
   * what `T` was asked for; this says which of the body's names hold a `T`.
   *
   * Both halves are needed because the two ways of reaching a member arrive with different things.
   * An associated function is written through the parameter's own name — `T.zero()` still has the
   * `T` in it — but a **method** is written through a value, and by the time an instantiation
   * analyzes `x.id()` the receiver carries the type the substitution produced. Nothing in it
   * remembers being written as `T`, so without this the body would be refused for an ambiguity its
   * own signature settled.
   */
  protected var pbounds: Map[String, String] = Map.empty

  /** The body's own names standing for a **by-name** parameter (`reference/declarations.md §
   * Default parameters and named arguments`).
   *
   * They are the **uniqued** names the scope hands back rather than what was written, which is what
   * makes shadowing need no rule of its own: a local declared over a by-name parameter is a
   * different name here, so reading it stays an ordinary read.
   */
  protected var byNameLocals: Set[String] = Set.empty

  /** The traits a type parameter was bounded by, as keys — what a use inside the body may reach
   * through it whether or not the file also imported them, since the signature has named them.
   */
  protected def boundTraits(written: String): Set[String] =
    tbounds.getOrElse(written, Nil).flatMap(b => traitKey(b.name)).toSet

  /** Whether the function being analyzed declared its result as a **list**
   * (`reference/declarations.md § Several results`) rather than as one type. `retTy` is the tuple
   * its parts lay out as either way; this is what says whether the body writes `a, b` or `(a, b)`,
   * and whether a call yielding a list may stand in its result.
   */
  protected var retIsList: Boolean = false

  /** Set for exactly one expression by `analyzeMulti`, and consumed by the funnel the moment that
   * expression is analyzed — so a call yielding several results is allowed where the form asked for
   * one, and nowhere inside it.
   */
  protected var multiOk: Boolean = false

  /** Whether the function being analyzed declared a `...`, which is what `va_start` needs: there is
   * no tail to start walking in a function that does not have one. C's rule exactly.
   */
  protected var variadicFn: Boolean = false

  /** The return type while analyzing an `ensure` postcondition, so `result` resolves to it.
   * `None` everywhere else, which is what makes `result` an ordinary identifier outside a
   * postcondition.
   */
  protected var ensureResultTy: Option[Type] = None

  /** The entry-time snapshots an `ensure` has asked for so far, present only while a postcondition
   * is being analyzed. Each `old(e)` appends its typed expression and reads back its position, so
   * codegen can capture them all at entry. `None` outside an `ensure`, which is what keeps `old`
   * an ordinary name everywhere else.
   */
  protected var oldBuf: Option[mutable.ListBuffer[TExpr]] = None

  /** The enclosing loops, innermost first, so a `break`/`continue` finds the one it leaves and a
   * `break value` records its type against that loop's result. `expected` is the type the loop's
   * context wants, pushed down so a `break`/`else` value boxes to `&T` on its own. `label` is the
   * loop's `'name`, if it has one, which a labeled `break`/`continue` resolves against.
   */
  protected class LoopCtx(val expected: Option[Type], val label: Option[String]):
    val breakTys = mutable.ListBuffer.empty[Type]

    /** The slot name and integer type of this loop's `variant`, where it declared one
     * (`reference/verification.md § invariant and variant on a loop`). Filled in once the body has
     * been analyzed, and read by whichever loop form built the context so it can wrap itself in a
     * `TCheckedLoop`.
     */
    var variant: Option[(String, Type)] = None
  protected var loops: List[LoopCtx] = Nil

  /** Whether the statement being analyzed sits directly in the body of a `for const`
   * (`reference/generics.md § A parameter may stand for a list of types`).
   *
   * The unrolled loop hides the enclosing loops while its copies are analyzed, so `loops` is empty
   * there whatever the loop is written inside — and that is the whole mechanism, since a `break`
   * with no loop to find is already an error. This flag only decides *which* sentence it gets: the
   * one about there being no loop, or the one about the loop the reader can see being unrolled.
   */
  protected var inConstFor: Boolean = false

  /** A loop body's `variant` on its way out to the context above, set after the body's own
   * statements are analyzed so that a nested loop has already taken and cleared its own.
   */
  protected var pendingVariant: Option[(String, Type)] = None

  /** Whether the body being analyzed is a slice shape block made real at the **read-only** view.
   *
   * The block was written for `[]T` and this instance's `self` is a `[]const T`, because that is
   * what the receiver was (`TraitLookup.viewedConst`). A write through it is refused like any other
   * write through a read-only view, and this is what lets the refusal say why the view is read-only
   * — which the author of the block never wrote and cannot see from the line it lands on.
   */
  protected var inConstSelf: Boolean = false

  /** What a refusal to write through a read-only view adds when the view is a shape block's own
   * `self` — empty everywhere else, so it may be appended without asking.
   *
   * Without it the message lands on a line whose author wrote `[]T` and reads as the compiler having
   * invented a `const` nobody typed. The one thing the reader cannot see from that line is the
   * receiver at the call, and the receiver is the whole explanation.
   */
  protected def constSelfNote: String =
    if !inConstSelf then ""
    else " — and this body is the block written for '[]T', made real at the read-only view because " +
      "that is what the receiver was. A member writing through 'self' is one only a '[]T' can be " +
      "given, so what has to change is the call on the '[]const T'"

  /** Numbers the variant slots so two loops in one function cannot share storage. */
  protected var variantSeq: Int = 0

  /** The name of the function whose body is being analyzed, as it was written — what `__FUNCTION__`
   * reports (`ReservedNames`).
   *
   * The **bare** name rather than the module-qualified key, because it is read by a program printing
   * a diagnostic about itself and a reader following one back to a file wants the name in that file.
   * It is also the name a generic's instantiations share: what a reader wrote is one function
   * however many times it was lowered, and reporting a mangled key would leak the lowering into the
   * program's own output.
   *
   * Empty outside any body — a module `val`'s initializer, an `extern`'s default — where there is no
   * function to name and `__FUNCTION__` says so rather than borrowing whichever was analyzed last.
   */
  protected var currentFunctionName: String = ""

  /** The **declaration name** of the body being analyzed, kept whole where `currentFunctionName` is
   * split — `Cell.count$set`, not `set`.
   *
   * The two differ for one reason and it is worth stating, because the difference is a trap rather
   * than a choice: `currentFunctionName` is `Modules.bare` of the name, and `Modules.bare` splits
   * at the **first** `$`, which is the module separator. A member filed under a name that itself
   * holds one — a setter, `count$set` (`reference/declarations.md § A property may be settable`) —
   * is therefore cut in the wrong place in a file with no module prefix to consume the first `$`,
   * and comes back as `set`.
   *
   * `currentFunctionName` stays as it is, because what it is for is `__FUNCTION__` and a name a
   * *reader* wrote never holds a `$`. What needs the whole thing is a question about which member
   * this body is, so that question asks here.
   */
  protected var currentMemberName: String = ""

  protected def resetFunction(): Unit = {
    // Cleared here rather than left to whoever sets it, because "outside any body" has to be a
    // state the analyzer can actually be in. Left uncleared it held the last name analyzed, so a
    // module `val`'s `__FUNCTION__` silently reported some unrelated function that a *previous
    // pass* had walked — the definition-time pass of `reference/generics.md § Bounds`, which runs
    // before any storage is laid down.
    currentFunctionName = ""
    currentMemberName = ""
    // Cleared for the same reason, and it matters for the same one: a body left over from the last
    // declaration walked would make a closure lowered outside any of them — in `main`, in a module
    // `val`'s initializer — inherit whether *that* was a test.
    inTestBody = false
    resetLocals()
    byNameLocals = Set.empty
    loops = Nil
    pendingVariant = None
    variantSeq = 0
    ensureResultTy = None
    oldBuf = None
    multiOk = false
    capturedFields = Map.empty
    nestedFuncs = Map.empty
    pendingNested = Nil
    pendingNeeds = Set.empty
    awaitingNeeds = false
    outerNested = Set.empty
    blockDeclares = Map.empty
    tbounds = Map.empty
    pbounds = Map.empty
  }

  /** Runs `body` and then restores every table the emitted program is built from.
   *
   * The definition-time pass of `reference/generics.md § Bounds` walks a generic body exactly as an
   * ordinary one is walked, so it registers instantiations exactly as an ordinary one does — a
   * `Box[T]`, a call to another generic function, a library renderer reached by a `print`. None of
   * those is a real instantiation: `T` is not a type anything can be laid out at, and nothing at
   * run time reaches them. Dropping what the pass registered is what keeps a diagnostics-only walk
   * from putting a type parameter into the emitted module.
   */
  protected def sandboxed[T](body: => T): T = {
    val saved = marked()

    try body
    finally
      rewind(saved)
      journal.leave()
  }

  /** Where a speculative region has to rewind to, taken as one value so that a caller may decide
   * *when* to put the tables back rather than only that it will.
   *
   * A mark is a position in a log rather than a copy of the tables: they record how to undo each write
   * while a region is open ([[Journal]]), so what has to be remembered here is only where in that
   * log the region started. The queue of instantiations still waiting for a body is the exception —
   * it is a work list that is drained as it fills rather than a table that grows with the program,
   * so copying it is a copy of what is in flight and not of what has been built.
   */
  private case class Mark(undo: Int, queued: List[(String, FuncDecl, Map[String, Type])])

  private def marked(): Mark = Mark(journal.enter(), pending.toList)

  private def rewind(saved: Mark): Unit = {
    journal.rewind(saved.undo)
    pending.clear();     pending ++= saved.queued
  }

  /** A question asked of the tables that is allowed to have no answer, with everything it registers
   * on the way dropped and everything it complains about left for the walk that follows.
   *
   * It sits here, beside `sandboxed`, because two quite different parts of the analyzer ask
   * speculative questions and neither is above the other: a method call asks whether a receiver has
   * a member of some name, and an **overloaded** call asks which of several declarations the
   * arguments fit (`reference/declarations.md § Overloading`). It was `MethodCalls`' private helper
   * until the second of those needed it from `CallCore`, which `MethodCalls` is built on top of.
   */
  protected def probe[T](body: => T): Option[T] =
    sandboxed {
      try Some(body)
      catch
        case AnalyzerError(_, _, _) => None
        case Poisoned()             => None
    }

  /** An analysis that is **allowed to fail**, and whose registrations are kept when it does not.
   *
   * This is `probe`'s other half, and the difference is the whole reason it exists: `probe` always
   * rewinds, which is right for a question about a type and wrong for anything that keeps the node.
   * A `TExpr` carried out of a `probe` refers to instantiations that were dropped on the way out —
   * a generic callee never registered, a vtable never built — so the emitted program names
   * something nothing emitted.
   *
   * Rewinding only on failure is what lets an argument be *tried* at its position for the cost of
   * one analysis rather than two. A failed attempt is left exactly as `probe` leaves one: nothing
   * registered, nothing said, since `err` throws rather than recording.
   */
  protected def attempt[T](body: => T): Option[T] = {
    val saved = marked()
    val said  = complaints

    def undo(): None.type = { rewind(saved); restoreComplaints(said); None }

    try
      val answer = body

      // **Coming back is not the same as succeeding**, which is the half a `catch` cannot see. A
      // region that failed inside a `recover` records its complaint and hands back a fallback, so
      // an attempt reading only the exception would keep a node built out of a mistake *and* leave
      // the mistake reported — a reading nobody kept, blamed on the reader.
      if complaints.length > said.length then undo() else Some(answer)
    catch
      case AnalyzerError(_, _, _) => undo()
      case Poisoned()             => undo()
    finally journal.leave()
  }

  /** Whether asking that question failed on a mistake **somebody has already been told about**,
   * rather than on its own account.
   *
   * `probe` folds the two together, because a speculative question only wants to know whether it has
   * an answer. Deciding what to *say* when it has none needs them apart: an `AnalyzerError` is this
   * question's own finding and is worth a message, while a `Poisoned` came from a value whose
   * deciding mistake was reported further up the file, and a second message about it points the
   * reader at the wrong line.
   */
  protected def alreadyReported(body: => Any): Boolean =
    sandboxed {
      try { body; false }
      catch
        case AnalyzerError(_, _, _) => false
        case Poisoned()             => true
    }

  // --- hooks provided by the Analyzer class --------------------------------------------
  //
  // These recursive entry points live in the class (statements, expressions, places) but are
  // called across the feature traits, so they are declared abstract here for the traits to see.

  protected def resolveBound(b: BoundRef, subst: Map[String, Type]): Type.Bound
  protected def resolveType(t: TypeRef, subst: Map[String, Type]): Type
  protected def selfBinding(t: Type): Map[String, Type]
  protected def substParams(t: Type, subst: Map[String, Type]): Type

  /** `T::Item` — the associated type of that name, read off the type that supplies it. Declared here
    * because the tables that answer it are `TraitLookup`'s and the walks that ask are the type
    * resolver's and the substitution's, which are not built on it.
    */
  protected def assocType(subject: Type, member: String): Type
  protected def assocTypeOpt(subject: Type, member: String): Option[Type]
  protected def withDefaults(
      key: String,
      tparams: List[String],
      tdefaults: Map[String, TypeRef],
      targs: List[Type],
      self: Map[String, Type],
  ): List[Type]
  protected def analyzeExpr(expr: Expr, expected: Option[Type] = None, discarded: Boolean = false): TExpr

  /** Analyzes one expression in a place a **result list** may stand (`reference/declarations.md § Several results`). */
  protected def analyzeMulti(expr: Expr, expected: Option[Type] = None): TExpr

  /** Whether a value produced *here* is the enclosing function's own result, which is the third
   * place a result list may stand — and so covers a branch or a block that ends in one.
   */
  protected def wantsResults(expected: Option[Type]): Boolean = retIsList && expected.contains(retTy)
  protected def analyzeBool(e: Expr): TExpr
  protected def analyzePlace(target: Expr, what: String, writes: Boolean = true): TExpr
  protected def requirePlace(t: TExpr, target: Expr, what: String, writes: Boolean = true): TExpr
  protected def invCheckFor(place: TExpr): List[(TExpr, Type.Struct, String)]
  protected def describe(target: Expr): String
  /** Pairs each setter in a member list with the property it writes, filling in the parameter type
   * the source deliberately leaves out (`reference/declarations.md § A property may be settable`).
   *
   * The type is the property's result and can be nothing else, so writing it on the setter as well
   * would be one fact kept in two places and a disagreement to diagnose. Doing it here is also what
   * refuses a setter with no property: the pairing is what gives the parameter a type at all, so an
   * unpaired one would otherwise reach name resolution carrying a placeholder and be reported as a
   * type nobody wrote.
   *
   * `inherited` is the other list a getter may be found in — the members of the trait an `impl`
   * block is keeping, which the block itself need not restate.
   */
  protected def pairSetters(
      members: List[MethodDecl],
      label: String,
      inherited: List[MethodDecl] = Nil,
  ): List[MethodDecl] = {
    val getters = (inherited ::: members).filter(m => m.isProperty).map(m => m.name -> m).toMap

    // A member list may be walked more than once on its way in — an `impl` block's is paired when
    // the block is hoisted and again with the defaults it inherited — so a setter whose parameter
    // already has a type is left exactly as it is. Pairing is idempotent, and the refusal below then
    // fires only for a setter that has never found its property.
    def unpaired(m: MethodDecl): Boolean =
      m.params.exists(_.typ match
        case NamedType(n, _) => n == DeclParser.setterValue
        case _               => false)

    members.map { m =>
      if !DeclParser.isSetter(m.name) || !unpaired(m) then m
      else
        val written = DeclParser.sourceName(m.name)

        getters.get(written).flatMap(_.retType) match
          case Some(ret) => m.copy(params = m.params.map(_.copy(typ = ret))).setPos(m.pos)
          case None =>
            at(m.pos)(err(s"'set $written' writes a property called '$written', and '$label' " +
              "declares none — a property is read as well as written, and its result is where the " +
              "value's type comes from"))
    }
  }

  protected def indexes(traitName: String, receiver: Expr): Boolean

  /** Whether `recv.name = …` reaches a **setter** rather than storage (`reference/declarations.md §
   * A property may be settable`), asked here for the reason `indexes` is: a multiple assignment has
   * to know before it commits to a store, and it is not where the answer lives.
   */
  protected def settable(receiver: Expr, name: String): Boolean
  protected def arithType(op: String, a: Type, b: Type, rhs: Option[Pos]): Type
  protected def constraintOf(t: Type): Option[Type.Constrained]
  protected def updateExpected(op: String, placeTy: Type): Option[Type]
  protected def updateDispatch(op: String, place: TExpr, value: TExpr): Option[TDispatch]
  protected def analyzeBlockBody(stmts: List[Stmt], expected: Option[Type], discarded: Boolean = false): TBlock
  protected def coerce(t: TExpr, expected: Type): TExpr
  protected def autoDeref(t: TExpr): TExpr
  protected def isPlace(t: TExpr): Boolean

  /** Whether an argument of one type, standing where the other is asked for, is a case the
   * *analysis* settles rather than one `coerce` repairs — an array written where a slice goes.
   *
   * It exists for the generic call path, which analyzes its arguments before it knows what the
   * parameters are and so can arrive at a `[3]int` for a position that turned out to want a
   * `[]const int`. A non-generic call never reaches it, having analyzed the argument against the
   * parameter in the first place (`CallCore`).
   */
  protected def becomesSlice(actual: Type, expected: Type): Boolean =
    (Type.unqualified(actual), Type.unqualified(expected)) match
      case (_: Type.Array, _: Type.Slice) => true
      case _                              => false

  /** Whether a place is one a write may not reach — a `val`, or a field or element of one.
   * Declared here because both callers are: assignment and `&` ask it in `ExprAnalysis`, and a
   * `*self` receiver asks it in `CallAnalysis`, which is the same question about the same place.
   */
  protected def readOnly(t: TExpr): Boolean
  protected def instantiateFunc(f: FuncDecl, targs: List[Type], constSelf: Boolean = false): String

  /** The call trait a value of this type implements, where it implements one (`reference/types.md § Function types`). */
  protected def callableOf(t: Type): Option[Type.Bound]

  /** The nested functions of one block, lowered together (`reference/declarations.md`). */
  protected def lowerNestedGroup(group: List[FuncDecl]): List[TStmt]

  /** Which of `candidates` the group's bodies read — what `pendingNeeds` is filled from (`0224`). */
  protected def groupNeeds(group: List[FuncDecl], candidates: Set[String]): Set[String]

  /** `value.name(args)` where `name` is a field holding a callable rather than a method (`reference/types.md § Function types`). */
  protected def callableField(
      rty: Type,
      name: String,
      recv: TExpr,
      args: List[Expr],
      expected: Option[Type],
  ): Option[TExpr]

  /** Analyzes a body inside the analysis of another one, giving back what it yields (`reference/expressions.md § Closures`). */
  protected def analyzeNested(
      name: String,
      params: List[(String, Type)],
      declaredResult: Option[Type],
      body: List[Stmt],
      environment: Option[Environment] = None,
      siblings: Map[String, Nested] = Map.empty,
      variadic: Boolean = false,
  ): (TFunc, Type)
}
