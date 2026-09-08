package sh.sysl

/** Statements, and the blocks and loop contexts they live in.
 *
 * A statement is analyzed as its own **recovery region**: a mistake inside one is recorded, the
 * region abandoned, and the walk resumes at the next statement — which is what turns one error per
 * compilation into one error per mistake. Winding the scope and loop stacks back is part of that,
 * so an abandoned block cannot leave its bindings visible to the code after it.
 *
 * The other thread here is that a block is an **expression**: it has a type, taken from its
 * trailing expression, or `never` where it ends in a jump and control does not arrive at all.
 */
trait StmtAnalysis extends TypeResolution with AsmAnalysis {

  /** A block whose trailing expression (if any) is its value — a function body or an if/match
   * branch. Statements share one lexical scope with the result expression.
   */
  protected def analyzeValueBlock(stmts: List[Stmt], expected: Option[Type], discarded: Boolean = false): TBlock = {
    pushScope()
    // The scope closes however the block ends, which is what a reading that is *allowed* to fail
    // needs: a branch tried without an expectation and then abandoned must leave nothing in view.
    try inBlock(stmts)(analyzeBlockBody(stmts, expected, discarded))
    finally popScope()
  }

  /** Runs a block's statements with that block's own nested functions in view
   * (`reference/declarations.md`).
   *
   * A block is the unit a nested function is hoisted over, so the group is found here, from the
   * statements themselves, rather than by looking ahead from the one being analyzed. What the block
   * declares — the group, and the names it binds — is given back at the end for the same reason a
   * scope is: an inner block's nested functions are not the outer block's.
   */
  protected def inBlock[R](stmts: List[Stmt])(body: => R): R = {
    val savedPending  = pendingNested
    val savedNeeds    = pendingNeeds
    val savedAwaiting = awaitingNeeds
    val savedNested   = nestedFuncs
    val savedDeclares = blockDeclares

    pendingNested = stmts.collect { case f: FuncDecl => f }
    // Every name the block binds, whether or not it has been reached yet — which is what lets a use
    // written above a declaration be told that rather than told the name stands for nothing.
    // Accumulated rather than replaced: a name bound further down an *enclosing* block is still one
    // this block was written above, and a body nested inside this one is written above it too.
    // A `const` is deliberately absent: one written in a block is refused outright, so it binds
    // nothing and a use of its name is genuinely undefined rather than written too early. Listing it
    // here made every such use report that it was "declared below" — from above it and from below it
    // alike, since the name was never bound at all.
    // **Where** each is bound travels with it, because `notYetBound` has two mistakes to tell apart
    // and the position is the only thing that separates them (card `0221`).
    val binds = stmts.flatMap(boundBy)

    blockDeclares = savedDeclares ++ binds

    // What the group reads from **this** block, whether or not it is bound yet (`0224`). The group
    // waits for the last of these and is lowered directly after it, because the environment holds
    // the address of every capture and a slot whose declaration has not run has no name to point at.
    // Empty for a group that reads only what is above it, which is the ordinary case and lowers
    // where it always did.
    pendingNeeds = groupNeeds(pendingNested, binds.map(_._1).toSet)
    awaitingNeeds = false

    try body
    finally
      pendingNested = savedPending
      pendingNeeds = savedNeeds
      awaitingNeeds = savedAwaiting
      nestedFuncs = savedNested
      blockDeclares = savedDeclares
  }

  /** The names a statement binds, with where it binds them. */
  private def boundBy(stmt: Stmt): List[(String, Option[Pos])] = stmt match
    case s @ VarDecl(n, _, _, _, _, _) => List(n -> s.pos)
    case s @ ValDecl(n, _, _, _, _, _) => List(n -> s.pos)
    case s @ RefDecl(n, _)             => List(n -> s.pos)
    case s @ MultiDecl(ns, _, _)       => ns.map(_ -> s.pos)
    case s @ PatternDecl(p, _, _)      => patternNames(p).map(_ -> s.pos)
    case _                             => Nil

  /** Strikes off what a statement just bound, and lowers the pending group once it has them all.
   *
   * Run after **every** statement, so the environment is built directly after the *last* binding any
   * of the block's nested functions reads — which is the whole of what lets one read a binding
   * written below it (`0224`). The lowered environment is returned as statements to sit at that
   * point, exactly as it used to sit at the first function.
   *
   * `awaitingNeeds` is what keeps it from being lowered *above* where the functions are written: a
   * binding that completes the set before any of them has been reached leaves the group pending, and
   * the first function lowers it in the ordinary way.
   */
  private def settleNeeds(stmt: Stmt): List[TStmt] = {
    if pendingNeeds.nonEmpty then pendingNeeds = pendingNeeds -- boundBy(stmt).map(_._1)

    if awaitingNeeds && pendingNeeds.isEmpty && pendingNested.nonEmpty then
      val group = pendingNested

      pendingNested = Nil
      awaitingNeeds = false
      lowerNestedGroup(group)
    else Nil
  }

  /** The body of a value block, using whatever scope the caller has established — a match arm
   * runs this after declaring its pattern bindings, so they are visible to the body.
   *
   * A block that ends in a **jump** — `return`, `break`, `continue` — has no trailing expression to
   * be the value of, and it does not fall out the bottom either, so its type is `never` rather than
   * `unit`. That is what lets `if c then 1 else return 0` be an `int`: the jump is still not an
   * expression (`reference/statements.md § return`), but the block around it is one, and its type
   * says control does not arrive.
   *
   * **A block in statement position has no value, whatever its last expression yields** (`reference/expressions.md § Statement position discards a block's value`).
   * There is no statement terminator to write "and throw this away" with, so a trailing call, an
   * assignment, or an `i++` would otherwise make a block that was plainly written for its effect
   * claim a value nobody asked for — and two such blocks under one `if` or `match` would then be
   * made to agree on it. A block that does not *arrive* keeps `never`: that is reachability rather
   * than a value, and the code around it is still entitled to know.
   */
  protected def analyzeBlockBody(stmts: List[Stmt], expected: Option[Type], discarded: Boolean = false): TBlock =
    stmts.reverse match
      case ExprStmt(e) :: initRev =>
        val init = initRev.reverse.flatMap(recoverStmt)
        // A block whose value *is* the enclosing function's result is the third place a result list
        // may stand (`reference/declarations.md § Several results`), which is what lets a branch or
        // a nested block forward one.
        val tr =
          if wantsResults(expected) then analyzeMulti(e, expected)
          else analyzeExpr(e, expected, discarded)
        TBlock(init, Some(tr), if discarded && tr.ty != Type.Never then Type.Unit else tr.ty)
      case (_: Return | _: Break | _: Continue) :: _ =>
        TBlock(stmts.flatMap(recoverStmt), None, Type.Never)
      case _ =>
        TBlock(stmts.flatMap(recoverStmt), None, Type.Unit)

  /** A statement sequence used only for its effects (a loop body): a fresh scope, no value. */
  protected def analyzeStmts(stmts: List[Stmt]): List[TStmt] = {
    pushScope()
    val r = loopStmts(stmts)
    popScope()
    r
  }

  /** A loop body: the `invariant` and `variant` clauses at its head (`reference/verification.md §
   * invariant and variant on a loop`), then the rest.
   *
   * The clauses are analyzed **in the loop's own scope**, before anything the body declares, which
   * is what lets one read the loop variable and stops one reading a local declared under it. The
   * split is by position, exactly as a function's `require`/`ensure` block is split — a clause that
   * reaches `recoverStmt` is therefore one written somewhere it may not be, and says so there.
   */
  protected def loopStmts(stmts: List[Stmt]): List[TStmt] = {
    val (clauses, rest) = stmts.span { case _: Invariant | _: Variant => true; case _ => false }
    val tclauses        = clauses.map(loopClause)
    // The rest is analyzed BEFORE the variant is handed up, so a loop nested inside this one has
    // already taken its own and put it back to `None`.
    val slot            = pendingVariant
    pendingVariant = None
    val trest = inBlock(rest)(rest.flatMap(recoverStmt))

    pendingVariant = slot
    tclauses ::: trest
  }

  /** One `invariant` or `variant` at the head of a loop body. */
  private def loopClause(c: Stmt): TStmt = at(c.pos) {
    c match
      case Invariant(cond, msg) => TInvariant(analyzeBool(cond), msg)
      case Variant(e) =>
        if pendingVariant.isDefined then
          err("a loop declares one 'variant' — a measure is the thing that decreases, and two of " +
            "them say nothing about which")
        val te = analyzeExpr(e)
        val ty = te.ty match
          case i: Type.Integer => i
          case other           => err(s"a 'variant' is an integer measure, not ${show(other)}")
        variantSeq += 1
        val slot = s"variant$variantSeq"

        pendingVariant = Some((slot, ty))
        TVariantCheck(slot, ty, te)
      case _ => sys.error("unreachable loop clause")
  }

  /** Wraps a loop that declared a `variant` so its slots are set up once per entry (`reference/verification.md § invariant and variant on a loop`). */
  protected def checkedLoop(ctx: LoopCtx, loop: TExpr): TExpr =
    ctx.variant match
      case Some((slot, ty)) => TCheckedLoop(slot, ty, loop)
      case None             => loop

  /** Analyzes a loop body with a fresh loop context on the stack, so `break`/`continue` inside it
   * resolve to this loop and each `break` value is collected for the loop's result type. Returns
   * the typed body together with the context the breaks were recorded in.
   */
  protected def analyzeLoopBody(expected: Option[Type], label: Option[String])(
      body: => List[TStmt],
  ): (List[TStmt], LoopCtx) = {
    label.foreach { l =>
      if loops.exists(_.label.contains(l)) then err(s"label '$l is already in scope")
    }
    val ctx = new LoopCtx(expected, label)
    loops = ctx :: loops
    val tb = body
    loops = loops.tail
    ctx.variant = pendingVariant
    pendingVariant = None
    (tb, ctx)
  }

  /** Resolves a `break`/`continue` to the loop it targets and that loop's distance out from the
   * innermost. An absent label is the nearest loop; a `'name` is the nearest loop carrying it.
   */
  private def resolveLoop(keyword: String, label: Option[String]): (LoopCtx, Int) = label match
    case None =>
      loops match
        case ctx :: _         => (ctx, 0)
        case Nil if inConstFor => notInAnUnrolledLoop(keyword)
        case Nil              => err(s"'$keyword' is only allowed inside a loop")
    case Some(l) =>
      loops.indexWhere(_.label.contains(l)) match
        case -1 if inConstFor => notInAnUnrolledLoop(keyword)
        case -1               => err(s"no enclosing loop is labeled '$l")
        case i                => (loops(i), i)

  /** Why a `for const` body takes neither `break` nor `continue` (`reference/generics.md § A
   * parameter may stand for a list of types`).
   *
   * There is no loop at run time for either to act on: the copies are straight-line code in the
   * enclosing function, so a `break` here would leave whatever loop the `for const` happens to sit
   * inside — silently, and one copy at a time. That is a wrong answer rather than a missing feature,
   * which is why this is refused rather than lowered to something.
   */
  private def notInAnUnrolledLoop(keyword: String): Nothing =
    err(s"a 'for const' is unrolled into one copy of its body per value, so there is no loop for " +
      s"'$keyword' to act on — a loop that is walked at run time is the ordinary 'for'")

  /** The result type of a loop: every `break` value and the `else` value must meet at one type.
   * With no `else`, normal completion yields `unit`, so a value-carrying `break` has nothing to
   * meet on that path — a clear error rather than a silent mismatch.
   */
  protected def loopResultType(ctx: LoopCtx, elseBlock: Option[TBlock]): Type = {
    val elseTy = elseBlock.map(_.ty).getOrElse(Type.Unit)
    val tys    = (ctx.breakTys.toList :+ elseTy).distinct
    val joined = tys.foldLeft(Option(tys.head))((acc, t) => acc.flatMap(join(_, t)))

    if joined.isDefined then joined.get
    else if elseBlock.isEmpty then
      val v = ctx.breakTys.find(t => !Type.noValue(t)).get
      err(s"this loop breaks with a ${show(v)} but has no 'else' to give a value when it finishes normally — add an 'else'")
    else
      err(s"a loop's break values and its 'else' must have the same type, but got ${tys.map(show).mkString(" and ")}")
  }

  /** The result type of a `loop`, which has no `else` and so nothing on the normal-completion path
   * for its `break` values to meet: they decide it between them.
   *
   * A `loop` nothing breaks out of is `never`. That is the one thing `while true` could not say —
   * a condition the analyzer does not evaluate leaves it looking like a loop that might finish —
   * and it is what lets one stand as the last thing a function returning a value does.
   */
  protected def endlessResultType(ctx: LoopCtx): Type = {
    val tys = ctx.breakTys.toList.distinct

    if tys.isEmpty then Type.Never
    else
      tys.foldLeft(Option(tys.head))((acc, t) => acc.flatMap(join(_, t))).getOrElse {
        err(s"a loop's break values must have the same type, but got ${tys.map(show).mkString(" and ")}")
      }
  }

  /** Analyzes one statement as its own recovery region, so a mistake costs the statement it is
   * in and nothing after it.
   *
   * The statement it stands in for is a no-op: nothing is emitted from a program that has an
   * error, so the substitute only has to be well-formed enough for the walk to continue.
   */
  protected def recoverStmt(stmt: Stmt): List[TStmt] = {
    // A statement abandoned part-way may have opened a scope or entered a loop that it never got
    // to close, so both are wound back to where the statement started. Without this an inner
    // block's bindings would stay visible after it, and the analyzer would be *more* permissive
    // after an error than before one.
    val depth = scopes.length
    val outer = loops

    recover {
      while scopes.length > depth do popScope()
      loops = outer
      bindFailed(stmt)
      List(TExprStmt(TUnitLit()))
    }(analyzeStmt(stmt) ::: settleNeeds(stmt))
  }

  /** Keeps what the rest of the block will need from a statement that failed.
   *
   * A `var` binds its name even when its initializer did not analyze — at `Type.Unknown`, which
   * poisons rather than reports — because otherwise one bad initializer turns every later use of
   * the name into an "undefined name" of its own, and the real mistake is lost among them.
   */
  private def bindFailed(stmt: Stmt): Unit = stmt match
    case VarDecl(name, _, _, _, _, _) => declare(name, Type.Unknown)
    case ValDecl(name, _, _, _, _, _) => declareReadOnly(name, Type.Unknown)
    // A ref whose place did not analyze binds the name at `Type.Unknown` like the other two, and
    // records no place: there is nothing to walk outward through, and a guard built from a poisoned
    // node would refuse assignments for a reason the program never gave.
    case RefDecl(name, _)       => declare(name, Type.Unknown)
    case MultiDecl(names, mutable, _) =>
      for n <- names do if mutable then declare(n, Type.Unknown) else declareReadOnly(n, Type.Unknown)
    case PatternDecl(p, mutable, _) =>
      for n <- patternNames(p) do
        if mutable then declare(n, Type.Unknown) else declareReadOnly(n, Type.Unknown)
    case _                      =>

  /** Whether a `?` sits anywhere in this tree.
   *
   * The descent is structural because the operator can be at any depth of any expression, and it
   * stops at a `Type` — which holds no expression, and whose recursive forms would otherwise be
   * walked forever.
   */
  private def containsTry(x: Any): Boolean = x match
    case _: Type         => false
    case _: TTry         => true
    case xs: Iterable[?] => xs.exists(containsTry)
    case p: Product      => p.productIterator.exists(containsTry)
    case _               => false

  private def analyzeStmt(stmt: Stmt): List[TStmt] = at(stmt.pos)(analyzeStmtAt(stmt))

  /** `a, b = b, a` and `a, b += 1, 2` (`reference/expressions.md § Several places at once`).
   *
   * Every place is analyzed before any value, which is what fixes each value's expected type: a
   * literal on the right takes the width of the place it is heading for, exactly as it does after a
   * single `=`. Each arm is then checked by the rule its own operator already has — the plain form
   * wants a value the place can hold, the compound one wants an operator that does not change the
   * place's type — so the form adds no rule of its own beyond the two sides being the same length.
   * The ordering it promises is a run-time matter and belongs to codegen.
   */
  /** The values a comma form on the left is given, where the right side is **one** thing carrying
   * several (`reference/types.md § Tuples`) — the third of the three feeds one comma syntax has.
   *
   * The one thing is evaluated once, into a name no program can write, and each part is read back
   * out of it. That is what keeps `a, b = f()` a single call, and it costs the form nothing else:
   * the hidden binding is an ordinary local, so it is counted, released and walked exactly as a
   * written one is.
   *
   * `None` where the right side is a list rather than one carrier, which is every other case.
   */
  private def spread(places: Int, values: List[Expr], subject: String): Option[(TStmt, List[TExpr])] =
    values match
      case List(one) if places > 1 =>
        val tv = at(one.pos)(analyzeMulti(one))

        Type.underlying(tv.ty) match
          case t: Type.Tuple if t.targs.length == places =>
            val name = declare(s"${Modules.sep}parts", tv.ty)
            val read = t.fields.indices.toList.map(i => TField(TLoad(name, tv.ty), i, t.fields(i)._2))

            Some((TVarDecl(name, tv.ty, tv), read))

          // A result list and a tuple are both taken apart here, and each is complained about in
          // its own terms: a call that yields several things has *results*, and the type nobody
          // wrote is not worth naming back at the reader.
          case t: Type.Tuple if yieldsResults(tv) =>
            at(one.pos)(err(s"$subject, and this yields ${quantity(t.targs.length, "result")}"))

          case t: Type.Tuple =>
            at(one.pos)(err(s"$subject, and a ${show(tv.ty)} has ${quantity(t.targs.length, "part")} " +
              "to give them"))

          case other =>
            at(one.pos)(err(s"$subject, and one ${show(other)} is not something to take apart — " +
              "only a tuple is"))
      case _ => None

  /** Whether a value came from a call declaring a **result list** rather than one type. */
  private def yieldsResults(t: TExpr): Boolean = t match
    case c: TCall  => c.results
    case c: TVCall => c.results
    case _         => false

  private def multiAssign(m: MultiAssign): List[TStmt] = {
    val taken =
      spread(m.targets.length, m.values, s"this assignment has ${m.targets.length} places on the left")

    if taken.isEmpty && m.targets.length != m.values.length then
      err(s"this assignment has ${m.targets.length} places on the left and ${m.values.length} " +
        s"${if m.values.length == 1 then "value" else "values"} on the right")

    // `b[i] = v` on a container is a call to `index_set` rather than a store (`14`), so it has no
    // address for the phase that locates the places to find and no reading of it separates what it
    // reads from what it writes. Saying that is worth more than the "needs something with an
    // address" the ordinary path would reach.
    for t <- m.targets do
      t match
        case Index(receiver, _) if indexes(Library.key("IndexSet"), receiver) =>
          at(t.pos)(err(s"an element set through '${qn(Library.key("Index"))}' is a call rather " +
            "than a store, so it cannot be one place of a multiple assignment — write that one " +
            "on its own"))
        // A property set through a setter is the second form of the same kind, and refused for the
        // same reason (`reference/expressions.md § Several places at once`): the call both reads and writes, so there is nothing to separate
        // into the two halves this form's ordering rule is about.
        case Field(receiver, name) if settable(receiver, name) =>
          at(t.pos)(err(s"'$name' is set by a call rather than by a store, so it cannot be one " +
            "place of a multiple assignment — write that one on its own"))
        case _ =>

    val what   = if m.op == "=" then "assignment" else s"'${m.op}'"
    val places = m.targets.map(analyzePlace(_, what))

    // A part read out of a carrier is already analyzed, so what is left for the arm is the check
    // the written form performs after analyzing its own value. The two are kept together here so
    // that neither feed can end up with a rule the other does not have.
    def value(target: Expr, place: TExpr, part: Option[TExpr], written: Expr): TWrite =
      if m.op == "=" then
        val tv = part.getOrElse(analyzeExpr(written, Some(place.ty)))

        if tv.ty == Type.Never || disagree(tv.ty, place.ty) then
          err(s"cannot assign ${show(tv.ty)} to ${describe(target)} of type ${show(place.ty)}")
        TWrite(place, m.op, tv, None, invCheckFor(place))
      else
        val binSym = m.op.dropRight(1)
        val raw    = part.getOrElse(analyzeExpr(written, updateExpected(binSym, place.ty)))
        // `v += 1.0` is `v = v + 1.0`, so the scalar splats here exactly as it does in the binary
        // form — the two spellings reach the same instruction and have to agree about it, which is
        // the rule this whole branch is written around.
        val tv     = balanceLanes(TZero(place.ty), raw)._2
        val d      = updateDispatch(binSym, place, tv)

        if d.isEmpty && disagree(arithType(binSym, place.ty, tv.ty, tv.pos), place.ty) then
          err(s"'${m.op}' would change the type of ${describe(target)}")
        TWrite(place, m.op, tv, d, invCheckFor(place), constraintOf(place.ty))

    // Each arm's position is the value it was written with, or — where one carrier supplied them
    // all — that carrier, since there is no separate expression to point at.
    val written = taken.fold(m.values)(_ => List.fill(m.targets.length)(m.values.head))
    val parts   = taken.fold(m.values.map(_ => None))(_._2.map(Some(_)))

    val writes =
      for (((target, place), part), w) <- m.targets.zip(places).zip(parts).zip(written)
      yield at(w.pos)(value(target, place, part, w))

    taken.map(_._1).toList :+ TMultiAssign(writes)
  }

  /** `val a, b = …` / `var a, b = …` (`reference/expressions.md § Several places at once`).
   *
   * The names are declared only once every value has been analyzed, so a value may still name
   * whatever the enclosing scope calls one of them — the binding does not shadow itself half way
   * through its own right-hand side.
   */
  private def multiDecl(m: MultiDecl): List[TStmt] = {
    for (n, i) <- m.names.zipWithIndex do
      if m.names.indexOf(n) != i then err(s"'$n' is named twice in one binding")

    val taken = spread(m.names.length, m.values, s"this binding names ${m.names.length} things")

    if taken.isEmpty && m.names.length != m.values.length then
      err(s"this binding names ${m.names.length} things and has ${m.values.length} " +
        s"${if m.values.length == 1 then "value" else "values"} to give them")

    val tvs = taken.fold {
      for v <- m.values
      yield
        val tv = at(v.pos)(analyzeExpr(v, None))
        if tv.ty == Type.Never then at(v.pos)(err("cannot bind a name to an expression that never returns"))
        tv
    }(_._2)

    taken.map(_._1).toList ::: (
      for (name, tv) <- m.names.zip(tvs)
      yield TVarDecl(if m.mutable then declare(name, tv.ty) else declareReadOnly(name, tv.ty), tv.ty, tv)
    )
  }

  /** Every name a pattern binds, in the order it binds them, so that a repeat can be reported
   * against the whole binding rather than found later as a shadow.
   */
  private def patternNames(p: Pattern): List[String] = p match
    case IdentPattern(n)        => List(n)
    // The outer name first, which is the order it is written in — so `v @ (v, w)` reports the
    // second `v` as the repeat rather than the first.
    case BindPattern(n, inner)  => n :: patternNames(inner)
    case TuplePattern(ps)       => ps.flatMap(patternNames)
    case StructPattern(_, fps)  => fps.flatMap((_, sub) => patternNames(sub))
    case WildcardPattern        => Nil
    case _                      => Nil

  /** `val (a, b) = …` / `var (a, b) = …` (`reference/types.md § Tuples`).
   *
   * The value is analyzed once into a temporary and the pattern is then walked against its type,
   * each part reading a field of what is above it. That is `spread`'s mechanism — the comma form
   * takes a tuple apart exactly this way — with the one difference that a pattern may go deeper, so
   * the walk recurses where `spread` stops after a level.
   *
   * **A part that is not a name is checked here rather than at `PatternAnalysis`**, because the
   * question is not the one a match arm asks. An arm may be refutable and fall through; a binding
   * has nowhere to fall, so what is refused is a pattern that could fail to match at all — and the
   * diagnostic names that, rather than the match-arm rules the reader is not in.
   */
  private def patternDecl(d: PatternDecl): List[TStmt] = {
    val names = patternNames(d.pattern)

    for (n, i) <- names.zipWithIndex do
      if names.indexOf(n) != i then err(s"'$n' is named twice in one binding")

    val tv = at(d.value.pos)(analyzeExpr(d.value, None))

    if tv.ty == Type.Never then
      at(d.value.pos)(err("cannot bind a name to an expression that never returns"))

    // The whole value is held once, under a name no program can write, and every part is read out
    // of it. Reading the value expression again per part would run its side effects once each.
    val temp = declare(s"${Modules.sep}parts", tv.ty)

    TVarDecl(temp, tv.ty, tv) :: bindPattern(d.pattern, TLoad(temp, tv.ty), d.mutable)
  }

  /** One level of a pattern binding: bind a name, ignore a wildcard, or read the parts of a tuple
   * and descend into each.
   */
  private def bindPattern(p: Pattern, subject: TExpr, mutable: Boolean): List[TStmt] = p match
    case WildcardPattern => Nil

    // `var whole @ (a, b) = pair` — the value under its own name, and its parts under theirs. The
    // subject is already the held temporary rather than the value expression, so naming it twice
    // reads that temporary twice and runs nothing a second time.
    case BindPattern(n, inner) =>
      bindPattern(IdentPattern(n), subject, mutable) ++ bindPattern(inner, subject, mutable)

    case IdentPattern(n) =>
      List(TVarDecl(if mutable then declare(n, subject.ty) else declareReadOnly(n, subject.ty),
        subject.ty, subject))

    case TuplePattern(ps) =>
      Type.underlying(subject.ty) match
        case t: Type.Tuple if t.targs.length == ps.length =>
          (for (sub, i) <- ps.zipWithIndex
           yield bindPattern(sub, TField(subject, i, t.fields(i)._2), mutable)).flatten

        case t: Type.Tuple =>
          err(s"this pattern takes ${quantity(ps.length, "part")}, and a ${show(subject.ty)} has " +
            s"${quantity(t.targs.length, "part")} to give it")
          Nil

        case other =>
          err(s"one ${show(other)} is not something to take apart — only a tuple is")
          Nil

    // A struct has exactly one shape, so naming it cannot fail and it belongs at a binding for the
    // same reason a tuple pattern does — `reference/patterns.md § Struct patterns` calls a tuple
    // pattern the positional form of this
    // one. What differs from the match path is that an unlisted field binds nothing here rather than
    // being filled with a wildcard: there is no exhaustiveness to discharge at a binding.
    case StructPattern(name, fieldPats) =>
      Type.underlying(subject.ty) match
        case s: Type.Struct if typeKey(name).contains(s.base) =>
          fieldPats.map(_._1).groupBy(identity).collectFirst { case (n, xs) if xs.size > 1 => n }
            .foreach(n => err(s"field '$n' is matched more than once"))

          (for (fname, sub) <- fieldPats
           yield s.fields.indexWhere(_._1 == fname) match
             case -1 =>
               err(s"struct '${qn(s.base)}' has no field '$fname'")
               Nil
             case i =>
               checkFieldVisible(s.base, fname)
               bindPattern(sub, TField(subject, i, s.fields(i)._2), mutable)).flatten

        case s: Type.Struct =>
          err(s"'$name{…}' does not match a ${show(s)} value")
          Nil

        case other =>
          err(s"'$name{…}' matches a struct, but the value is ${show(other)}")
          Nil

    // `Name(…)` is a variant pattern or the positional form of a struct pattern, told apart by what
    // the value's type turns out to be — the same rule an arm applies. Against a struct it is one
    // shape and belongs here for the reason `Name{…}` does; the positional form additionally names
    // every field, which is what makes adding one to the struct a checked to-do rather than a
    // silently shorter binding.
    case VariantPattern(written, args) if Type.underlying(subject.ty).isInstanceOf[Type.Struct] =>
      val s = Type.underlying(subject.ty).asInstanceOf[Type.Struct]

      if !typeKey(written).contains(s.base) then
        err(s"'$written(…)' does not match a ${show(s)} value")
        Nil
      else if args.length != s.fields.length then
        err(s"struct '${qn(s.base)}' has ${quantity(s.fields.length, "field")}, " +
          s"but ${supplied(args.length, "sub-pattern")}")
        Nil
      else
        checkEveryFieldVisible(s.base, s.fields.map(_._1), "this pattern",
          "take apart the fields it does offer by name, as 'Name{…}'")

        (for (sub, i) <- args.zipWithIndex
         yield bindPattern(sub, TField(subject, i, s.fields(i)._2), mutable)).flatten

    // Everything `reference/statements.md § match` admits in an arm and a binding cannot use. Named
    // individually, because the reason differs: a literal or a range is a *test*, and a variant is
    // a choice among several.
    case _: LitPattern | _: RangePattern =>
      err("a binding cannot test a value — this pattern matches only some values, and a binding " +
        "has no other arm to take when it does not match")
      Nil

    case _: VariantPattern =>
      err("a binding cannot choose among variants — this pattern matches one of several shapes, " +
        "and a binding has no other arm to take when the value has another")
      Nil

    // A quoted name is a *reference*, so it tests rather than binds
    // (`reference/patterns.md § A backticked name references rather than binds`). It earns its own
    // sentence because the fix is not the one the other refusals want: the name here is almost
    // always the name that was meant, and it is the backticks that are wrong.
    case EqPattern(n) =>
      err(s"a binding cannot test a value — '`$n`' names something already declared, and a binding " +
        s"has no other arm to take when the value turns out to differ; write '$n' to bind a new name")
      Nil

  /** `@align(n)` above a binding, folded to the boundary it named.
   *
   * The same fold a struct's goes through, so a bound that is not a power of two or is not a
   * constant is refused in the same words — one rule about what an alignment is, stated once. A
   * refusal costs the annotation and not the declaration, which is what lets the rest of the
   * function still be analyzed against a binding that exists.
   */
  private def boundary(name: String, align: Option[Expr]): Option[Int] =
    align.flatMap(a => recover(Option.empty[Int])(alignBound(name, a)))

  /** `@section("…")` above a **local**, which is refused here rather than in the grammar.
   *
   * The two are the same syntax and differ only in where they stand: a top-level `var` is module
   * storage in every file but the one the program starts in, where it is a local of the entry point
   * (`reference/modules.md § Where a program starts`). So the parser cannot tell them apart, and this is the first place that can.
   *
   * What it refuses is real rather than unimplemented. A local's storage is the frame, laid down by
   * whichever call is running; a section is a region of the image, decided once at the link. There is
   * no reading of the attribute that would make both true.
   */
  private def noSection(name: String, section: Option[String]): Unit =
    for s <- section do
      err(s"'$name' is a local, so it cannot be placed in section \"$s\" — its storage is the frame " +
        "of whichever call is running, and a section is a region of the image the linker decides " +
        "once. Declare it as module storage, outside every function, for a section to be about")

  /** Most statements are one statement. The two comma forms are the exception, and the only reason
   * this hands back a list: a binding that names several things is several declarations.
   */
  private def analyzeStmtAt(stmt: Stmt): List[TStmt] = stmt match
    case m: MultiAssign => multiAssign(m)
    case m: MultiDecl   => multiDecl(m)
    case d: PatternDecl => patternDecl(d)

    // An import binds a name for the statements after it and emits nothing. It is read here rather
    // than gathered ahead of the block because it takes effect where it is written, as Scala's
    // does — the code above it has not imported anything.
    case i: ImportDecl =>
      importInBlock(i)
      List(TExprStmt(TUnitLit()))

    case VarDecl(name, typOpt, Some(init), _, align, section) =>
      noSection(name, section)
      val declared = typOpt.map(rt)
      val ti       = analyzeExpr(init, declared)
      // A binding needs a value to hold, and an initializer that does not finish never produces
      // one — the code after it is unreachable, so the declaration is a mistake rather than a
      // clever way to spell divergence.
      if ti.ty == Type.Never then err(s"cannot bind '$name' to an expression that never returns")
      val declTy = declared.getOrElse(ti.ty)
      if declared.isDefined && disagree(ti.ty, declTy) then
        err(s"cannot initialize '$name': declared ${show(declTy)} but the value is ${show(ti.ty)}")
      List(TVarDecl(declare(name, declTy), declTy, ti, boundary(name, align)))

    // A local `val` is a `var` that may not be assigned to again — same frame, same lifetime, same
    // code. Only the binding differs, so the two share everything below this line, and the read-only
    // half of the rule is enforced where an assignment target is checked rather than here.
    case ValDecl(name, typOpt, init, _, align, section) =>
      noSection(name, section)
      val declared = typOpt.map(rt)
      val ti       = analyzeExpr(init, declared)
      if ti.ty == Type.Never then err(s"cannot bind '$name' to an expression that never returns")
      val declTy = declared.getOrElse(ti.ty)
      if declared.isDefined && disagree(ti.ty, declTy) then
        err(s"cannot initialize '$name': declared ${show(declTy)} but the value is ${show(ti.ty)}")
      List(TVarDecl(declareReadOnly(name, declTy), declTy, ti, boundary(name, align)))

    // `ref name = place` (`reference/memory.md § ref — a name for a place`). The place is analyzed
    // once, here, and what the name means afterwards is the storage it found — so neither the path
    // nor the checks along it are repeated, and neither is the copy a `var` would have made.
    case RefDecl(name, placeExpr) =>
      val tp = analyzeExpr(placeExpr)

      // Rule one, and the message says what a place is rather than that this is not one: the mistake
      // is nearly always a call, and "it has no address" is the fact that explains every case at once.
      if !isPlace(tp) then
        err(s"'ref' names a place — a local, a field, an element, or a dereference — and this " +
          s"expression has no address for '$name' to name. Bind it with 'val' to hold the value it " +
          s"produces")
      if tp.ty == Type.Never then err(s"cannot bind '$name' to an expression that never returns")

      List(TRefDecl(declareRef(name, tp, refHazards(tp)), tp.ty, tp))

    case VarDecl(name, typOpt, None, _, align, section) =>
      noSection(name, section)
      val ty = typOpt.map(rt).getOrElse(err(s"'$name' needs either a type or an initial value"))
      if !hasZero(ty) then err(s"${show(ty)} has no zero value, so '$name' needs an initial value")
      List(TVarDecl(declare(name, ty), ty, TZero(ty), boundary(name, align)))

    // A statement's value is nobody's, so whatever branching it contains is analyzed knowing that.
    case ExprStmt(e) =>
      List(TExprStmt(analyzeExpr(e, None, discarded = true)))

    case Return(opt) =>
      val tv = opt.map(e => if retIsList then analyzeMulti(e, Some(retTy)) else analyzeExpr(e, Some(retTy)))
      tv match
        case Some(_) if retTy == Type.Unit    => err("cannot return a value from a function with no return type")
        case Some(t) if disagree(t.ty, retTy) => err(s"return type mismatch: expected ${show(retTy)}, got ${show(t.ty)}")
        case None if retTy != Type.Unit       => err(s"this function must return a ${show(retTy)} value")
        case _                                =>
      List(TReturn(tv))

    /** `become f(…)` — `return f(…)` with the jump guaranteed (`TailJumps`).
     *
     * The value is read at the function's own result type, exactly as a `return`'s is, because that
     * is what it is: the callee's result is this function's. What is *not* settled here is whether
     * the frame can be replaced — that needs the callee's own signature and this function's
     * parameters as the typed tree has them, so it is `TailJumps.check`'s and runs over the whole
     * program.
     *
     * What is settled here is that there is a call at all. Anything else is a mistake about what the
     * word means rather than about what a frame is, and saying so at the statement is where a reader
     * is looking.
     */
    case Become(call) =>
      val t = analyzeExpr(call, Some(retTy))

      t match
        case _: TCall | _: TCallPtr => ()
        case _ =>
          err("'become' replaces this frame with a call's, so what follows it has to be a call — " +
            "'return' is what hands a value back without replacing anything")

      if disagree(t.ty, retTy) then
        err(s"'become' hands on this function's result, so the call has to answer ${show(retTy)} — " +
          s"this one answers ${show(t.ty)}")

      List(TBecome(t))

    // `break value` records its type against the loop it targets — the nearest, or the one a
    // `'label` names — which unites it with that loop's other breaks and its `else` to fix the
    // loop's result type. The value is analyzed in the target loop's expected type, so a `break &T`
    // boxes the same way a `break` in a `&T` context asks.
    case Break(label, opt) =>
      val (ctx, depth) = resolveLoop("break", label)
      val tv           = opt.map(e => analyzeExpr(e, ctx.expected))
      ctx.breakTys += tv.map(_.ty).getOrElse(Type.Unit)
      List(TBreak(tv, depth))

    case Continue(label) =>
      val (_, depth) = resolveLoop("continue", label)
      List(TContinue(depth))

    // `defer stmt` (`reference/memory.md § Where defer sits`). The statement is analyzed here,
    // where it is written, so it sees exactly the names in scope at that point — and it is handed
    // to the block rather than emitted, so nothing runs until the block is left.
    //
    // What may be deferred is bounded by what teardown can mean. A jump would leave the block from
    // inside the code that runs *because* the block is already being left, and there is no second
    // exit to take. A declaration would bind a name that dies before anything could read it. Both
    // are refused by name rather than by a general rule, because each is a different mistake.
    case Defer(inner) =>
      inner match
        case _: Return =>
          err("a deferred statement runs while its block is being left, so it cannot 'return' — " +
            "there is no exit left to take. Compute what the function returns before the block ends")
        case _: Break | _: Continue =>
          err("a deferred statement runs while its block is being left, so it cannot 'break' or " +
            "'continue' — the loop edge it would take is the one already being taken")
        case _: Defer =>
          err("'defer' schedules a statement, and scheduling a scheduling has nothing to run: " +
            "write the statement itself after this 'defer'")
        case _: VarDecl | _: ValDecl | _: MultiDecl | _: ConstDecl =>
          err("a deferred declaration binds a name that dies the moment the statement finishes, " +
            "so nothing could read it — defer what uses the value instead of what declares it")
        case _ =>

      val body = analyzeStmts(List(inner))

      // A `?` leaves the function, which is the same exit a `return` would take and is refused for
      // the same reason. It is read out of the analyzed body rather than the written statement
      // because `?` is an operator inside an expression, at whatever depth the expression has.
      if body.exists(containsTry) then
        err("a deferred statement runs while its block is being left, so a '?' in it has nowhere " +
          "to return to. Check the result and handle the failure in the deferred statement itself")

      List(TDefer(body))

    // `asm` (`reference/inline-assembly.md`). The arms are chosen between here, so what reaches the
    // emitter is one architecture's instructions and no record that there were others.
    case a: AsmStmt => List(analyzeAsm(a))

    // A `const` written at the top of a file is hoisted and folded into its uses before anything
    // runs, so it never reaches the statement walk — `ModuleFiles.entryPoint` filters it out with the
    // other declarations. One that reaches here was written **inside a body**, where there is nothing
    // to hoist it into, and is refused for the same reason the types below are.
    case _: ConstDecl =>
      err("a constant is a module member and is declared at the top level — it is folded into its " +
        "uses before the program runs, so there is no block for one written here to belong to. A " +
        "name bound to a value inside a body is a 'val'")

    // A `c const` block is measured **per file**, by one probe carrying that file's `@include`s, so
    // the top level of a file is the only place one can be asked from. It is also a module member
    // for the reason above, which the C in it does not change.
    case _: CConstBlock =>
      err("a 'c const' block is declared at the top level of a file — the C in it is compiled " +
        "against that file's '@include' headers, and a block inside a body would be asking the " +
        "same question from a place that cannot answer it")

    // A `c type` is measured by the same probe and declares a **type**, so it is out of place here
    // twice over: for the reason above, and for the one a `struct` written in a body is.
    case _: CTypeBlock =>
      err("a 'c type' block is declared at the top level of a file — the C in it is compiled " +
        "against that file's '@include' headers, and what it declares is a type, which is a module " +
        "member rather than something a body can hold")

    // A function declared inside a body is a **nested function** (`reference/declarations.md`), and
    // the block's are lowered together the first time one is reached — so the ones after it in the
    // same block have already been dealt with and contribute nothing further here.
    case _: FuncDecl =>
      if pendingNested.isEmpty then Nil
      // The group reads something this block binds further down, so its environment cannot be built
      // here — it holds the *address* of every capture, and a slot whose declaration has not run has
      // no name to point at. It waits, and `settleNeeds` lowers it after the last of them (`0224`).
      else if pendingNeeds.nonEmpty then
        awaitingNeeds = true
        Nil
      else
        val group = pendingNested

        pendingNested = Nil
        lowerNestedGroup(group)

    case _: StructDecl | _: EnumDecl | _: TraitDecl | _: ImplDecl | _: ExternDecl |
        _: ExternVarDecl | _: TypeDecl =>
      err("structs, enums, traits, impls, externs, and types may only be declared at the top level")

    // `static` names the one distinction a *file's* top level draws — module member or body-local —
    // and an inner block has no such choice to make: everything in one is local to it.
    case _: StaticDecl =>
      err("'static' marks a declaration at the top of the file the program starts in, saying it " +
        "belongs to the module rather than to that file's body. A declaration inside a block is " +
        "local to that block and there is no module member for it to be instead")

    // The leading clauses of a function body are split off before the body is analyzed, so any
    // that reach here sit after another statement or inside an inner block — both disallowed.
    case _: Require | _: Ensure =>
      err("'require'/'ensure' clauses must come before any other statement in a function body")

    // Split off at the head of a loop body and at the head of a function's, so one reaching here was
    // written after a statement or in a block that is neither. The two get different sentences
    // because the mistakes are different: an `invariant` has one place and a `variant` has two.
    case _: Invariant =>
      err("an 'invariant' belongs at the head of a loop's body — it is a condition that holds on " +
        "every entry to that body, and one written after some of the work has already been done " +
        "is not that. A condition over a struct's fields is written in the struct, and one over a " +
        "function's arguments is a 'require'")

    case _: Variant =>
      err("a 'variant' belongs at the head of a loop's body, where it is what decreases from one " +
        "iteration to the next, or in a function's contract block, where it is what decreases at " +
        "each recursive call")

    /* `@assert` reaches here in the entry file, whose top level is a body (`reference/modules.md § Where a program starts`) rather than a
     * run of declarations — so this is the same assert a module file hoists, met as a statement.
     *
     * **It is settled here rather than deferred**, and the two paths do not overlap: a module file's
     * asserts are declarations, are collected by hoisting, and are checked by the walk over
     * `assertDecls`; one written in the entry file is a statement, is never hoisted, and would
     * otherwise be checked by nobody. Both settle after every constant has folded, which is the one
     * ordering the condition needs — it may name a constant declared below it.
     *
     * It emits nothing either way: a true assertion is not code, and a false one has already
     * stopped the compilation.
     *
     * **`tsubst` is what makes one inside a generic mean anything** (`reference/generics.md §
     * Monomorphization`). A body is analyzed once per instantiation with its parameters bound to
     * the arguments, so the condition is settled against the types that were actually chosen —
     * which is the only moment `sizeof(T)` is a number. Folding against an empty map instead
     * reported `T` as an unknown type, about a parameter declared a line above.
     */
    case a: AssertDecl =>
      checkAssert(a, tsubst)
      Nil

}
