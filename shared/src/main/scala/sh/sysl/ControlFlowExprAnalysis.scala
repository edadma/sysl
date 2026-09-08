package sh.sysl

/** Control flow **as an expression** (`reference/statements.md`), plus the three forms that carry
 * several values at once.
 *
 * Every one of these yields a value, which is the whole reason they are analyzed here rather than in
 * `StmtAnalysis`: an `if` in statement position and an `if` on the right of a `var` are the same
 * form asked a different question, and the difference is carried by `expected` and `discarded`
 * rather than by two grammars. A loop yields what its `break` carried and `unit` when it finishes on
 * its own, so a loop is a value too.
 *
 * Split out of `ExprAnalysis`, whose expression dispatch calls in here.
 */
trait ControlFlowExprAnalysis extends ExprSupport {

  /** A branch, a loop, or a form that yields more than one value. The parameter names them all, so
   * that the match below is exhaustive over exactly what the dispatch sends here.
   */
  protected def controlExpr(
      expr: IfExpr | MatchExpr | While | DoWhile | Loop | CFor | For | ConstFor | Quantifier |
        TryExpr | RangeExpr | ResultList | Lambda | Tuple | Block,
      expected: Option[Type],
      discarded: Boolean,
  ): TExpr = expr match
    // An indented block under a binding's `=`. It is a value block like a branch's, asked the same
    // question by the same context, and its scope closes with it — a name bound inside is the
    // block's, exactly as one bound in a branch is the branch's.
    case Block(stmts) =>
      TBlockExpr(analyzeValueBlock(stmts, expected, discarded))

    // An `if` whose own value is unused hands that down: each branch is a block in statement
    // position, so neither is asked what it yields and the two have nothing to disagree about.
    case IfExpr(cond, thenBody, elseOpt) =>
      // **A branch that has no type of its own takes its sibling's**, which is the tiering
      // `analyzeOperands` gives a binary operator's two sides and a range's two ends, reaching the
      // one place two positions have to agree and did not have it. Without it a bare literal falls
      // to `int` and the pair is refused for a difference the reader never wrote — over a `usize`,
      // `if n == 0 then 1 else n` needed the width said a second time, and the annotation it asked
      // for went on the declaration where it said nothing a reader wanted to know.
      //
      // **Only a branch that would otherwise be guessing is told anything**, so nothing that
      // resolves today resolves differently: where both branches know what they are, or where the
      // position already supplies a type, this is the analysis it always was. Two branches that
      // genuinely disagree still disagree, in the same words — what has gone is the case where one
      // of them never had an opinion.
      val elseGuesses = elseOpt.exists(guessing)
      val thenGuesses = guessing(thenBody)

      // The `else` leads when it is the branch that knows, which is the one case it is analyzed
      // before the condition. It reads nothing the condition binds — an `is` binding reaches the
      // *then* branch and no further, which is what the scope below is for — so it is the same
      // reading; what moves is only which of two broken branches is complained about first.
      val early =
        if expected.isEmpty && thenGuesses && !elseGuesses then
          elseOpt.map(analyzeValueBlock(_, None, discarded))
        else None

      // The condition's own scope wraps the condition and the *then* branch and nothing else, which
      // is the whole of what an `is` binding's reach has to be said about
      // (`reference/expressions.md § is — a pattern where a condition is wanted`). The `else` is
      // analyzed outside it, and so is an `elif` — the parser nests one into the else branch, so it
      // is already on the other side of this `popScope` and cannot read a name the test bound.
      //
      // It is taken as a function because the *then* side may have to be read twice: once to find
      // out whether it can stand alone, and once for real with what the `else` settled. The scope
      // closes either way, including on the reading that failed.
      def thenSide(want: Option[Type]): (List[TCondTerm], TBlock) = {
        pushScope()
        try (analyzeCond(cond), analyzeValueBlock(thenBody, want, discarded))
        finally popScope()
      }

      val thenWants = expected.orElse(early.flatMap(settles))

      // **A branch may have no type of its own for a reason `guessing` cannot see, and the sibling
      // rule is owed to that branch too.** A bare literal is the case that tier was written for: it
      // is recognised from the syntax, before anything is analyzed. A nullary generic call is the
      // other one and is invisible until it is tried — `buf()` has nothing in its argument list to
      // fix `T`, so it is solved from the expected type (`reference/generics.md § Inference is
      // bidirectional`) and simply fails where there is none. Under a declared result the `if`
      // already worked, because the result was that expectation; standing on its own it was refused
      // for a type the branch beside it knew all along.
      //
      // So a branch with nothing to go on is *tried*, and a branch that cannot stand alone is held
      // over until its sibling has spoken — which is what `analyzeOperands` does with an operand
      // that has no reading of its own, for the cost of one analysis rather than two. **Only a
      // branch that failed is told anything**: a reading that succeeded is kept exactly as it was,
      // registrations and all, so nothing that resolves today resolves differently.
      val (tc, tThen, settledElse) =
        if thenWants.isDefined || elseOpt.isEmpty then
          val (c, t) = thenSide(thenWants)
          (c, t, None)
        else
          attempt(thenSide(None)) match
            case Some((c, t)) => (c, t, None)
            case None         =>
              // The `else` leads, exactly as it does for a guessing *then* above. Where it cannot
              // stand alone either, neither branch settles anything and the second reading of the
              // *then* raises what the first one swallowed — the same words, at the branch the
              // reader wrote first.
              val sibling = elseOpt.flatMap(b => attempt(analyzeValueBlock(b, None, discarded)))
              val (c, t)  = thenSide(sibling.flatMap(settles))
              (c, t, sibling)

      val tElse = early.orElse(settledElse).orElse(elseOpt.map { b =>
        val want = expected.orElse(if elseGuesses && !thenGuesses then settles(tThen) else None)

        // The other order, and it needs the same held-over reading: with the *then* branch the one
        // that knows, an `else` that cannot stand alone takes what the *then* settled.
        if want.isDefined then analyzeValueBlock(b, want, discarded)
        else
          attempt(analyzeValueBlock(b, None, discarded))
            .getOrElse(analyzeValueBlock(b, settles(tThen), discarded))
      })
      // The branches meet at one type, and a branch that does not finish takes the other's. A
      // branch used only for its effect is a different thing: one `unit` branch makes the whole
      // `if` a statement, whose value is nobody's, exactly as a missing `else` does.
      val ty = tElse match
        case Some(eb) =>
          join(tThen.ty, eb.ty).getOrElse {
            if eb.ty == Type.Unit || tThen.ty == Type.Unit then Type.Unit
            else err(s"if branches have different types: ${show(tThen.ty)} and ${show(eb.ty)}")
          }
        case None => Type.Unit
      TIf(tc, tThen, tElse, ty)

    case MatchExpr(scrut, arms) =>
      val ts    = analyzeExpr(scrut)
      val tarms = analyzeArms(ts.ty, arms, expected, discarded)
      TMatch(ts, tarms, matchResultType(ts.ty, tarms))

    // A loop's `else` is a block like any other, so a loop in statement position discards it too.
    // Without that, Python's own idiom — walk, `break` on a hit, set a flag in the `else` when
    // nothing hit — would be refused for a disagreement between the flag and a bare `break`.
    case While(label, cond, body, elseOpt) =>
      // As with `if`: the condition and the body share one scope, so a binding the test made is the
      // body's to read and is remade each round. The `else` runs when the test finally fails, which
      // is the one path on which there is nothing bound, so it sits outside.
      pushScope()
      val tc            = analyzeCond(cond)
      val (tbody, ctx)  = analyzeLoopBody(expected, label)(analyzeStmts(body))
      popScope()
      val telse         = elseOpt.map(analyzeValueBlock(_, expected, discarded))
      checkedLoop(ctx, TWhile(tc, tbody, telse, loopResultType(ctx, telse)))

    // The body is analyzed first and in its own scope, which has closed by the time the test is
    // reached — so the foot's condition reads what the enclosing block holds and nothing the body
    // declared. That is C's rule, and it is also the only one the form can have: a `var` made in the
    // body is remade every round, and a test that read one would be reading the last round's.
    //
    // The condition is `analyzeBool` rather than `analyzeCond` for the reason the three-clause
    // `for`'s is: an `is` binding is live through the branch that the test guards, and a test at the
    // foot guards nothing — the body it belongs to has already run.
    case DoWhile(label, body, cond, elseOpt) =>
      val (tbody, ctx) = analyzeLoopBody(expected, label)(analyzeStmts(body))
      val tcond        = analyzeBool(cond)
      val telse        = elseOpt.map(analyzeValueBlock(_, expected, discarded))
      checkedLoop(ctx, TDoWhile(tbody, tcond, telse, loopResultType(ctx, telse)))

    case Loop(label, body) =>
      val (tbody, ctx) = analyzeLoopBody(expected, label)(analyzeStmts(body))
      checkedLoop(ctx, TLoop(tbody, endlessResultType(ctx)))

    // The init's binding belongs to the loop and to nothing outside it, so the scope opens before
    // the condition — which reads that binding — and closes after the `else`, which may too.
    case CFor(label, init, cond, step, body, elseOpt) =>
      pushScope()
      val tinit        = init.toList.flatMap(recoverStmt)
      val tcond        = cond.map(analyzeBool)
      val (tbody, ctx) = analyzeLoopBody(expected, label)(loopStmts(body))
      val tstep        = step.toList.flatMap(recoverStmt)
      val telse        = elseOpt.map(analyzeValueBlock(_, expected, discarded))
      popScope()
      // With no condition the loop cannot finish on its own, so its type is what its `break`s
      // carry, exactly as `loop`'s is — and an `else` that can never run is a mistake worth saying.
      if tcond.isEmpty && telse.isDefined then
        err("this 'for' has no condition, so it never finishes on its own and its 'else' cannot run")
      checkedLoop(ctx,
                  TCFor(tinit, tcond, tstep, tbody, telse,
                        if tcond.isEmpty then endlessResultType(ctx) else loopResultType(ctx, telse)))

    /** `for const i in 0..<A.len` — the loop the compiler **unrolls** (`reference/generics.md § A
     * parameter may stand for a list of types`).
     *
     * The body is analyzed once per value of the range, each copy on its own, with the name
     * standing at a `ConstArg` for the length of that copy — which is exactly what a value
     * parameter stands at (`reference/generics.md § A parameter may stand for a value`), so the
     * name folds into its uses through the machinery that already exists and `self.i` selects a
     * part through the same constant.
     *
     * **Analyzing each copy separately is the whole feature**, and it is why this cannot be a
     * desugaring into an ordinary `for`: the parts of a tuple have different types, so one written
     * line type-checks differently at each position and there is no single typed body to run.
     *
     * What comes out is a `TSeq` of the copies, so nothing downstream of the analyzer ever meets an
     * unrolled loop. It yields `unit` for the same reason: there is no loop for a `break` to carry a
     * value out of, and a range known at compile time has nothing to say by finishing.
     */
    case ConstFor(name, iter, body) =>
      val (lo, hi) = iter match
        case RangeExpr(Some(l), Some(h), inclusive) =>
          val ends =
            for
              a <- constInt(l, tsubst)
              b <- constInt(h, tsubst)
            yield (a, if inclusive then b else b - 1)

          ends.getOrElse(err("a 'for const' is unrolled, so its range must be known at compile " +
            "time — 'A.len' for a type pack, a 'const', or a literal. A range computed at run time " +
            "is what the ordinary 'for' walks"))

        case _ =>
          err("a 'for const' walks a range with both ends written, as '0..<A.len' — it is unrolled " +
            "into one copy of its body per value, so there is nothing else for it to iterate")

      if hi - lo > ConstFor.maxCopies then
        err(s"a 'for const' over ${hi - lo + 1} values would emit that many copies of its body, and " +
          s"${ConstFor.maxCopies} is the most one is unrolled to — a loop this long is a run-time " +
          "'for', which costs one copy whatever it counts to")

      val saved      = tsubst
      val savedLoops = loops
      val savedFlag  = inConstFor

      // The enclosing loops are **hidden** while the copies are analyzed, which is what stops an
      // unlabelled `break` in one of them from silently leaving a loop the `for const` sits inside.
      // A real loop written *within* the body pushes onto the emptied stack and works as it always
      // did; what is left with nothing to find is exactly the case that had to be refused.
      loops = Nil
      inConstFor = true

      try
        TSeq((lo to hi).toList.map { v =>
          // The name is a compile-time constant for this copy and nothing else is: a pack's own
          // binding stays, since `A.len` is what the range was counted from and the copies do not
          // change it.
          tsubst = saved + (name -> Type.ConstArg(v, Type.usize))
          TBlockExpr(analyzeValueBlock(body, None, discarded = true))
        })
      finally
        tsubst = saved
        loops = savedLoops
        inConstFor = savedFlag

    case For(label, name, iter, body, elseOpt) =>
      iter match
        // `for i in T::Range` iterates a constrained integer subtype's range, `First` through `Last`
        // inclusive — the one place `::Range` is meaningful.
        // An alias is in the same table and is not one of these: it declares no range, so it falls
        // through to the ordinary attribute path and is refused there in the base's own words.
        case TypeAttr(Ident(tn), "Range") if lookupOpt(tn).isEmpty &&
            typeKey(tn).exists(k => constrainedDecls.contains(k) && !plainAlias(k)) =>
          val c = resolveConstrained(typeKey(tn).get)
          val i = c.base match
            case i: Type.Integer => i
            case other           => err(s"'${qn(typeKey(tn).get)}::Range' iterates an integer subtype, not ${show(other)}")
          val (lo, hi) = (c.lo, c.hi) match
            case (Some(l), Some(h)) => (l, h)
            case _                  => err(s"'${qn(typeKey(tn).get)}::Range' needs a 'within' range")
          val last      = if c.exclusiveHi then hi - 1 else hi
          pushScope()
          // The loop variable is a `T`, since what the range walks are the values of the subtype —
          // the bounds and the comparison stay at the base, which is what `T` is laid out as.
          val u         = declare(name, c)
          val (tb, ctx) = analyzeLoopBody(expected, label)(loopStmts(body))
          popScope()
          val telse     = elseOpt.map(analyzeValueBlock(_, expected, discarded))
          checkedLoop(ctx,
                      TFor(u, i, TIntLit(lo.toBigInt, i), TIntLit(last.toBigInt, i), inclusive = true, tb,
                           telse, loopResultType(ctx, telse)))

        case RangeExpr(Some(lo), Some(hi), inclusive) =>
          val List(tlo, thi) = analyzeOperands(List(lo, hi), None)
          if tlo.ty != thi.ty then
            err(s"a 'for' range needs matching bounds, got ${show(tlo.ty)} and ${show(thi.ty)}")
          val vty = tlo.ty match
            case i: Type.Integer => i
            case other           => err(s"a 'for' range iterates integer bounds, not ${show(other)}")
          pushScope()
          val u            = declare(name, vty)
          val (tb, ctx)    = analyzeLoopBody(expected, label)(loopStmts(body))
          popScope()
          val telse        = elseOpt.map(analyzeValueBlock(_, expected, discarded))
          checkedLoop(ctx, TFor(u, vty, tlo, thi, inclusive, tb, telse, loopResultType(ctx, telse)))

        case _ =>
          val seq = autoDeref(analyzeExpr(iter))
          seq.ty match
            case Type.Array(_, elem) => forEach(label, name, seq, elem, body, elseOpt, expected, discarded)
            case Type.Slice(elem, _) => forEach(label, name, seq, elem, body, elseOpt, expected, discarded)
            // A string has two granularities and no reason to prefer one silently, so which one
            // is wanted is written: `s.bytes` for the bytes, `s.chars` for the characters they
            // encode. Neither is the default, because a program that means one rarely means both.
            case Type.Str =>
              err("a string is iterated as 's.bytes' or 's.chars', " +
                "since a string has bytes and characters both")
            case ty if iterateElem(ty).isDefined =>
              iterating(label, name, seq, iterateElem(ty).get, body, elseOpt, expected, discarded)
            case other =>
              err(s"'for' iterates an integer range, an array, a slice, or a type that implements " +
                s"'${qn(Library.key("Iterate"))}', and ${show(other)} is none of those" +
                walkHint(other, name, iter))

    // `for all i in lo..hi do pred` (`reference/verification.md § for all and for some`). The range
    // is read exactly as a counted `for`'s is — same node, same two diagnostics — so the two forms
    // cannot come to disagree about what a range is. What it does not share is the loop machinery:
    // a quantifier has no `break` to meet a type at, so nothing here consults the loop context.
    case Quantifier(universal, name, iter, pred) =>
      val word = if universal then "for all" else "for some"

      iter match
        case RangeExpr(Some(lo), Some(hi), inclusive) =>
          val List(tlo, thi) = analyzeOperands(List(lo, hi), None)
          if tlo.ty != thi.ty then
            err(s"a '$word' range needs matching bounds, got ${show(tlo.ty)} and ${show(thi.ty)}")
          val vty = tlo.ty match
            case i: Type.Integer => i
            case other           => err(s"a '$word' range iterates integer bounds, not ${show(other)}")
          pushScope()
          val u  = declare(name, vty)
          val tp = analyzeBool(pred)
          popScope()
          TQuantifier(universal, u, vty, tlo, thi, inclusive, tp)

        case _ =>
          err(s"'$word' quantifies over an integer range, written 'lo..<hi' or 'lo..hi'")

    case TryExpr(e) =>
      analyzeTry(analyzeExpr(e))

    // A range **with both ends written** is a value: `sysl.Range[T]`, three fields, built here.
    //
    // The four positions that read a range as a *form* — a `for` header, a slice index, a `match`
    // pattern and a quantifier — never reach this arm, because each matches the `RangeExpr` node
    // itself and reads the bounds directly. That is what keeps the counted `for` a counter and a
    // comparison with no struct, no `Option` a step and no call in it; the value is for every other
    // position, where before there was nothing a range could be.
    //
    // **An open end stays a form.** `..`, `lo..` and `..hi` mean something in an index and nothing
    // on their own — what an absent bound *is* depends on what is being indexed — so they keep the
    // refusal, which now names the one place they are legal rather than all four.
    case RangeExpr(Some(lo), Some(hi), inclusive) =>
      val List(tlo, thi) = analyzeOperands(List(lo, hi), None)

      // The same two questions the `for` header asks of its bounds, in the same order and the same
      // words: a range value and a counted loop must not come to disagree about what a range is.
      if tlo.ty != thi.ty then
        err(s"a range needs matching bounds, got ${show(tlo.ty)} and ${show(thi.ty)}")
      val vty = tlo.ty match
        case i: Type.Integer => i
        case other           => err(s"a range runs between integer bounds, not ${show(other)}")

      val s = instantiateStruct(Library.key("Range"), List(vty))

      TStructNew(s, List(tlo, thi, TBoolLit(inclusive)))

    case _: RangeExpr =>
      err("a range with an open end is only allowed in a slice index — as a value it would have to " +
        "say what the missing bound is, and only the thing being indexed knows that. Write both " +
        "ends")

    // `a, b` where a function's result list is what is being produced. It builds the aggregate the
    // caller takes apart — the same one a tuple builds, since a result list is a tuple's layout
    // without a tuple's type.
    case ResultList(values) =>
      if !retIsList then
        err("several values separated by commas are a function's result list, and this function " +
          "declares one result — write the values it wants, or declare a result list")

      val want = retTy.asInstanceOf[Type.Tuple]

      if values.length != want.targs.length then
        err(s"this function yields ${quantity(want.targs.length, "result")}, but " +
          s"${supplied(values.length, "value")}")

      val ts = values.zip(want.targs).map((v, w) => analyzeExpr(v, Some(w)))

      for ((t, w) <- ts.zip(want.targs)) do
        if disagree(t.ty, w) then
          at(t.pos)(err(s"this result is declared ${show(w)}, but the value is ${show(t.ty)}"))

      TStructNew(want, ts)

    case l: Lambda => analyzeLambda(l, expected)

    // `(a, b)` — a tuple, built exactly as a struct is: the parts are the fields, in the order they
    // were written. What each part is *wanted* at comes from the tuple being asked for, which is
    // what lets `var p: (i8, i8) = (1, 2)` narrow its literals the way a struct's fields do.
    case Tuple(elems) =>
      // A function declaring a result list yields several things and not one tuple
      // (`reference/declarations.md § Several results`), so the parentheses are refused where they
      // would build the carrier the form says never exists.
      if wantsResults(expected) then
        err(s"this function yields ${quantity(elems.length, "result")} rather than a tuple — " +
          s"write the values without the parentheses")

      val wanted = expected.map(Type.underlying) match
        case Some(t: Type.Tuple) if t.targs.length == elems.length => t.targs.map(Some(_))
        case _                                                     => elems.map(_ => None)

      val ts = elems.zip(wanted).map((e, w) => analyzeExpr(e, w))

      // A `unit` part is let through for the reason a `unit` field is (`reference/types.md § unit
      // and never`): the layout skips it. `never` is refused for the reason it is refused
      // everywhere but a result — a part that is never produced is a part nothing can give the
      // tuple.
      for t <- ts do
        if t.ty == Type.Never then
          at(t.pos)(err("a tuple part has to be a value, and this expression never produces one"))

      TStructNew(tupleType(ts.map(_.ty)), ts)

  /** An `if`'s or a `while`'s condition, as the `&&`-joined chain of terms it is
   * (`reference/expressions.md § is — a pattern where a condition is wanted`).
   *
   * The chain is flattened here rather than left as nested `Binary("&&", …)` because a term may
   * **bind**, and a binding's reach is "from its own `is` rightward" — which is a statement about
   * the flat sequence and not about a tree. Left-nesting would put `a is P && g` under the same node
   * whether `g` reads what `P` bound or not.
   *
   * Terms are analyzed left to right, in the scope the caller opened, so each `is` declares into the
   * scope the terms after it look names up in. Nothing else about a condition changes: a chain with
   * no `is` in it is the same expressions in the same order, split at the `&&` the emitter was going
   * to short-circuit at anyway.
   */
  protected def analyzeCond(e: Expr): List[TCondTerm] = conjuncts(e).map(condTerm)

  private def conjuncts(e: Expr): List[Expr] = e match
    case Binary("&&", l, r) => conjuncts(l) ::: conjuncts(r)
    case other              => List(other)

  private def condTerm(e: Expr): TCondTerm = e match
    case p: IsPattern => at(p.pos)(condIs(p))
    // Everything else is an ordinary boolean, and an `is` buried inside one is refused by
    // `analyzeExpr` — which is where the message lives, since it has to name every position rather
    // than the one this function happens to be looking at.
    case other => TCondTest(analyzeBool(other))

  private def condIs(p: IsPattern): TCondIs = {
    val subject = analyzeExpr(p.subject)

    if subject.ty == Type.Never then
      err("this expression never produces a value, so there is nothing here for a pattern to match")

    val tpats = p.patterns.map(analyzePattern(_, subject.ty))

    // A pattern that matches every value of the type asks a question with one answer. Refused rather
    // than folded away, because the two forms it takes are both a mistake worth naming: `x is n` is a
    // binding wearing a test's clothes, and a struct pattern with no refutable field is a
    // destructuring that belongs in the branch it was guarding. Among alternatives it is one of them
    // being irrefutable that decides it, since that one already answers for the rest.
    if !tpats.forall(refutable) then
      err(if p.negated then
            s"this pattern matches every ${show(subject.ty)}, so 'is not' is never true here"
          else
            s"this pattern matches every ${show(subject.ty)}, so the test is always true — " +
              s"take the value apart with 'match', or bind it with 'var'")

    // Alternatives share one answer, so the branch cannot know which of them matched — the same
    // rule an arm's alternatives are held to (`reference/patterns.md § The pattern forms`), said in
    // the words this position needs.
    if tpats.length > 1 && tpats.exists(binds) then
      err("alternative patterns joined by '|' cannot bind a name — the branch cannot know which of " +
        "them matched. Write '_' for the parts you are not naming")

    // `x is not Some(n)` would name `n` on the one path where nothing matched it. The binder is what
    // is refused, not the negation: `x is not Some(_)` is the early-exit guard the form is for.
    if p.negated && tpats.exists(binds) then
      err("a pattern under 'is not' cannot bind a name — the branch it guards is the one where it " +
        "did not match, so there would be nothing for the name to hold. Write '_' for the parts you " +
        "are not naming")

    TCondIs(subject, tpats, p.negated)
  }

  /** `for name in seq` over storage that is already there: each element is copied out by index, and
   * the sequence is evaluated once.
   */
  protected def forEach(label: Option[String], name: String, seq: TExpr, elem: Type, body: List[Stmt],
                      elseOpt: Option[List[Stmt]], expected: Option[Type], discarded: Boolean): TExpr = {
    pushScope()
    val u         = declare(name, elem)
    val (tb, ctx) = analyzeLoopBody(expected, label)(loopStmts(body))
    popScope()
    val telse     = elseOpt.map(analyzeValueBlock(_, expected, discarded))
    checkedLoop(ctx, TForEach(u, elem, seq, tb, telse, loopResultType(ctx, telse)))
  }

  /** `for name in cursor` over a sequence that has to be produced a value at a time
   * (`library/core.md § Walking a type of your own`).
   *
   * The cursor is the loop's own: the expression is evaluated once into a slot nothing outside the
   * loop can name, and `next` takes that slot's address, so a `Chars` or any other iterator advances
   * in place while whatever was written stays a value like every other. That the slot is a *copy* is
   * the ordinary value semantics — draining `for c in it` leaves an `it` the program declared
   * untouched, exactly as passing it to a function would.
   */
  protected def iterating(label: Option[String], name: String, seq: TExpr, elem: Type, body: List[Stmt],
                        elseOpt: Option[List[Stmt]], expected: Option[Type], discarded: Boolean): TExpr = {
    val cursor = freshName("iter")
    val step   = callMethodOn(TLoad(cursor, seq.ty), "next", Nil, None)
    val opt = step.ty match
      case e: Type.Enum if e.base == Library.key("Option") && e.targs == List(elem) => e
      case other =>
        err(s"'${qn(Library.key("Iterate"))}' asks its 'next' for an " +
          s"${show(Type.Enum(Library.key("Option"), List(elem)))}, " +
          s"and this one gives back ${show(other)}")

    pushScope()
    val u         = declare(name, elem)
    val (tb, ctx) = analyzeLoopBody(expected, label)(loopStmts(body))
    popScope()
    val telse     = elseOpt.map(analyzeValueBlock(_, expected, discarded))
    val bind      = TVariantPattern(opt, opt.variant("Some").get, List(TBindPattern(u, elem)))
    checkedLoop(ctx, TIterate(cursor, seq.ty, seq, step, bind, tb, telse, loopResultType(ctx, telse)))
  }

  /** The name `Iterate` gives the type it yields. It is an **associated** type rather than an
   * argument, so a type chooses it once and a `for` has nothing to disambiguate.
   */
  private val iterateItem = "Item"

  /** The member a `for` was reaching for, on a type that has one.
   *
   * A container is not itself a cursor here and is not meant to be: `sysl.buf.Buf` hands out a
   * `view()` of the elements it holds and every `sysl.container` type hands out a `walk()`. Both are
   * one call away from what a loop wants, and the refusal above named the trait without naming
   * either — so a reader who had written the obvious line had to go and find out that the road was
   * a method rather than a missing implementation.
   *
   * ==Read off the declaration rather than resolved==
   *
   * A member of a generic type has no signature until something instantiates it, and this runs on
   * the path where the compilation is about to stop — so the question is asked of what the member
   * was *written* as. A slice or an array is one syntactically; anything else is a cursor, and is
   * walkable exactly when its type implements `Iterate`, which is the same question the loop just
   * asked of the receiver and answered no to.
   *
   * Only a member the site could have called: no arguments, no type arguments of its own, and
   * visible from here. Naming one a reader may not write would be worse than naming nothing.
   */
  private def walkVia(ty: Type): List[String] = {
    val iterate = Library.key("Iterate")
    val owner   = ownerKey(ty)

    def cursorKey(written: String): Option[String] =
      resolveName(written, quiet = true)(n => structDecls.contains(n) || enumDecls.contains(n))
        .map(followAlias)

    def walkable(r: TypeRef): Boolean = r match
      case _: ArrayType    => true
      case NamedType(n, _) => cursorKey(n).exists(k => implsOf(iterate, k).nonEmpty)
      case _               => false

    memberDecls.toList.collect {
      case ((base, name), m)
          if base == owner && m.receiver.isDefined && !m.isProperty && m.params.isEmpty &&
            m.tparams.isEmpty && m.retType.exists(walkable) &&
            visible(memberAccessKey(base, name)) =>
        name
    }
  }

  /** `walkVia`'s answer as the tail of the refusal, and the loop as it should have been written.
   *
   * The worked line is offered only where the subject is a plain name, which is what a loop over a
   * container almost always writes. Anything else would have to be reproduced from the tree to be
   * shown, and a spelling the reader did not write is worse than the sentence alone.
   */
  private def walkHint(ty: Type, name: String, iter: Expr): String =
    walkVia(ty) match
      case Nil => ""
      case roads =>
        val named = roads.map(r => s"'$r()'").mkString(" and ")
        val shown = iter match
          case Ident(subject) => s", so 'for $name in $subject.${roads.head}()' is the loop"
          case _              => ""

        s" — what it does have is $named, which answers with something a 'for' walks$shown"

  /** What a type's `Iterate` implementation yields, or `None` where it has none.
   *
   * The element is `Iterate`'s associated type, so it is *determined* by whatever is being walked
   * rather than selected at the loop: a type supplies one `Item`, and a second implementation of
   * `Iterate` is the duplicate it looks like rather than an ambiguity a `for` has to resolve. That
   * is the whole reason the element moved out of the trait's argument list — a signature generic
   * over what it walks could not name the element otherwise.
   *
   * The three subjects a loop can be handed each reach the answer their own way, and all three go
   * through `assocTypeOpt`: a **trait object** wrote the element into its own type
   * (`*Iterate[Item = string]`), a **type parameter** has it from the bound that licensed the walk,
   * and a **concrete type** has it from the implementation. What this adds on top is the membership
   * question — a type with an `Item` of some *other* trait's is not something a `for` may walk, and
   * without this it would be.
   */
  protected def iterateElem(ty: Type): Option[Type] = {
    val iterate = Library.key("Iterate")

    // Read out of the requirement closure in both cases, so that a trait *requiring* `Iterate` is
    // walked too — exactly as its members are reached.
    def declared(b: Type.Bound) = traitClosure(b, selfBinding(ty)).exists(_.name == iterate)

    Type.erasedTrait(ty) match
      // **The object is what holds the answer, not the pointer to it.** A `*Iterate[Item = string]`
      // has no implementation filed for it and needs none: the element is written into the object
      // type, which is the whole of what the binding buys, so the projection is asked of the trait
      // rather than of the mode wrapping it.
      case Some(tr) => Option.when(declared(tr.bound))(tr).flatMap(_.assoc(iterateItem))
      case None =>
        val reaches = ty match
          case a: Type.Abstract => a.bounds.exists(declared)
          case _                => implsOf(iterate, ownerKey(ty)).nonEmpty

        if reaches then assocTypeOpt(ty, iterateItem) else None
  }

  /** A match's arms, with the ones that have **no type of their own** told what the rest settled —
   * the `if` rule above, over as many alternatives as the form has.
   *
   * The arms that know go first and the guessing ones follow, which is the only reordering: each
   * arm is analyzed in its own scope, so what an arm binds was never visible to another and the
   * reading of every one of them is the same either way. The list is put back in source order
   * before it leaves, because exhaustiveness is a question about the arms as written.
   *
   * **The whole thing stands aside where the position already supplies a type.** With one expected
   * there is nothing for an arm to tell another and every arm is analyzed exactly as it was.
   *
   * An arm has no type of its own for either of the two reasons a branch does: it is a bare literal,
   * which is read from the syntax, or it is a call whose type arguments only the expected type can
   * fix, which is invisible until the arm is tried. So an arm that is not guessing is *tried*, and
   * one that cannot stand alone is held over with the guessing ones until a sibling has settled
   * something.
   */
  private def analyzeArms(
      scrutTy: Type,
      arms: List[MatchArm],
      expected: Option[Type],
      discarded: Boolean,
  ): List[TArm] = {
    if expected.isDefined then arms.map(analyzeArm(scrutTy, _, expected, discarded))
    else
      val own = arms.map(a =>
        if guessing(a.body) then None else attempt(analyzeArm(scrutTy, a, None, discarded)))

      // The first arm that settles anything is what the held-over ones are told. Where the arms that
      // know disagree among themselves this picks one of them, and `matchResultType` then reports
      // that disagreement — which is the complaint the reader is owed, rather than one about an
      // arm that only ever repeated what it was handed.
      val want = own.flatten.flatMap(t => settles(t.body)).headOption

      arms.zip(own).map((a, t) => t.getOrElse(analyzeArm(scrutTy, a, want, discarded)))
  }
}
