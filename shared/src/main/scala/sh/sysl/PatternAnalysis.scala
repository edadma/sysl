package sh.sysl

import scala.collection.mutable

/** Match arms and patterns: turning each arm and pattern into its typed form, and checking that
 * a match is exhaustive. Pattern tests are pure value reads, so the exhaustiveness rule is the
 * only semantic subtlety — an enum match must cover every variant or carry a catch-all, while a
 * scalar match need only be exhaustive when it is used for a value.
 */
trait PatternAnalysis extends TypeResolution {

  /** Analyzes one arm in its own scope so pattern bindings are visible to the guard and body.
   * Alternatives (`a | b`) may not bind, since the body cannot know which alternative matched.
   */
  protected def analyzeArm(scrutTy: Type, arm: MatchArm, expected: Option[Type], discarded: Boolean = false): TArm =
    at(arm.pos)(analyzeArmAt(scrutTy, arm, expected, discarded))

  private def analyzeArmAt(scrutTy: Type, arm: MatchArm, expected: Option[Type], discarded: Boolean): TArm = {
    pushScope()
    // The arm's scope closes however the arm ends, so an arm read speculatively — tried without an
    // expectation, to find out whether it can stand alone — leaves none of its bindings behind.
    try
      val tpats = arm.patterns.map(analyzePattern(_, scrutTy))
      if tpats.length > 1 && tpats.exists(binds) then
        err("alternative patterns joined by '|' cannot bind a name")
      val tguard = arm.guard.map(analyzeBool)
      val tbody  = analyzeBlockBody(arm.body, expected, discarded)
      TArm(tpats, tguard, tbody)
    finally popScope()
  }

  /** The names **this** pattern has bound so far, and whether one is already being walked.
   *
   * A pattern is one binding form however deeply it nests, so a name written twice inside it is a
   * name written twice — not a shadow, which is what an inner scope would have made of it. The
   * ordinary scope machinery cannot see that on its own: every binding declares into the arm's one
   * scope, and a second `declare` of a name is how shadowing legitimately works everywhere else.
   *
   * `Rect(v, v)` compiled before this and quietly bound the second, which reads like a test that the
   * two fields are equal — and is not one. Scala, Rust, OCaml and Haskell all refuse it for that
   * reason; a pattern that wants two things equal says so in a guard.
   */
  private val boundInPattern = mutable.Set.empty[String]
  private var walkingPattern = false

  /** Turns one pattern into its typed form, declaring any bindings into the current scope. A
   * bare name is a nullary-variant pattern when it names a variant of the scrutinee's enum, and
   * a binding otherwise.
   *
   * The outermost call is where a pattern's own set of bound names starts, and every call inside it
   * goes straight through — which is what makes "twice in one pattern" a question this can answer,
   * and keeps each alternative of an `a | b` arm its own pattern.
   */
  protected def analyzePattern(p: Pattern, ty: Type): TPattern =
    if walkingPattern then analyzePatternIn(p, ty)
    else
      walkingPattern = true
      boundInPattern.clear()
      try analyzePatternIn(p, ty)
      finally walkingPattern = false

  /** The name a binding introduces, refused where this pattern has already introduced it.
   *
   * It is written once, which is `declarePattern`'s subject: a pattern hands over a copy of the part
   * it matched, so a write to the name would be discarded where the arm ends.
   */
  private def bindOnce(name: String, ty: Type): String =
    if !boundInPattern.add(name) then
      err(s"'$name' is bound twice in one pattern, and the second would quietly stand for a " +
        "different part of the value — rename it, or compare the two in a guard")
    else declarePattern(name, ty)

  private def analyzePatternIn(p: Pattern, ty: Type): TPattern = p match
    case WildcardPattern => TWildPattern(ty)

    /* `n @ pat` — the inner pattern against the same value, and the name bound to the whole of it.
     *
     * The outer name is declared **before** the inner pattern is walked, which is the order a reader
     * meets them in: in `v @ Some(v)` the second `v` is the one that repeats, and declaring in
     * writing order is what makes the ordinary duplicate rule say so about that one. A binding is
     * not a test, so nothing about the arm's coverage changes.
     */
    case BindPattern(name, inner) => TAtPattern(bindOnce(name, ty), analyzePattern(inner, ty))

    case LitPattern(v) =>
      val t = analyzeExpr(v, Some(ty))
      if t.ty != ty then err(s"pattern is ${show(t.ty)} but the value is ${show(ty)}")
      // A literal pattern matches by equality, so any equatable type may carry one — the ordered
      // scalars and `bool`. `bool`'s two literals also make it exhaustive on their own (below).
      if !Type.isEquatable(ty) then err(s"a ${show(ty)} value cannot be matched against a literal")
      TLitPattern(t)

    case RangePattern(lo, hi, inclusive) =>
      // A range is between two endpoints of a discrete, numerically-ordered value — the integers,
      // the floats, and `char`. `string` is ordered too, but a `"a".."z"` range reads as a
      // character range while quietly matching every string whose bytes sort between them, so it is
      // excluded rather than left as a surprise.
      if !(Type.isNumeric(ty) || ty == Type.Char) then
        err(s"a range pattern needs a numeric or char value, not ${show(ty)}")
      val tl = analyzeExpr(lo, Some(ty))
      val th = analyzeExpr(hi, Some(ty))
      if tl.ty != ty || th.ty != ty then err(s"range pattern must match the ${show(ty)} value")
      TRangePattern(tl, th, inclusive)

    // A name in a pattern binds only where nothing answers to it. A variant is asked for first, then
    // a constant — which is what keeps `reference/modules.md § const — a value`'s constant patterns off the trap Rust documents,
    // where a `const` in a pattern quietly binds instead of matching. There is no second resolution
    // here to disagree with the first: a constant joins the one a variant already went through.
    // A qualifier is read the same way a variant pattern's is: the scrutinee's type already settled
    // which enum is meant, so the prefix is dropped. It cannot be a binding, though — a name with a
    // dot in it is not a name a program can declare — so a qualified pattern that resolves to
    // nothing is a mistake rather than a new local.
    case IdentPattern(written) =>
      val name = written.substring(written.lastIndexOf('.') + 1)

      ty match
        case en: Type.Enum if en.variant(name).exists(_.fields.isEmpty) =>
          TVariantPattern(en, en.variant(name).get, Nil)
        case en: Type.Enum if en.variant(name).isDefined =>
          err(s"variant '$name' carries data — match it as '$name(…)'")
        case _ =>
          constKey(written) match
            case Some(key) => analyzePattern(LitPattern(constLiteral(key)), ty)
            // A `val` is storage read while running, so there is no value here to compare against —
            // and letting the name bind instead is precisely the trap the paragraph above says
            // cannot arise. It is refused rather than quietly shadowed, which leaves the reader the
            // choice of a different name or an equality test.
            case None if globalKey(written).isDefined =>
              err(s"'$written' is a 'val', which is read while the program runs, so a bare name here " +
                s"would bind rather than match — write it as '`$name`' to test the value it holds, " +
                s"or bind a different name")
            // An `extern` variable is storage too, and storage the linker fills — so there is even
            // less of a value here to match against than a `val` offers.
            case None if externVarKey(written).isDefined =>
              err(s"'$written' is an 'extern' variable, which is read while the program runs, so a " +
                s"bare name here would bind rather than match — write it as '`$name`' to test the " +
                s"value it holds, or bind a different name")
            case None if written != name =>
              err(s"'$written' names no variant of ${show(ty)} and no constant, and a qualified " +
                s"name cannot bind — write the name a program can declare")
            case None => TBindPattern(bindOnce(name, ty), ty)

    /* `` `name` `` — the quoting says a reference was meant, so this never binds.
     *
     * The three answers are the three kinds of thing a name can stand for here. A nullary variant
     * is the same test the bare form gives, and quoting it is merely explicit. A `const` folds to
     * the literal it always did. Everything else is *storage* — a `val`, an `extern` variable, a
     * local, a parameter — and becomes an ordinary equality against whatever it holds when the
     * match runs; that is a test rather than a shape, so it tells exhaustiveness nothing and the
     * arm still needs a catch-all after it.
     *
     * A name that resolves to nothing is a diagnostic and not a new local, which is the reading a
     * qualified name already gets above: the quoting has said this is a reference, so failing to
     * find one is a mistake rather than an invitation to declare it.
     */
    case EqPattern(written) =>
      val name = written.substring(written.lastIndexOf('.') + 1)

      ty match
        case en: Type.Enum if en.variant(name).exists(_.fields.isEmpty) =>
          TVariantPattern(en, en.variant(name).get, Nil)
        case en: Type.Enum if en.variant(name).isDefined =>
          err(s"variant '$name' carries data — match it as '$name(…)'")
        case _ =>
          constKey(written) match
            case Some(key) => analyzePattern(LitPattern(constLiteral(key)), ty)
            case None =>
              val t = analyzeExpr(Ident(written), Some(ty))

              if t.ty != ty then err(s"pattern is ${show(t.ty)} but the value is ${show(ty)}")
              if !Type.isEquatable(ty) then
                err(s"a ${show(ty)} value cannot be matched against a name — there is no '==' to test it with")
              TLitPattern(t)

    // A variant is named within the scrutinee's own enum, so a module prefix on the pattern is
    // repeating what the value already settled — it is dropped, and the type is what decides.
    case VariantPattern(written, args) =>
      val name = written.substring(written.lastIndexOf('.') + 1)

      ty match
        case en: Type.Enum =>
          en.variant(name) match
            // The same advice the construction gives, and it is worded the same way for the same
            // reason: naming the spelling that was just refused is no advice at all (card `0220`).
            case Some(v) if v.fields.isEmpty =>
              err(s"variant '$name' carries nothing, so it is matched as a name on its own — drop " +
                s"the parentheses and the ${quantity(args.length, "sub-pattern")} inside them")
            case Some(v) =>
              if args.length != v.fields.length then
                err(s"variant '$name' has ${quantity(v.fields.length, "field")}, " +
                  s"but ${supplied(args.length, "sub-pattern")}")
              TVariantPattern(en, v, args.zip(v.fields).map { case (a, (_, fty)) => analyzePattern(a, fty) })
            case None =>
              err(s"enum '${en.name}' has no variant '$name'")
        // The positional form destructures a struct's fields in declaration order, so it must name
        // every field — adding one to the struct then turns each positional match into a checked
        // to-do, the way a new enum variant does.
        case s: Type.Struct =>
          if !typeKey(written).contains(s.base) then
            err(s"'$written(…)' does not match a ${show(s)} value")
          if args.length != s.fields.length then
            err(s"struct '${qn(s.base)}' has ${quantity(s.fields.length, "field")}, " +
              s"but ${supplied(args.length, "sub-pattern")}")
          // Naming every field is reading every field, so the positional pattern needs each of them
          // visible — the same rule the positional constructor is held to (`reference/modules.md §
          // Visibility`).
          checkEveryFieldVisible(s.base, s.fields.map(_._1), "this pattern",
            "match the fields it does offer by name, as 'Name{…}'")
          TStructPattern(s, args.zip(s.fields).map { case (a, (_, fty)) => analyzePattern(a, fty) })
        case other =>
          err(s"'$name(…)' matches an enum variant or a struct, but the value is ${show(other)}" + heldBehind(other))

    // A tuple pattern is the positional struct pattern with the name off (`reference/types.md §
    // Tuples`), which is why it ends as the same node: a tuple's parts are its fields, and matching
    // every one of them is what the positional form already means.
    case TuplePattern(args) =>
      ty match
        case t: Type.Tuple if args.length == t.fields.length =>
          TStructPattern(t, args.zip(t.fields).map { case (a, (_, fty)) => analyzePattern(a, fty) })
        case t: Type.Tuple =>
          err(s"${show(t)} has ${quantity(t.fields.length, "part")}, but " +
            s"${supplied(args.length, "sub-pattern")}")
        case other =>
          err(s"'(…)' matches a tuple, but the value is ${show(other)}" + heldBehind(other))

    // The named form matches a struct by field name, in any order, and may leave fields unlisted —
    // those are unconstrained. Both source forms end as one `TStructPattern` with a sub-pattern per
    // field in declaration order, a wildcard filling any field the pattern did not mention.
    case StructPattern(name, fieldPats) =>
      ty match
        case s: Type.Struct =>
          if !typeKey(name).contains(s.base) then err(s"'$name{…}' does not match a ${show(s)} value")
          fieldPats.map(_._1).groupBy(identity).collectFirst { case (n, xs) if xs.size > 1 => n }.foreach { n =>
            err(s"field '$n' is matched more than once")
          }
          for (fname, _) <- fieldPats do
            if !s.fields.exists(_._1 == fname) then err(s"struct '${qn(s.base)}' has no field '$fname'")
            else checkFieldVisible(s.base, fname)
          val byName = fieldPats.toMap
          TStructPattern(
            s,
            s.fields.map { case (fname, fty) =>
              byName.get(fname).map(analyzePattern(_, fty)).getOrElse(TWildPattern(fty))
            },
          )
        case other =>
          err(s"'$name{…}' matches a struct, but the value is ${show(other)}" + heldBehind(other))

  /** What a "this pattern does not fit that value" complaint adds when the value is the right shape
   * held behind a memory mode.
   *
   * **Selection is the only implicit dereference** (`03`), and deliberately: `p.x` reaches through a
   * `*T` or a `&T`, and a pattern does not, so a recursive type — which has to be held behind one to
   * exist at all — is matched by writing the dereference. That is a rule a reader has to know before
   * they can guess the fix, and the fix is one character, so the one place it bites is where it is
   * worth saying.
   */
  private def heldBehind(t: Type): String = t match
    case Type.Ptr(inner) if matchable(inner)    => hint
    case Type.Ref(inner, _) if matchable(inner) => hint
    case _                                      => ""

  private def hint: String =
    " — a pattern does not reach through a memory mode the way a field selection does, so match what " +
      "it holds: '*x match'"

  private def matchable(t: Type): Boolean = t match
    case _: Type.Enum | _: Type.Struct => true
    case _                             => false

  /** Whether a pattern can fail — false for a wildcard, a binding, or a struct whose every field's
   * sub-pattern is itself irrefutable, since a struct has one form and nothing to test a tag
   * against. A variant pattern is always refutable even on a one-variant enum: the tag is read and
   * compared either way, and an enum gaining a variant should not change what an existing pattern
   * means.
   *
   * Used where a test that cannot fail is a mistake rather than a tautology to fold away — the
   * condition of an `if` or a `while` (`reference/expressions.md § is — a pattern where a condition
   * is wanted`).
   */
  protected def refutable(p: TPattern): Boolean = p match
    case _: TWildPattern | _: TBindPattern => false
    // A binding never rules a value in or out, so `n @ pat` can fail exactly where `pat` can. That
    // is what makes `var n @ Point{x, y} = p` an irrefutable binding rather than a refused test.
    case a: TAtPattern                     => refutable(a.inner)
    case s: TStructPattern                 => s.args.exists(refutable)
    case _                                 => true

  /** Whether a pattern binds any name (directly or inside a variant's or struct's sub-patterns). */
  protected def binds(p: TPattern): Boolean = p match
    case _: TBindPattern    => true
    case _: TAtPattern      => true
    case v: TVariantPattern => v.args.exists(binds)
    case s: TStructPattern  => s.args.exists(binds)
    case _                  => false

  /** Checks exhaustiveness and returns the value type of a match (`unit` unless every arm
   * yields the same non-unit type). An enum match must cover every value or carry a
   * catch-all; a scalar match need only be exhaustive when it is used for a value.
   */
  protected def matchResultType(scrutTy: Type, arms: List[TArm]): Type = {
    val bodyTys = arms.map(_.body.ty).distinct

    // An arm that does not finish — one that aborts rather than yielding — constrains nothing, so
    // it is set aside before the others are compared. That is what lets `None -> exit(1)` sit
    // beside `Some(v) -> v` and the match still have the payload's type.
    val reached = bodyTys.filterNot(_ == Type.Never)

    // Two arms that each yield a value cannot yield *different* value types — there is nothing to
    // unify them to. Diagnosed like an `if` with mismatched branches rather than collapsed to a
    // silent `unit`, which would hide the mistake. A value/unit mix is the "not every arm yields"
    // case and stays `unit`, exactly as an `if` with no `else` does.
    // The two views of one slice are the one pair of *known* types with a meeting point, and they
    // meet at the read-only one — the same rule `join` gives an `if`'s branches and `meetViews`
    // gives a pair of operands. Applied only where there were two to reconcile, so a lone `[]T` arm
    // stays the writable view it was.
    val yielded  = reached.filterNot(_ == Type.Unit)
    val met      = yielded.map(Type.constView).distinct
    val merged   = yielded.sizeIs > 1 && met.sizeIs == 1
    val settled  = if merged then reached.map(Type.constView).distinct else reached
    val valueTys = if merged then met else yielded

    if valueTys.size > 1 then
      err(s"match arms have different types: ${valueTys.map(show).mkString(" and ")}")
    val valueTy =
      if settled.isEmpty then Type.Never
      else if settled.size == 1 && settled.head != Type.Unit then settled.head
      else Type.Unit

    // An enum match is checked wherever it stands, because falling off the end of one has no
    // defined result even for effect; every other scrutinee is checked only where the match is
    // used for a value, since the unmatched case of a statement match is a no-op.
    val checked = scrutTy match
      case _: Type.Enum => true
      case _            => valueTy != Type.Unit

    // Coverage is a question about values and the arms answer it together, so it is computed over
    // all of them at once (`Exhaustiveness`) rather than arm by arm. What differs per scrutinee is
    // what the answer can usefully be called.
    if checked then
      Exhaustiveness.gap(scrutTy, arms).foreach { g =>
        // `bool` is a closed two-value type, so covering both `true` and `false` with unguarded
        // arms is exhaustive on its own — and naming the one that is missing tells the reader less
        // than saying what the type needs.
        if scrutTy == Type.Bool then
          err("a 'match' on 'bool' that yields a value must cover both 'true' and 'false' — add the missing arm or an 'else'")

        // A gap worth naming: an enum's, which is the to-do list a closed sum type is for, or any
        // other whose names are the whole of what is missing.
        else if scrutTy.isInstanceOf[Type.Enum] || g.exact then
          err(s"match on '${show(scrutTy)}' is not exhaustive; missing ${Exhaustiveness.describe(g.names)} " +
            s"(add an 'else' arm)")

        // Otherwise the gap is a number's or a string's complement, which has no pattern to name
        // it, so the complaint says only that there is one.
        else err("a 'match' that yields a value must be exhaustive — add an 'else' arm")
      }

    valueTy
  }
}
