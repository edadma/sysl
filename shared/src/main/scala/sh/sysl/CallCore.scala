package sh.sysl

/** What every call has in common, whatever is being called: matching arguments against a parameter
 * list, counting them, solving a generic callee's type arguments out of them, and passing a variadic
 * tail.
 *
 * It sits at the bottom of the call layering because the three things above it — a method, an
 * operator, a constructor — are all a free call underneath. The analyzer lowers every member to a
 * function under the mangled name `Type.member`, so a method call is a free call whose first
 * argument is the receiver, and an operator is a method call whose name came from a token. That is
 * why the checking lives here once rather than three times.
 *
 * Argument checking runs **provisionally** first where a callee is generic: the arguments have to be
 * analyzed to solve the type parameters, and solving them is what says whether the arguments were
 * right. `probe` is what makes that recoverable — an attempt that fails leaves no diagnostic behind,
 * so an overload set can be tried without the failures reaching the programmer.
 */
trait CallCore extends Literals with TraitObjects with ArgumentBinding {

  /** Producing the receiver a method's `self` sigil asked for — by value, by address, or by
   * reference. Supplied by `CallAnalysis`, which is mixed in last: it is the one step every route to
   * a member ends in, and the routes are what the traits between here and there are.
   */
  protected def buildReceiver(mode: RecvMode, tr: TExpr, member: String = ""): TExpr

  /** The call trait a type turned out to implement, with the arguments it implements it at — which
   * is how a callable argument that has just been read says what it takes and what it yields.
   *
   * Supplied by `Closures`, mixed in after this, for the reason `buildReceiver` is: a closure's own
   * struct and the four other shapes a callable can arrive as are that layer's to tell apart.
   */
  protected def callableOf(t: Type): Option[Type.Bound]


  /** Type-checks positional arguments against a resolved parameter list. `pre` holds arguments
   * already analyzed during type-argument inference, so they are not analyzed twice.
   */
  protected def checkArgs(
      what: String,
      params: List[(String, Type)],
      args: List[Expr],
      pre: Option[List[TExpr]],
      positional: Boolean = false,
  ): List[TExpr] = {
    // An argument analyzed against a parameter that was still being solved was analyzed without an
    // expected type, so a value headed for a `&T` parameter is boxed here instead of at its own
    // analysis, and one headed for a trait object is erased here.
    //
    // A bare literal is the one case a coercion cannot repair, because a literal does not convert —
    // it *is* the type its position gives it (`01`). Where that position is a parameter mentioning
    // what was being solved, the type was not known until the solution was, so every literal
    // argument is analyzed **here**, against the parameter it turned out to have: `rotr(x, 7)` on
    // `rotr[T](x: T, n: T)` gives the `7` the `T` that the `x` settled. What inference held is a
    // stand-in carrying the literal's default type and nothing else, which is why this is
    // unconditional rather than a repair applied where the two disagree.
    //
    // `pre` may carry a receiver the argument list does not, so the two are aligned from the right.
    val ts = pre match
      case Some(provisional) =>
        val srcs = List.fill(provisional.length - args.length)(None) ::: args.map(Some(_))

        provisional.zip(srcs.padTo(provisional.length, None)).zip(params).map {
          case ((t, src), (_, pty)) =>
            if src.exists(isLiteral) || src.isDefined && becomesSlice(t.ty, pty) then
              analyzeExpr(src.get, Some(pty))
            else reread(coerce(t, pty), src, pty)
        }
      case None => args.zip(params).map { case (a, (_, pty)) => analyzeExpr(a, Some(pty)) }

    // A **written-out array standing where a slice is asked for**, which for a literal is a
    // conversion the *analysis* performs rather than one `coerce` repairs afterwards: `[1, 2, 3]`
    // becomes a slice because the position it was written in said so, and by the time there is a
    // `[3]int` to coerce the storage has already been made as an array. A non-generic call never
    // meets this — its parameter type is known, so the argument is analyzed against it in the first
    // place — and a generic one analyzed the argument before the solution existed, which is exactly
    // what this re-does.
    //
    // Asked of the two **types** rather than of the argument's shape, so a named array takes the
    // same road as a literal: re-analyzing `a` against `[]const int` reaches the coercion that
    // views it, which is what a non-generic call already does for `plain(a)`. Matching on the
    // syntax would have made the two forms differ here for a rule that does not distinguish them.

    // The complaint is about one argument, so it is reported where that argument is written
    // rather than at the call as a whole.
    //
    // A `unit` parameter is legal and costs nothing — it is zero-sized, so it is dropped from the
    // signature and the argument is evaluated for its effect alone. `never` is the one that cannot
    // be a parameter, and inference is the only place it can be caught: a generic parameter accepts
    // whatever the argument's type is, and an expression that never arrives is the one thing
    // inference must not conclude a slot from. A diverging argument against a *written* parameter
    // is untouched — it is dead code, and that parameter still has a layout.
    // A callable's parameters have no names a program wrote — the call trait's are the library's —
    // so what a mismatch there names is the *position*, which is the only thing the caller can see.
    for ((t, (pname, pty)), i) <- ts.zip(params).zipWithIndex do
      val which = if positional then s"the ${ordinal(i + 1)} argument of $what" else s"'$pname' of '$what'"

      at(t.pos):
        if disagree(t.ty, pty) then err(s"$which is ${show(pty)}, but ${show(t.ty)} was given")
        else if pty == Type.Never then err(s"cannot pass an expression that never returns as $which")

    ts
  }

  /** The arguments of a generic call, analyzed once for the inference that follows — each against
   * its parameter's type wherever that type is already known.
   *
   * A parameter naming none of the parameters being solved has nothing to contribute to the
   * solution and nothing left to wait for, so its argument is analyzed exactly as a non-generic
   * call's is. That is what keeps `01`'s rule — a parameter's type at a call fixes an unsuffixed
   * literal — true of a generic callee: `f[T](x: T, n: usize)` has no more to work out from the `7`
   * in `f(1u32, 7)` than a plain function has.
   *
   * A literal at a parameter that *does* name one is not analyzed at all: it stands in for its own
   * default type, `solve` consults it last, and `checkArgs` analyzes it once the parameter is a
   * type. The stand-in is a node for its type and nothing else, and never reaches the output.
   * Anything with a type of its own is analyzed bare there, because the type it would be checked
   * against is the thing being solved — and anything without one waits for the second pass.
   *
   * The parameter types are the declaration's, so they are resolved where it was written; the
   * arguments are the caller's, and are analyzed where *they* were written.
   *
   * **`seed` is what a caller already knows before any argument is read.** A free function knows
   * nothing — every one of its type parameters is answered by an argument — but a *member* is
   * reached through a receiver, and a receiver is a type that has its arguments already. Those
   * belong in the solution from the start rather than being waited for, and nothing in the call can
   * supply them: `Box[int].apply[U]` has no argument that mentions `T`, so a closure standing at
   * `&Fn(T) -> U` was held back for a `T` that was never coming.
   */
  protected def provisionalArgs(
      decl: String,
      tparams: List[String],
      ptypes: List[TypeRef],
      args: List[Expr],
      bounds: Map[String, List[BoundRef]] = Map.empty,
      seed: Map[String, Type] = Map.empty,
      result: Option[TypeRef] = None,
      expected: Option[Type] = None,
  ): List[TExpr] = {
    val tps  = tparams.toSet
    val want = inDecl(decl)(ptypes.map(r => Option.unless(mentions(r, tps))(resolveType(r, Map.empty))))
    val at   = args.zip(want.padTo(args.length, None))

    // A **callable** argument is the one shape that has no type of its own to be analyzed at: a
    // closure's parameters come from the context, and a bare function name is a callable only where
    // one is asked for. `null` is the third, written as a value rather than as code: it is an
    // address and nothing in it says of what. So the pass runs twice — everything else first, then
    // these against what the first pass has by then made concrete.
    //
    // Holding one back cannot lose the solution, and that is what makes the wait safe rather than
    // merely convenient: an argument with no type of its own has nothing to unify, so the parameter
    // it stands at is settled by the others or by nothing at all. `two(&x, null)` and
    // `two(null, &x)` therefore get one answer, which an ordering rule would not have given.
    // **Which arguments are callables is decided HERE, in the caller's scope, and carried to the
    // second pass rather than asked again there.** `callableArg` answers by looking a name up, and
    // the second pass runs under `inDecl(decl)` — so asked there, it is asking whether the *callee's*
    // module declares a function of that name. A function passed by name to a bare-arrow parameter
    // in another module was therefore held back as a callable by this pass and read as though it
    // were not one by the next, and reported that nothing here wanted a callable while the
    // parameter's own bound said otherwise.
    //
    // A closure literal is what hid it for so long: it is a callable in any scope, so the two passes
    // agreed about every argument that was not a bare name.
    val callable = at.map((a, _) => callableArg(a))

    // Each entry is the node read, if one was, and whether reading it took a **literal's default**
    // for want of anything better — which is what the ordering below turns on.
    //
    // A **construction over literals** answers that question the same way and is why the flag is
    // computed rather than written `false`: `Some(3)` read alone is an `Option[int]`, and the only
    // thing that decided the `int` was a literal with no width on it. See `adaptable`.
    val first: List[Option[(TExpr, Boolean)]] = at.zip(callable).map { case ((a, e), isCallable) =>
      if isCallable || e.isEmpty && (nullArg(a) || implicitArg(a)) then None
      else
        e match
          case Some(_) => Some(analyzeExpr(a, e) -> false)
          case None =>
            literalDefault(a) match
              case Some(ty) => Some(standIn(ty).setPos(a.pos) -> true)
              // **A fourth shape with no type of its own, and it is not one a syntax can name**:
              // anything whose bare analysis cannot get off the ground for want of the context it
              // was denied. `None` at a parameter of type `Option[T]` is the one that found this —
              // it is a generic construction whose own type argument nothing here settles, so
              // analyzing it alone raises where a `null` would merely have waited.
              //
              // Held back on the same argument as the three above, and it is the same argument: an
              // expression that could not be read has unified nothing, so the parameter it stands
              // at is settled by the others or by nothing at all, and the wait cannot lose the
              // solution. What it gains is the second pass, where the parameter is a type and the
              // argument is read against it exactly as a non-generic call reads it.
              //
              // `attempt` rather than `probe` because the node is kept where the analysis
              // succeeded, which is the ordinary case here and must cost one analysis rather than
              // two.
              case None => attempt(analyzeExpr(a)).map(t => t -> adaptable(a, t))
    }

    // What the first pass settles. `map[A, B](xs: []A, out: []B, f: A -> B)` gets both from the two
    // slices, and only then is `Fn(A) -> B` a thing a closure can be read against — which is why
    // this is a partial solution rather than the real one, made here and thrown away.
    //
    // It starts at what the caller already knew, so a member's receiver counts as settled before
    // the first argument is read. `unify` binds only a name it has not got (`GenericInstantiation`),
    // so a seeded one is never overwritten by an argument that happens to mention it — which is the
    // right way round: the receiver is the authority on its own arguments, and an argument that
    // disagrees is a type error `checkArgs` reports against the instantiated signature.
    val partial = scala.collection.mutable.Map.empty[String, Type] ++= seed

    // **The three tiers are `solve`'s, and they are here because a held-back argument is read
    // against this map rather than against that one.** What carries a type of its own settles a
    // parameter first, the expected type next, and a literal's default only where neither reached
    // it (`reference/generics.md § Inference is bidirectional`). Reading them in one pass instead
    // let a literal fix the parameter a closure was about to be analyzed at, so `val n: usize =
    // twice(0, a -> a + 1)` read its closure at `int` and then reported the result against the
    // binding — with the annotation that was supposed to answer the question sitting one line
    // above.
    //
    // `unify` writes only where the map is silent, so the order is the whole of the precedence.
    for case (r, Some((t, false))) <- ptypes.zip(first) do inDecl(decl)(unify(r, t.ty, tps, partial))

    if partial.size < tparams.length then
      for r <- result; e <- expected do inDecl(decl)(unify(r, e, tps, partial))

    if partial.size < tparams.length then
      for case (r, Some((t, true))) <- ptypes.zip(first) do inDecl(decl)(unify(r, t.ty, tps, partial))

    // **A held-back argument that turned out to be a callable joins the solution, so the held-back
    // arguments after it are read against what it settled.** The three tiers above are made of the
    // arguments that had a type of their own, and a callable never does — so a parameter named only
    // by an *arrow's* result was still unknown when the next arrow was read, and that next arrow was
    // told nothing said what it took by a call whose previous argument had just said so.
    //
    // `apply(x: int, first: int -> N, again: N -> N)` is the shape: `first` takes an `int`, so it is
    // readable, and its result is the whole of what `N` is. Reading it and throwing that away left
    // `again: N -> N` with an unsolved parameter in what it *takes*, which is the one thing
    // `callBound` requires to be settled.
    //
    // `unify` writes only where the map is silent, so this cannot overturn what the tiers decided —
    // it fills what none of them reached.
    //
    // **By READINESS rather than left to right, which is what keeps the declaration's order out of
    // the answer.** `heldWant` says whether an argument can be read at all — for a callable, whether
    // what it *takes* is settled — so each round reads the ones that can be and lets what they
    // settle reach the rest. Reading in source order instead makes
    // `f(g: T -> int, f: int -> T)` refuse a call that `f(f: int -> T, g: T -> int)` accepts, on two
    // parameters that differ in nothing but which was written first, and the page's own rule about
    // held-back arguments — *waiting is not queueing* — says that is the wrong shape of answer.
    //
    // It terminates because a round that reads nothing stops it, and every round that reads
    // something reads at least one argument that will not be offered again.
    val out = scala.collection.mutable.ArrayBuffer.from(first.map(_.map(_._1)))

    def expectedFor(i: Int): Option[Type] =
      inDecl(decl)(heldWant(callable(i), ptypes.lift(i), tps, bounds, partial.toMap))

    def read(i: Int): Unit = {
      val t = analyzeExpr(at(i)._1, expectedFor(i))

      out(i) = Some(t)

      for
        (_, taken, yielded) <- inDecl(decl)(callShape(ptypes.lift(i), tps, bounds))
        supplied            <- callableOf(t.ty)
        if supplied.args.length == taken.length + 1
      do
        inDecl(decl) {
          for (ref, ty) <- taken.zip(supplied.args) do unify(ref, ty, tps, partial)
          for ref <- yielded do unify(ref, supplied.args.last, tps, partial)
        }
    }

    var pending  = out.indices.filter(out(_).isEmpty).toList
    var progress = true

    while progress && pending.nonEmpty do
      val (ready, rest) = pending.partition(expectedFor(_).isDefined)

      progress = ready.nonEmpty
      ready.foreach(read)
      pending = rest

    // What never became readable is read in source order, so the refusal a reader gets is the
    // leftmost one rather than whichever round happened to end.
    pending.foreach(read)

    out.toList.map(_.get)
  }

  /** What an argument held back from the first pass is analyzed against, once the rest have been
   * read.
   *
   * A callable asks for the call trait its parameter's bound names. `null` and an implicit member
   * ask for the **parameter itself**, which is a type by now wherever the other arguments settled
   * what it mentions — so `two[T](a: *T, b: *T)` gives the `null` in `two(&x, null)` the `*int`
   * that the `&x` said, which is what the same call to a non-generic `two` has always done.
   *
   * `None` in either case where something the parameter names is still unknown, which leaves the
   * argument to report it: a closure that its parameters have no types, a `null` or a leading dot
   * that its context gave it none. `one[T](a: *T)` called `one(null)` is that — there is nothing else to read, and
   * the answer is the refusal it always was rather than a guess.
   */
  private def heldWant(
      isCallable: Boolean,
      ptype: Option[TypeRef],
      tps: Set[String],
      bounds: Map[String, List[BoundRef]],
      partial: Map[String, Type],
  ): Option[Type] =
    // **Asked of the callable rather than of the shapes that are not one**, which is the same
    // question read the other way round and is what lets a further kind of held-back argument
    // through without another case. A callable is the one thing here that wants its parameter's
    // **bound**; everything else held back wants the parameter *itself*, once the other arguments
    // have settled what it mentions.
    //
    // That covers `null`, a leading dot, and an argument whose own analysis could not be made at
    // all, on one reason: none of them carries a type, so what each needs is what the parameter
    // turned out to be. `same(Colour.red, .green)` reads its second argument against the `Colour`
    // the first said, `two(&x, null)` reads its second against `*int`, and `same(n, None)` reads
    // its second against the `Option[usize]` that `n` said.
    // **The answer is the caller's, taken before this pass entered the callee's scope.** Asking
    // `callableArg` here would ask the callee's module whether it declares the name.
    //
    // **A function name at a parameter that is not a callable bound falls through to the parameter
    // itself**, which is what `spawn(work, &n)` needs: its parameter is a raw `*extern` pointer, so
    // there is no bound to read and the answer to "what is wanted here" is that C pointer. Without
    // the fallback the expectation was nothing at all, and the refusal lost the sentence naming the
    // `&` that would have fixed it — the one thing a reader of that call actually needs. It costs
    // nothing where a bound *is* found, and where the parameter still mentions something unsolved
    // the filter below answers `None` exactly as it did before.
    if isCallable then callBound(ptype, tps, bounds, partial).orElse(plain(ptype, tps, partial))
    else plain(ptype, tps, partial)

  private def plain(ptype: Option[TypeRef], tps: Set[String], partial: Map[String, Type]): Option[Type] =
    ptype.filterNot(mentions(_, tps -- partial.keySet)).map(resolveType(_, partial))

  /** `null` written as an argument, which is the one *value* whose type its context supplies. */
  private def nullArg(a: Expr): Boolean = written(a) match
    case NullLit() => true
    case _         => false

  /** `.red` written as an argument — the other shape with no type of its own, and held back from the
   * first pass for `null`'s reason: there is nothing in it to unify, and the qualifier it needs is
   * the parameter type the *other* arguments settle.
   */
  private def implicitArg(a: Expr): Boolean = written(a) match
    case _: ImplicitMember | Call(_: ImplicitMember, _) => true
    case _                                              => false

  /** Whether an expression is one whose type the *context* has to supply (`reference/expressions.md
   * § Closures`, `§6`).
   *
   * The two shapes are the two ways of writing a callable that is not already a value: a literal,
   * whose parameters are typed by what asks for it, and the name of a declared function, which is
   * not a value at all until something says which call trait to build it into. Neither can be
   * analyzed first and converted afterwards, which is what separates them from every other
   * expression a converting context meets.
   */
  protected def callableArg(a: Expr): Boolean = written(a) match
    case _: Lambda => true
    case Ident(n)  => lookupOpt(n).isEmpty && funcKey(n).isDefined
    case _         => false

  /** What was actually written at an argument position, which for a **filled default** is inside
   * the wrapper the binding put around it (`reference/declarations.md § Default parameters and
   * named arguments`).
   *
   * Both questions above are about the *shape* of an expression, and a wrapper has a shape of its
   * own that answers no to each — so a default of a closure literal was neither held back for the
   * callable it is nor given the bound that says what it takes, and reported that its parameters had
   * no types. A default stands exactly where the argument would have been written, so what is asked
   * about it is what would have been asked about the expression written there.
   */
  private def written(a: Expr): Expr = a match
    case DefaultArg(_, e) => written(e)
    case _                => a

  /** The call trait a callable argument is being asked for, read off the bound of the parameter it
   * stands at, under whatever the other arguments have already settled.
   *
   * **Only what the closure TAKES has to be settled; what it yields may still be open.** That
   * asymmetry is the whole of this function and it follows from what a closure is: `analyzeLambda`
   * takes its parameter types from the context and its result from the body, so a result the
   * context does not know is not a difficulty — it is the ordinary case, and `Type.Unknown` is how
   * `fnParts` already says so.
   *
   * Requiring the result too made `map`'s shape unreadable, and circularly: in
   * `collect[T, U](xs: []const T, f: T -> U)` the only thing that can say what `U` is *is* the
   * closure, so waiting for `U` before reading the closure waits forever. What came out was
   * "'n' has no type here", which points at the closure and blames the caller for not annotating a
   * parameter the declaration had already stated. `collect([1, 2, 3], n -> s"<${n}>")` is that call.
   *
   * `None` where a **parameter** of the bound still names something nothing has determined, which
   * leaves the closure to report that its parameters have no types — the honest answer there, since
   * they have none.
   */
  private def callBound(
      ptype: Option[TypeRef],
      tps: Set[String],
      bounds: Map[String, List[BoundRef]],
      partial: Map[String, Type],
  ): Option[Type] =
    for
      (name, taken, yielded) <- callShape(ptype, tps, bounds)
      unsolved = tps -- partial.keySet
      if !taken.exists(r => mentions(r, unsolved))
      result = yielded
                 .filterNot(mentions(_, unsolved))
                 .map(r => resolveType(r, partial))
                 .getOrElse(Type.Unknown)
    yield Type.Trait(name, taken.map(r => resolveType(r, partial)) :+ result)

  /** The two ways a parameter can ask for a callable, read down to one shape: the trait's name, what
   * it takes, and what it yields.
   *
   * **A bare arrow and a boxed `&Fn` are the same question and were not being asked the same way.**
   * `reference/types.md § Function types`'s two spellings differ in what they *cost* — the arrow
   * becomes a bounded type parameter and monomorphizes, the boxed one is a trait object and
   * dispatches — and in nothing else that matters here, because both state a signature the closure
   * standing at them can be read against. Only the first was consulted, so `f[T](x: T, f: &Fn(T) ->
   * T)` could not read its closure even with `T` long since settled, and said the closure's
   * parameters had no types.
   *
   * A call trait's arguments are its parameters and then its result, which is why the result is the
   * last of them in the first case and is written where it stands in the second.
   */
  private def callShape(
      ptype: Option[TypeRef],
      tps: Set[String],
      bounds: Map[String, List[BoundRef]],
  ): Option[(String, List[TypeRef], Option[TypeRef])] = ptype.flatMap {
    case NamedType(n, Nil) if tps(n) =>
      bounds.getOrElse(n, Nil).find(b => Type.Fn.isCall(b.name)).map { ref =>
        (traitKey(ref.name).getOrElse(ref.name), ref.args.dropRight(1), ref.args.lastOption)
      }
    case r =>
      fnWritten(r).map { f =>
        val base = Type.Fn.base(f.params.length)

        (traitKey(base).getOrElse(base), f.params, Some(f.ret))
      }
  }

  /** The callable a parameter's own type names, behind whichever mode it was written with. */
  private def fnWritten(r: TypeRef): Option[FnType] = r match
    case f: FnType           => Some(f)
    case RefType(inner, _)   => fnWritten(inner)
    case PtrType(inner)      => fnWritten(inner)
    case _                   => None

  /** A node that carries a type and no value, for a literal inference reads before anything has
   * analyzed it. `checkArgs` replaces every one of them.
   */
  private def standIn(ty: Type): TExpr = ty match
    case _: Type.Floating => TFloatLit(0L, ty)
    case _                => TIntLit(0, ty)

  /** A call to a name, which may stand for one function or for several (`reference/declarations.md
   * § Overloading`).
   *
   * **A name declared once takes the same path it always did**, and that is worth stating as a
   * property rather than as an optimization: `overloadKeys` answers with the one key, the branch
   * below is not entered, and nothing about the analysis of the overwhelming majority of calls
   * changed when overloading arrived.
   *
   * **A candidate is chosen by trying the call against it**, rather than by a rule that compares
   * argument types to parameter types. That is the whole design here, and the reason is that the
   * comparison is not the small thing it sounds like: it would have to know about named arguments,
   * defaulted parameters, variadic tails, literal inference, generic solving, and every coercion a
   * parameter admits. All of that is `callFunction`, so asking `callFunction` is the only way to get
   * the same answer twice. A candidate the call does not fit is one that reported something, which
   * `probe` catches and drops.
   *
   * The cost is analyzing the arguments once per candidate, and it is paid only where a name is
   * overloaded at all.
   */
  protected def callOverloaded(
      plain: String,
      written: List[Expr],
      expected: Option[Type],
      targs: List[Expr] = Nil,
  ): TExpr = {
    // **A declaration this file may not name is not a candidate** (`reachableOverloads`), and it is
    // dropped here rather than anywhere later: a sibling file's `private` helper of this spelling
    // must not make the call ambiguous, must not be the one chosen for taking arguments no
    // reachable declaration takes, and must not be listed as something the name offers.
    val keys = reachableOverloads(plain)

    if keys.length == 1 then callFunction(funcDecls(keys.head), written, expected, targs)
    else
      val candidates = keys.map(funcDecls)
      val fitting    = candidates.flatMap(f => fitSignature(f, written, expected, targs).map(f -> _))

      narrow(fitting, written) match
        case List((one, _)) => callFunction(one, written, expected, targs)

        // Nothing fits. Where exactly one candidate could have taken this many arguments, its own
        // complaint is the useful message — the reader wrote a call meaning that one and got a type
        // wrong — so it is made again outside the sandbox and allowed to report. Otherwise the
        // mistake is about which function was meant, and the answer is the roster.
        case Nil =>
          // An argument carrying `Type.Unknown` was already refused where it was made, and it makes
          // *every* candidate fail — so the roster below would be printed with nothing eliminated,
          // listing declarations that plainly do take the call as written. That is the worst message
          // this could give: it says overload resolution rejected a call the reader can see matches,
          // and sends them to the wrong line. The first error is the one to fix, and it is already on
          // screen above this one.
          if written.exists(a => alreadyReported(analyzeExpr(a))) then poisoned()

          val plausible = candidates.filter(f => arityFits(f.params.length, f.variadic, written.length))

          if plausible.length == 1 then callFunction(plausible.head, written, expected, targs)
          else
            err(s"no '${qn(plain)}' takes these arguments — the declarations of that name are:\n" +
              candidates.map(f => s"    ${signatureOf(f)}").mkString("\n"))

        // Several fit, and the language does not guess. A literal is the usual cause: `0` is an
        // `int` and an `i64` and an `f64`, so a name declared at two of those is genuinely ambiguous
        // at a bare `0` and is not at `0i64`.
        case many =>
          err(s"'${qn(plain)}' is ambiguous here — ${many.length} of its declarations take these " +
            s"arguments:\n" + many.map((f, _) => s"    ${signatureOf(f)}").mkString("\n") +
            "\nAnnotate an argument, or name the type a literal is meant at")
  }

  /** The parameter types a candidate would take this call at, or `None` where it does not take it.
   *
   * It is one `probe` of `callFunction`, which is the same question `reference/declarations.md §
   * Overloading` says to ask — but the answer kept is the **signature the fit arrived at** rather
   * than only that there was one. For a generic candidate that is the signature of the
   * *instantiation*: `callFunction` ends at `funcInsts(name)` with `name` the instantiated key, so
   * the type arguments the call solved are already applied. That is what lets `narrow` ask a
   * generic candidate the same exactness question as an ordinary one, without being handed the
   * substitution or having to read coercion nodes back out of the tree.
   *
   * The instantiation the probe registered is dropped with everything else it did — `probe` is
   * `sandboxed` — so this reads the table inside the attempt and hands out types, which outlive it.
   */
  private def fitSignature(
      f: FuncDecl,
      written: List[Expr],
      expected: Option[Type],
      targs: List[Expr],
  ): Option[List[Type]] =
    probe {
      val TCall(inst, _, _, _) = callFunction(f, written, expected, targs): @unchecked
      funcInsts(inst)._1.map(_._2)
    }

  /** The tie-breaks applied to the candidates a call fits, in order, stopping as soon as one leaves
   * a single answer (`reference/declarations.md § Overloading`). Each candidate arrives with the
   * signature its fit arrived at, which for a generic one is the instantiation's — see
   * `fitSignature`.
   *
   * The first two are about **exactness**, and both exist because a call that fits two declarations
   * usually fits one of them the way it was written and the other by something the language did for
   * it.
   *
   * 1. **A candidate that needed no default fitted the call as written.** `f(x: int)` and
   *    `f(x: int, y: int = 0)` both take `f(1)`, and the reader who wrote `f(1)` meant the first.
   * 2. **A candidate whose parameters are exactly the arguments' own types** beats one reached by a
   *    conversion. This is what makes a literal's natural type decide between two widths, and it is
   *    asked of the types the fit settled on rather than of the types the declaration wrote — so
   *    `g[T](x: T)` is exact at a `[]int` argument and beats `g(s: []const int)`, which is reached
   *    only by giving up the ability to write.
   * 3. **A candidate that named its parameters beats one that was solved for them**, where both
   *    fitted at the same types. `f(x: int)` beside `f[T](x: T)` at `f(0)` fits both at `int`, and
   *    a generic declaration is the one that took the call by being told what to be. Without this
   *    the second tie-break would turn that call — which resolves today — into an ambiguity, so it
   *    is a guard rather than a preference.
   *
   * What is deliberately *not* here is a rule ranking one conversion above another. Two candidates
   * each reached by a different conversion are ambiguous, and saying so is better than a ladder of
   * precedences nobody can predict from the source. Ranking a declaration against a declaration, as
   * the third does, is a different question from ranking the routes the arguments took.
   *
   * **THE THIRD ASKS ITS QUESTION OF A POOL, AND THE POOL IS NOT ALWAYS THE EXACT ONES.** It read
   * `exactTypes` until card `0369`, so it was skipped outright whenever *no* candidate was exact —
   * which is every call reached by a conversion. `g(x: []u8)` beside `g[T](x: []T)` resolves at a
   * `[]u8` argument and was ambiguous at a `[3]u8` one, on the same two declarations fitted at the
   * same signature, because the array had to be viewed to reach either.
   *
   * The widening is bounded by the paragraph above rather than by a new principle: where the
   * candidates all fitted at **identical** parameter types, they took identical routes, so choosing
   * between them ranks nothing but the declarations. Candidates that fitted at different types stay
   * ambiguous, since telling them apart would be ranking the conversions.
   */
  private def narrow(
      fits: List[(FuncDecl, List[Type])],
      written: List[Expr],
  ): List[(FuncDecl, List[Type])] =
    if fits.length <= 1 then fits
    else
      val exactArity = fits.filter(_._1.params.length == written.length)
      val ranked     = if exactArity.length == 1 then exactArity else fits
      val natural    = written.map(a => probe(analyzeExpr(a).ty))

      if ranked.length <= 1 then ranked
      else
        val exactTypes = ranked.filter { (_, ptypes) =>
          ptypes.length == natural.length && ptypes.zip(natural).forall((p, t) => t.contains(p))
        }

        if exactTypes.length == 1 then exactTypes
        else
          val pool =
            if exactTypes.nonEmpty then exactTypes
            else if ranked.map(_._2).distinct.length == 1 then ranked
            else Nil

          val spelled = pool.filter(_._1.tparams.isEmpty)

          if spelled.length == 1 then spelled else ranked

  /** Whether a callee with this many parameters could take this many arguments at all — the arity
   * question alone, with nothing said about types. Defaults make the low end, a variadic tail
   * removes the high one.
   */
  private def arityFits(params: Int, variadic: Boolean, args: Int): Boolean =
    args <= params || (variadic && args >= params)

  /** One declaration as a diagnostic lists it: the name a reader wrote and the parameters that tell
   * it from its siblings. The result is left off — it is not what distinguishes two overloads,
   * since `reference/declarations.md § Overloading` refuses a pair that differ only in it.
   */
  private def signatureOf(f: FuncDecl): String =
    s"${qn(f.name)}(${f.params.map(p => s"${p.name}: ${p.typ.show}").mkString(", ")})"

  /** The type parameters of a declaration that **inference cannot reach from a call**: named by
   * none of its parameters and by no result, so both of `reference/generics.md § Inference is
   * bidirectional`'s directions are empty — the arguments say nothing about them and neither does
   * the type the value is read into.
   *
   * It is a property of the *declaration* rather than of any one call, which is what makes it worth
   * asking as its own question: no call of a function shaped like this can be solved, so the reader
   * is owed the one thing that does settle it rather than a remedy that does not apply. `solve` could
   * only say what it failed to find, and what it asked for — an annotation on the expected type — is
   * impossible advice where the result mentions nothing.
   *
   * The shape is easiest to reach for with a value parameter: a `[const W: usize]` kernel that reads
   * and writes through slices carries its width in no argument and answers `unit`. **That is the
   * shape that earned the written list**, so this now names it.
   */
  protected def unsettleable(f: FuncDecl): List[String] =
    f.tparams.filterNot(tp =>
      f.params.exists(p => mentions(p.typ, Set(tp))) || f.retType.exists(mentions(_, Set(tp))) ||
        arrowMentions(f, tp))

  /** Whether a parameter is named by an **arrow the sugar moved into a bound**.
   *
   * `f: A -> B` is a parameter type as the author wrote it, and `callBounds` rewrites it to a plain
   * `$F` whose bound carries the `A` and the `B`. So a walk over the written parameter types no
   * longer sees them, and `chain[A, B](x: int, f: int -> A, g: A -> B, …)` was told `A` appears in
   * neither its parameters nor its result — on a declaration whose second parameter is `int -> A`.
   *
   * Read off the bound rather than by un-rewriting, since the bound is where the types now live and
   * is what every later stage reads them from.
   */
  private def arrowMentions(f: FuncDecl, tp: String): Boolean =
    f.params.exists(_.typ match
      case NamedType(n, Nil) if MemberLowering.isCallBound(n) =>
        f.bounds.getOrElse(n, Nil).exists(_.args.exists(mentions(_, Set(tp))))
      case _ => false)

  /** The type parameters somebody **wrote**, which are the ones an explicit list fills.
   *
   * A bare arrow is sugar for a bounded parameter (`MemberLowering.callBounds`), so a declaration
   * written `chain[A, B](x: int, f: int -> A, g: A -> B)` carries four parameters and two of them
   * are the sugar's. Those are named with a character no identifier may hold, precisely so that no
   * program can write one — and there is nothing to write anyway, the argument being the closure's
   * own anonymous type.
   *
   * **So an explicit list is a list of the ones the author declared**, and the sugar's stay where
   * they have always come from, which is the arguments. Counting all of them told a reader that a
   * declaration they wrote as `chain[A, B]` takes four, and named two with a `$`.
   */
  protected def authored(tparams: List[String]): List[String] =
    tparams.filterNot(MemberLowering.isCallBound)

  /** Which copy of a generic declaration a **written** type-argument list names — `&f[T]` at an
   * address (`reference/ffi.md § A function's address`) and `f[T](x)` at a call
   * (`reference/generics.md § [] means type application in a type, indexing in an expression`).
   *
   * The two positions share every rule and differ only in what a refusal points at, which is what
   * `atCall` is for. They were one position for as long as the call head was deferred; what settled
   * that deferral is the shape a *value* parameter makes reachable, where a kernel reading and
   * writing through slices names its width in no argument and answers `unit`, so neither of
   * `reference/generics.md § Inference is bidirectional`'s directions carries it and there is no
   * binding to annotate.
   *
   * The expected type is not consulted. Where both are present the written arguments win outright
   * rather than being checked against it, because the result is checked against the expected type
   * anyway by whatever it is being handed to — a second check here would report the same mismatch in
   * worse words.
   */
  protected def instantiationWritten(
      written: String,
      decl: FuncDecl,
      targs: List[Expr],
      atCall: Boolean,
  ): String = {
    // **A bare arrow's parameter has no written form and no argument here to read it off**, which
    // leaves an address with nothing to instantiate. A call is the other case and does not come
    // through here: there the arguments answer it, which is where it came from.
    if authored(decl.tparams).length != decl.tparams.length then
      err(s"'$written' takes a bare arrow, so it is generic in the callable's own type — and an " +
        "address has no arguments to read that from, nor any way to write it. Declare the " +
        "parameter as a counted callable, '&Fn(…) -> …', to give it a type that can be named")

    val types = writtenTypeArgs(written, decl.tparams, decl.tvalues, decl.tpacks, targs, atCall)

    // The same check a call makes on the arguments it solved (`checkBounds`). Writing them out does
    // not exempt them: a bound is what the body was compiled against, and an unsatisfied one would
    // otherwise surface as a missing method inside a monomorphized body the reader never wrote.
    checkBounds(decl, types)
    instantiateFunc(decl, types)
  }

  /** The types a written list stands for, checked against the parameters it is a list *for*.
   *
   * It takes the three lists rather than a declaration because a **member** carries its own set
   * beside the ones it inherits from the receiver's type: `fd.tparams` for a method is the owner's
   * arguments followed by the member's, and only the member's half is ever written at a call.
   */
  protected def writtenTypeArgs(
      written: String,
      tparams: List[String],
      tvalues: Map[String, TypeRef],
      tpacks: Set[String],
      targs: List[Expr],
      atCall: Boolean,
  ): List[Type] = {
    if tparams.isEmpty then
      err(s"'$written' is not generic, so it has no type arguments to write — " +
        (if atCall then s"the call is '$written(…)'" else s"its address is '&$written'"))

    if targs.length != tparams.length then
      err(s"'$written' takes ${quantity(tparams.length, "type argument")} " +
        s"(${tparams.map(t => s"'$t'").mkString(", ")}), and ${targs.length} " +
        s"${if targs.length == 1 then "was" else "were"} written")

    // A type **pack** stands for a list of types rather than one, so it has no written argument to
    // stand against: `..A` is not an expression in any reading, and writing out one argument per
    // element would be a different arity from the declaration's.
    for tp <- tparams if tpacks(tp) do
      err(s"'$tp' is a type pack, which stands for a list of types rather than one, so it has no " +
        "written form here — a declaration taking one has its instantiation read off " +
        (if atCall then "the arguments at the call" else "the type its address is wanted at"))

    // A declaration's parameters are one list and one argument position whichever kind each of them
    // is (`reference/generics.md § A parameter may stand for a value`), so this walks the two lists
    // together: a `const` parameter folds its argument to a value of the type the declaration
    // wrote, and every other one resolves as a type.
    tparams.zip(targs).map { (tp, e) =>
      tvalues.get(tp) match
        case Some(vt) => at(e.pos)(valueArg(ValueArgType(e), recover(Type.Unknown)(rt(vt)), tsubst))
        case None     => rt(typeArgWritten(e, atCall))
    }
  }

  /** A written type argument, as the *expression* grammar delivered it.
   *
   * `&f[T]` and `f[T](x)` are both parsed as a subscript, because a name followed by a bracket is a
   * subscript everywhere else in the language and the parser is not the thing that knows what `f`
   * is. So what arrives is an expression to be read back as the type it was written as, and the
   * shapes that survive the round trip are the ones a type and an expression spell identically: a
   * name, a qualified name, a name applied to arguments, `*T`, `&T`, a tuple, and an integer for a
   * value parameter (`reference/generics.md § A parameter may stand for a value`).
   *
   * **The rest are refused by name rather than misread.** A slice, a `weak`, a `volatile`, a vector
   * and a callable have spellings the expression grammar has no production for, so there is nothing
   * here to recover them from. That is a hole in this form and not in the language: an annotation on
   * what receives the value still reaches every one of them, which is what the message says to write.
   */
  protected def typeArgWritten(e: Expr, atCall: Boolean): TypeRef = at(e.pos) {
    e match
      case Ident(n)          => NamedType(n)
      case Unary("*", inner) => PtrType(typeArgWritten(inner, atCall))
      case Unary("&", inner) => RefType(typeArgWritten(inner, atCall), sync = false)
      case Tuple(parts)      => TupleType(parts.map(typeArgWritten(_, atCall)))
      case n: IntLit         => ValueArgType(n)
      case Index(r, i)       => NamedType(typeArgName(r), List(typeArgWritten(i, atCall)))
      case TypeArgs(r, as)   => NamedType(typeArgName(r), as.map(typeArgWritten(_, atCall)))
      case f: Field          => NamedType(typeArgName(f))
      case _ =>
        err("this is not a type — what is written in the brackets is a type argument, and they read " +
          "it out of the expression grammar, so a slice, a 'weak', a 'volatile', a vector and a " +
          "callable have no spelling here. " +
          (if atCall then "Annotate what receives the result and let the call infer it instead"
           else "Write the type on the binding instead: 'var f: *extern(…) -> … = &…'"))
  }

  /** The name a written type argument applies its own arguments to — `Box` in `Box[int]`, and the
   * whole dotted path in `mod.Box[int]`, which is one name to the type grammar (`qualifiedName`).
   */
  protected def typeArgName(e: Expr): String = e match
    case Ident(n)    => n
    case Field(r, n) => s"${typeArgName(r)}.$n"
    case _           => err("this is not a type name, so it cannot be given type arguments")

  protected def callFunction(
      f: FuncDecl,
      written: List[Expr],
      expected: Option[Type],
      targs: List[Expr] = Nil,
  ): TExpr = {
    // A variadic callee — foreign or sysl's own — fixes only where its declared parameters stop;
    // everything after them is the tail, checked by the rule below rather than against a parameter.
    val variadic = f.variadic

    val shown = qn(f.name)

    // Names placed and defaults filled before anything else reads the list, so everything below —
    // the arity check, the generic solve, `checkArgs` — sees the call written out in full.
    val args = bindArgs(s"function '$shown'", Some(f.name), f.params, written, variadic)

    // A `@test` function has one caller and it is not in the program (`reference/attributes.md § A test has one caller, and the program is not it`). Every build but
    // `sysl test` drops it, so a call would compile and then fail at the *link*, naming a symbol
    // nothing in the source explains — which is the shape of failure a diagnostic exists to prevent.
    // Refused wherever the call is written, a test's own body included: two tests sharing work share
    // an ordinary function, which is what one is for.
    if f.test.isDefined then
      err(s"'$shown' is a '@test' function, which 'sysl test' calls and nothing else does — " +
        "every other build leaves it out, so this call would have no definition to reach. " +
        "Work two tests share belongs in an ordinary function they both call")

    // An interrupt handler has one caller and it is the processor (`reference/ffi.md § interrupt`).
    // It is entered on an asynchronous event with a frame the hardware pushed, and it leaves
    // through a return-from-interrupt that restores the interrupted context — so a call written
    // here would set up an ordinary frame and then execute an instruction that unwinds something
    // that never happened. Its address is still worth taking, which is what a vector table is built
    // from.
    if f.conv.isDefined then
      err(s"'$shown' is an interrupt handler, which the processor enters and no program calls — " +
        "it leaves through a return-from-interrupt, which would unwind a frame this call never " +
        s"pushed. Take its address for the vector table with '&$shown', and put whatever the two " +
        "share in an ordinary function they both call")

    checkArity(s"function '$shown'", f.params.length, variadic, args.length)

    // An extern is declared in the output only if something reaches it, which is what keeps an
    // unused one — the library's `exit`, in a program that never panics — out of the module.
    if externDecls.contains(f.name) then externsUsed += f.name

    val (name, pre) =
      // **Written out, they are what settles the instantiation and nothing else is consulted**
      // (`reference/generics.md § [] means type application in a type, indexing in an expression`)
      // — not the arguments, and not the expected type. Their whole reason for existing is the call
      // inference cannot reach, so a solve running first would report a failure about a question
      // the reader has already answered.
      if targs.nonEmpty && authored(f.tparams).length == f.tparams.length then
        (instantiationWritten(shown, f, targs, atCall = true), None)
      else if f.tparams.isEmpty then (f.name, None)
      else
        // What a written list says, where the declaration also carries the sugar's own parameters.
        // The written ones are answered and the rest are solved below exactly as they always were,
        // which is the whole of what an explicit list is for on such a declaration.
        val written =
          if targs.isEmpty then Map.empty[String, Type]
          else
            val mine = authored(f.tparams)

            mine.zip(writtenTypeArgs(shown, mine, f.tvalues, f.tpacks, targs, atCall = true)).toMap

        // Asked before the solve rather than left to it, because the solve can only report what it
        // failed to find and the answer here is that it was never going to find it — and, since the
        // list may be written, that there is somewhere to say so. A parameter the reader has just
        // written out is not stuck, whatever the declaration says about where it appears.
        val stuck = unsettleable(f).filterNot(written.contains)

        if stuck.nonEmpty then
          val names    = stuck.map(t => s"'$t'").mkString(" and ")
          val (is, it) = if stuck.length == 1 then ("is", "it") else ("are", "them")

          err(s"$names $is in neither the parameters of '$shown' nor its result, so nothing in this " +
            s"call says what $it should be — write $it out, as '$shown[…](…)'")

        val provisional =
          provisionalArgs(f.name, f.tparams, f.params.map(_.typ), args, f.bounds, written,
            result = f.retType, expected = expected)
        // The parameter types being matched against are the declaration's, written in the
        // declaration's terms — so a `Pair[T]` there is that module's `Pair` whichever module the
        // call was written in.
        val solved = inDecl(f.name)(
          solve(shown, f.tparams, f.params.map(_.typ), provisional.map(_.ty), f.retType, expected,
            args.zip(provisional).map((a, t) => adaptable(a, t)), f.bounds, written))
        checkBounds(f, solved)
        (instantiateFunc(f, solved), Some(provisional))

    val (params, rtype) = funcInsts(name)
    // A variadic's tail has no declared parameter to be checked against and is analyzed below, so
    // both lists are cut to the parameters — which is also what keeps them aligned.
    val checked  = checkArgs(shown, params, args.take(params.length), pre.map(_.take(params.length)))

    checkCrossings(f.crossing, shown, params, checked)

    val declared = externDecls.get(f.name).fold(checked)(vaPassed(checked, _))

    funcsUsed += name
    // Only a free function can be an `extern`, so this is the one call form whose tail may be a
    // foreign one — and the only one an aggregate may cross.
    val foreign = externDecls.contains(f.name)

    TCall(name, declared ::: args.drop(params.length).map(variadicArg(_, foreign)), rtype)
  }

  /** Holds each `@crossing` argument to `06`'s rule about what may reach another concurrency domain.
   *
   * **The check is made at the call and reads the *instantiated* signature**, which is what makes it
   * reach a facility the language does not own: a package's `task(…)` and the library's `spawn` are
   * one shape here, and a generic parameter has a concrete type by the time this runs, exactly as a
   * `&sync Box[T]` is asked afresh per instantiation (`Sharing`).
   *
   * **A `*T` parameter is looked *through*, and that is the whole of what the annotation buys.** A
   * raw pointer is on the crossable list because it carries no count of its own — which says nothing
   * about the object at the far end, and the object at the far end is what crossed. Everything else
   * is asked about the parameter's own type, because a parameter copies no buffer and shares
   * whatever it names.
   *
   * Reported at the **argument**, since that is the thing that could have been written differently.
   *
   * **It takes the list rather than a declaration**, because a member carries one too (card `0313`)
   * and a member reaches a call through `MethodCalls` and `AbstractMethods` rather than through the
   * free-function path here. A receiver among `params` is skipped by construction: `@crossing` names
   * the member's own parameters and `self` is not one of them.
   */
  protected def checkCrossings(
      crossing: List[String],
      shown: String,
      params: List[(String, Type)],
      args: List[TExpr],
  ): Unit =
    if crossing.nonEmpty then
      for ((pname, pty), a) <- params.zip(args) if crossing.contains(pname) do
        val (subject, crossed) = pty match
          case Type.Ptr(inner) => (s"what '$pname' of '$shown' points at", inner)
          case other           => (s"'$pname' of '$shown'", other)

        at(a.pos)(Sharing.crossing(subject, crossed).foreach(err))

  /** The arguments of a foreign call, with each one headed for a C by-value `va_list` parameter
   * turned into what the target's ABI passes there (`getting-started/cli.md § targets`).
   *
   * The address is what was checked and is what all three answers are formed from, so this wraps
   * rather than replaces — and it happens here, once, because a foreign call is the only place a
   * `va_list` leaves sysl's own convention behind.
   */
  private def vaPassed(args: List[TExpr], e: ExternDecl): List[TExpr] = {
    val byValue = foreignVaByValue(e)

    if byValue.isEmpty then args
    else args.zipWithIndex.map((a, i) => if byValue(i) then TVaPass(a) else a)
  }

  /** How many arguments a callee accepts, said in the caller's own words.
   *
   * A `...` turns the count into a floor: the declared parameters are still all required, and what
   * follows them is the tail, which no declaration bounds. Every call form asks this the same way,
   * so a member and a free function report the mistake in the same shape.
   */
  protected def checkArity(shown: String, want: Int, variadic: Boolean, got: Int): Unit =
    if variadic then
      if got < want then
        err(s"$shown takes at least ${quantity(want, "argument")}, but ${supplied(got, "argument")}")
    else if got != want then
      err(s"$shown takes ${quantity(want, "argument")}, but ${supplied(got, "argument")}")

  /** One argument in a variadic extern's tail.
   *
   * There is no declared parameter to check it against, so what stands in for one is the rule C
   * itself imposes at the call: only what varargs can carry may be passed, and it is passed
   * already widened. LLVM applies no default argument promotions of its own — an `i8` or an `f32`
   * handed over as written is read back as garbage — so the widening happens here, in the tree,
   * where it is something a test can see rather than a detail of the emitter.
   *
   * What may always cross is what C can name on the other side: an integer, a float, a `char`, a
   * raw pointer, or the address of a function.
   *
   * A **`*extern`** is in that list because it is a pointer everywhere it matters — its `lty` is
   * `LType.Ptr`, it occupies one general-purpose register, and a C library's variadic setter is the
   * ordinary way one is registered (`curl_easy_setopt(h, CURLOPT_WRITEFUNCTION, f)`). It is a
   * distinct type from `Ptr` because there is no value at the end of it to read through, which is a
   * statement about dereferencing rather than about how it travels.
   *
   * An **aggregate** crosses to a *foreign* callee and not to a sysl one, and the asymmetry is not
   * an oversight. C allows a struct in a variadic tail and hands it over under exactly the
   * classification a declared parameter of the same type gets (`getting-started/cli.md § targets`),
   * so a foreign call needs no rule of its own — whoever compiled the other side applies that
   * classification too. A **sysl** variadic callee reads its own tail back with a walk (§9), and
   * the walk is written for values that fit a register; an aggregate would have to be read back
   * some other way, which is a question about the walk and not about the call.
   */
  protected def variadicArg(a: Expr, foreign: Boolean = false): TExpr = {
    val t = analyzeExpr(a)

    at(t.pos):
      t.ty match
        case i: Type.Integer if i.bits < 32   => convert(t, Type.Integer(32, i.signed))
        case f: Type.Floating if f.bits < 64  => convert(t, Type.Real)
        case _: Type.Integer | _: Type.Floating | Type.Char | _: Type.Ptr | _: Type.CFn => t
        case other if CAbi.aggregate(other) && foreign => t
        case other if CAbi.aggregate(other) =>
          err(s"a ${show(other)} cannot be passed to a sysl function's '...' — a walk over the tail " +
            "reads back one register at a time and an aggregate is not one, where a foreign callee " +
            "takes it because C says which registers it arrives in")
        case other =>
          err(s"a ${show(other)} cannot be passed to '...' — a variadic argument must be an " +
            "integer, a float, a char, a raw pointer, or the address of a function")
  }

  /** Enforces a generic function's trait bounds against the type arguments a call resolved to. For
   * each bounded parameter, the concrete type must carry an `impl` of every trait the bound names —
   * checked here at the call, so a caller supplying a type that does not implement the trait is
   * told exactly that, rather than meeting a missing-method error deep inside the monomorphized
   * body. A user type conforms by an `impl` written for its owner key, a built-in by the compiler's
   * own rule (`reference/expressions.md § Operator dispatch`) — which is what lets `sum(3, 4)`
   * instantiate a `[T: Add]`.
   *
   * A *type's* parameters are held to their bounds by the same rule, at the point the type is
   * applied, so the two forms of "what this declaration assumes" are one check.
   */
  protected def checkBounds(f: FuncDecl, targs: List[Type]): Unit =
    inDecl(f.name)(checkParamBounds(qn(f.name), f.tparams, f.bounds, targs))
}
