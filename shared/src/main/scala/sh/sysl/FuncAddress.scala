package sh.sysl

import scala.collection.mutable

/** A function's address, and a call through one — the two halves of `reference/ffi.md § A
 * function's address`.
 *
 * This is the seam a C library reaches back through. `extern` (`reference/ffi.md § extern — a declaration with no body`) lets sysl call outward, and
 * that was enough while every foreign interface was one a program calls; it is not enough for the
 * ones that call *back*. `qsort` wants a comparison, `signal` wants a handler, `pthread_create`
 * wants a thread body, and every one of them takes it as an address. Symmetrically, `dlsym` hands an
 * address *in*, and until something could be called through one that whole interface was declarable
 * and unusable.
 *
 * Both directions are the same value — one word holding the address of code compiled to the
 * machine's C convention — so both are `Type.CFn`, and neither is a sysl callable. A `Fn` is a
 * trait (`reference/types.md § Function types`): a bound at a parameter, a two-word object where a
 * concrete type is required, and in both cases something with an environment beside it. C has no
 * notion of an environment, which is why this is a separate type rather than a third mode of that
 * one, and it is why nothing here can take the address of a closure.
 */
trait FuncAddress extends CallCore {

  /** `&f` — the address of a declared function.
   *
   * The `&` is the same one `03` gives every other address, and it is deliberate that no bare `f`
   * reaches here: a bare function name already means the capture-free closure
   * (`reference/expressions.md § Closures`), and a spelling that meant a sysl callable in one slot
   * and a C address in another would be a silent choice between two representations that share
   * nothing.
   */
  protected def functionAddress(
      written: String,
      plain: String,
      expected: Option[Type] = None,
      targs: List[Expr] = Nil,
  ): TExpr = {
    val key  = overloadAddressed(written, plain, expected)
    val decl = funcDecls(key)

    // Which copy of it? A generic function is not code until its arguments are settled
    // (`reference/generics.md § Monomorphization`), so there is no single body to name until they
    // are — but the *expected type* settles them wherever there is one, exactly as a call's
    // arguments do, and an instantiation is one body like any other. Refusing outright was too
    // broad: it is what stopped a library offering a callback helper at all, since a trampoline's
    // state type belongs to the application rather than to the binding, so every application had to
    // hand-roll one and cast the userdata itself.
    //
    // Where the arguments are *written*, they are what settles it, and the expected type is not
    // consulted at all: `&f[T]` exists precisely for the signatures the expected type cannot solve.
    val instKey =
      if targs.nonEmpty then instantiationWritten(written, decl, targs, atCall = false)
      else if decl.tparams.isEmpty then key
      else instantiationFor(written, decl, expected)

    // A `...` is read relative to the last named argument, so a caller has to know where the named
    // ones stop. A `*extern` says only what it is called with, which is what makes it callable at
    // all, and there is nothing in that to hold a tail.
    if decl.variadic then
      err(s"'$written' is variadic, and a '*extern' fixes the arguments a call passes — a tail has " +
        "no width a signature could state, so a variadic function is reached by calling it")

    if decl.test.isDefined then
      err(s"'$written' is a '@test' function, which 'sysl test' calls and nothing else does — every " +
        "other build leaves it out, so its address would be of a definition the program does not have")

    // An intrinsic is a name the back end recognises and lowers, not a function that exists to be
    // pointed at: there is no body anywhere for an address to name, and LLVM refuses a module that
    // takes one. The wrapper the message asks for *is* a real function and does have an address.
    if externDecls.get(key).exists(e => Intrinsics.declared(e.symbol)) then
      err(s"'$written' is an intrinsic, which the back end lowers to an instruction rather than a " +
        "function anything calls — there is no body for an address to name. A sysl function that " +
        "calls it is what has one")

    val (params, ret) = funcInsts(instKey)
    val ptypes        = params.map(_._2)

    // The address is of *code*, and the code has to be the code C would call. Where every part of
    // the signature crosses as itself the two conventions agree and the function's own symbol is the
    // answer; where one does not, sysl emitted the aggregate its own way and C would read the wrong
    // registers. Refusing is the honest half of that — an adapter is what closes it, and until there
    // is one this says which parameter is the problem rather than handing over an address that
    // silently means something else.
    //
    // A **narrow** scalar crosses as itself and is not one of those: the widening C expects of the
    // result is written on the definition rather than being something an address has to arrange
    // (`CAbi.extension`), so a callback answering a `bool` is reachable through here and correct.
    //
    // **Two kinds of function are past this question rather than exempted from it**, and both used
    // to be refused by it (0136):
    //
    //   - an **`extern`** is C. Sysl neither compiled it nor chose its convention, and its type is
    //     what the header published — so there is no lowering here to be wrong about, and the
    //     reason above is a statement about code this compiler emitted. Refusing one made every C
    //     callback a binding wanted to register unnameable, which is most of what a callback-shaped
    //     library is;
    //   - a function carrying **`@export`**, whose address is its thunk's (`ExportThunk`) — a
    // definition that genuinely has the convention this rule is written about. `reference/ffi.md §
    // @export` said that all along; what changed is that it is now true.
    val hasCConvention = externDecls.contains(instKey) || decl.exported.isDefined

    if !hasCConvention then
      for (t, i) <- (ptypes :+ ret).zipWithIndex do
        if !crossesAsItself(t) then
          val which = if i < ptypes.length then s"the ${ordinal(i + 1)} parameter" else "the result"

          err(s"$which of '$written' is ${show(t)}, an aggregate, and an aggregate crosses to C in " +
            "whichever registers that machine's convention names rather than the ones a sysl call " +
            "uses — so this address would be of a function C cannot call correctly. A wrapper " +
            "taking the parts behind a '*T', or '@export' on this one, is what has an address")

    funcsUsed += instKey
    if externDecls.contains(instKey) then externsUsed += instKey

    TFuncAddr(instKey, instKey, Type.CFn(ptypes, ret))
  }

  /** Which declaration of an overloaded name an address is of (`reference/declarations.md §
   * Overloading`).
   *
   * **The expected type decides, and it is the same mechanism a generic function's arguments are
   * read off** — an address is handed to something whose signature is already fixed, so the
   * signature is there to be matched. What is compared is the parameter list: a `*extern` states
   * exactly what a call through it passes, and that is what tells two overloads apart.
   *
   * The result is deliberately not compared. `reference/declarations.md § Overloading` refuses a
   * pair differing only in it, so it carries no information here — and comparing it would refuse an
   * address whose expected type is spelled with a result the declaration converts to.
   *
   * **With no expected type there is nothing to read**, and this reports rather than guessing. An
   * address is one word with no context of its own; taking the first declaration would be choosing
   * silently between functions that share nothing but a name.
   */
  private def overloadAddressed(written: String, plain: String, expected: Option[Type]): String = {
    // The same set a call chooses from — a declaration this file may not name is not one an address
    // may be taken of either (`reachableOverloads`).
    val keys = reachableOverloads(plain)

    if keys.length == 1 then keys.head
    else
      val wanted = expected.flatMap(cfnOf)
      val fits   = keys.filter(k => wanted.exists(w => probe(funcInsts(k)._1.map(_._2)).contains(w.params)))

      fits match
        case List(one) => one
        case _ if wanted.isEmpty =>
          err(s"'$written' names ${keys.length} functions, and an address is of one of them — " +
            "which is read off the type the context wants, and there is none here. Annotate what " +
            "this address is being stored in or passed as")
        case Nil =>
          err(s"no '$written' has the parameters '${show(expected.get)}' asks for — the declarations " +
            s"of that name are:\n" + keys.map(k => s"    ${addressOf(k)}").mkString("\n"))
        case many =>
          err(s"'$written' is ambiguous as an address — ${many.length} of its declarations match " +
            s"'${show(expected.get)}'")
  }

  /** One declaration of an overloaded name as an address's diagnostic lists it. */
  private def addressOf(key: String): String =
    probe(funcInsts(key)) match
      case Some((params, ret)) =>
        s"${qn(key)}: *extern(${params.map(p => show(p._2)).mkString(", ")}) -> ${show(ret)}"
      case None => qn(key)

  /** Which copy of a generic function this address names, read off the type the context wants.
   *
   * This is what serves every address whose arguments the signature *does* mention, which is most
   * of them: the match is against the **signature as declared**, so it solves exactly what a call
   * site's arguments would, and a `*extern` is handed to something whose signature is already
   * fixed. A parameter the signature does not mention cannot be solved here — that is what the
   * written form (`CallCore.instantiationWritten`) is for, and the message below names it.
   */
  private def instantiationFor(written: String, decl: FuncDecl, expected: Option[Type]): String = {
    val want = expected.flatMap(cfnOf).getOrElse(
      err(s"'$written' is generic, so which copy of it this addresses depends on its type " +
        s"arguments, and nothing here says what they are — write them, as " +
        s"'&$written[${decl.tparams.mkString(", ")}]', or write the type this is stored in, as " +
        s"'var f: *extern(…) -> … = &$written'"))

    if want.params.length != decl.params.length then
      err(s"'$written' takes ${quantity(decl.params.length, "parameter")}, and the " +
        s"${show(want)} wanted here takes ${want.params.length}")

    val tparams = decl.tparams.toSet
    val sub     = mutable.Map.empty[String, Type]

    for (p, a) <- decl.params.zip(want.params) do unify(p.typ, a, tparams, sub)
    for r <- decl.retType do unify(r, want.ret, tparams, sub)

    val unsolved = decl.tparams.filterNot(sub.contains)

    if unsolved.nonEmpty then
      err(s"'$written' is generic, and the ${show(want)} wanted here does not say what " +
        s"${unsolved.map(t => s"'$t'").mkString(" or ")} should be — a type parameter the signature " +
        s"does not mention cannot be settled by the type of the address. Write it: " +
        s"'&$written[${decl.tparams.mkString(", ")}]'")

    instantiateFunc(decl, decl.tparams.map(sub))
  }

  /** Whether a type is handed over to C as the thing it is, so a sysl definition of that signature
   * is already one the other side can call.
   *
   * The test is by **shape** rather than by asking `CAbi`, and deliberately: the classification is
   * a property of the target and this runs before one is chosen, so a rule that consulted it would
   * let a program compile for one machine and not another. What every convention agrees about is
   * the boundary drawn here — a value that occupies one register crosses as itself everywhere, and
   * an aggregate is what each of them has its own reading of (`getting-started/cli.md § targets`).
   */
  private def crossesAsItself(t: Type): Boolean = Type.underlying(t) match
    case _: Type.Integer | _: Type.Floating | Type.Bool | Type.Char => true
    case Type.Unit | Type.Never                                     => true
    case _: Type.CFn                                                => true
    // A trait object is a pair of words on either mode, so it is an aggregate however it is spelled.
    case Type.Weak(inner)                                           => !inner.isInstanceOf[Type.Trait]
    case p @ (_: Type.Ptr | _: Type.Ref)                            => !Type.erased(p)
    // A simple enum is its underlying integer and nothing else; a data enum is a tag beside a union.
    case e: Type.Enum                                               => e.simple
    case _                                                          => false

  /** `p(args)` where `p` holds a `*extern`.
   *
   * There is no declaration to check against — the callee is a value — so what the arguments are
   * held to is the signature the *type* carries. That is the whole of the promise: nothing here can
   * see the function at the other end, and the `*` is where the language says so.
   */
  protected def callThroughAddress(callee: TExpr, args: List[Expr]): TExpr = {
    val Type.CFn(params, ret) = cfnOf(callee.ty).get: @unchecked

    if args.length != params.length then
      at(callee.pos)(err(s"a ${show(callee.ty)} is called with ${quantity(params.length, "argument")}, " +
        s"and ${args.length} were given"))

    TCallPtr(callee, checkArgs(show(callee.ty), positions(params), args, None, positional = true), params, ret)
  }

  /** The parameters of a `*extern`, in the shape `checkArgs` reads. A function pointer's parameters
   * have no names — the type is the only thing that describes them — so a mismatch names the
   * position, which is the same thing a callable's does (`checkArgs`).
   */
  private def positions(params: List[Type]): List[(String, Type)] =
    params.zipWithIndex.map((t, i) => (s"${i + 1}", t))

  /** The C function pointer a type is, seeing through a transparent subtype the way every other
   * question about a representation does.
   */
  protected def cfnOf(t: Type): Option[Type.CFn] = Type.underlying(t) match
    case c: Type.CFn => Some(c)
    case _           => None
}
