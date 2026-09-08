package sh.sysl

import ir.{Access, Arg, BinOp, CastOp, Inst, LType, Val}

/** Arithmetic, comparison, and the conversions between scalar widths.
 *
 * Two things run through all of it. The first is that a **constrained** type is laid out as its
 * base and computes as its base — the subtype names a set of values, not a second way to add — so
 * `a += e` and `a = a + e` reach the same instruction by different routes and must agree about it.
 *
 * The second is that overflow checking is **opt-in and narrow**. Arithmetic on raw integers is
 * defined to wrap, so the checked form is in play only when an operand came through a ranged
 * (`within`) type, and only then when the exact interval the operands allow can fall outside the
 * base width. `overflowChecked` computes that interval rather than guessing at it, which is what
 * keeps a small counter or an index on the plain path at no cost.
 *
 * Where a type has no instruction for what is being asked, the operator's trait method stands in
 * (`reference/expressions.md § Operator dispatch`). `dispatchValue` is that path, and it applies a
 * method to two **values** rather than two expressions — which is the whole distinction from
 * lowering an ordinary call: the forms that reach it use one operand twice from a single
 * evaluation.
 */
trait ArithEmitter extends CallEmitter {
  /** What a compound assignment stores: a slot that holds a count — a string, or a struct with a
   * reference in it — builds a fresh value the slot will take a count for, and a slot of anything
   * else is arithmetic.
   */
  protected def combine(op: String, ty: Type, valueTy: Type, dispatch: Option[TDispatch],
                        cur: Val, v: Val): Val =
    (ty, dispatch) match
      // The dispatched call owns its own result, the way every other call to sysl does — `ownTemp`
      // here as well would put one value in the region twice and release it twice.
      case (_, Some(d))  => dispatchValue(d, ty, valueTy, cur, v, ty)
      case (Type.Str, _) => ownTemp(strConcat(cur, v), Type.Str)
      // A constrained slot is arithmetic at the type it is laid out as, which is what the binary
      // path does too — the subtype names a set of values, not a second way to add. It detects
      // overflow on the same terms as well: `a += e` computes what `a = a + e` computes, so a range
      // that invites a wrap has to be caught here too, or the two spellings disagree about a value
      // the produce-site check would then never see.
      case _ =>
        val bin = op.dropRight(1)
        val bt  = Type.underlying(ty)
        // `x <<= n` computes what `x = x << n` computes, so the count is widened or clamped on the
        // same terms — see `ScalarEmitter.shiftAmount`.
        val rv  = if bin == "<<" || bin == ">>" then shiftAmount(bt, valueTy, v) else v

        bt match
          case it: Type.Integer if bin == "<<" && isRanged(ty)                       => checkedShl(it, cur, rv)
          case it: Type.Integer if (bin == "+" || bin == "-" || bin == "*") && isRanged(ty) =>
            checkedArith(bin, it, cur, rv)
          case _ => arith(bin, bt, cur, rv)

  /** Whether an integer `+`/`-`/`*` needs the overflow-detecting form. Arithmetic on raw integers is
   * defined to wrap, so it is only in play when an operand was produced through a ranged (`within`)
   * type — the opt-in that makes arithmetic checked — and only then when the exact result the
   * operands' ranges allow can fall outside the base width. A range whose results always fit the
   * width stays on the plain path, so the common case (a small counter, an index) costs nothing.
   */
  protected def overflowChecked(op: String, l: TExpr, r: TExpr, bt: Type.Integer): Boolean =
    (op == "+" || op == "-" || op == "*") && (isRanged(l.ty) || isRanged(r.ty)) && {
      val (alo, ahi) = interval(l, bt)
      val (blo, bhi) = interval(r, bt)
      val (rlo, rhi) = op match
        case "+" => (alo + blo, ahi + bhi)
        case "-" => (alo - bhi, ahi - blo)
        case "*" =>
          val corners = List(alo * blo, alo * bhi, ahi * blo, ahi * bhi)

          (corners.min, corners.max)
        case _ => sys.error(s"unreachable overflowChecked '$op'")

      rlo < baseMin(bt) || rhi > baseMax(bt)
    }

  /** A ranged constrained integer — a `within` type — is the opt-in that makes arithmetic checked;
   * a predicate-only subtype or a bare `new` derivation bounds no magnitude and does not qualify.
   */
  protected def isRanged(t: Type): Boolean = t match
    case c: Type.Constrained => (c.lo.isDefined || c.hi.isDefined) && Type.underlying(c).isInstanceOf[Type.Integer]
    case _                   => false

  /** The exact value interval an operand can hold: a literal is its own value; a ranged type is its
   * declared bounds (`..<` excludes the top); anything else spans the whole base width.
   */
  private def interval(e: TExpr, bt: Type.Integer): (BigInt, BigInt) = e match
    case TIntLit(v, _) => (v, v)
    case _ =>
      e.ty match
        case c: Type.Constrained if isRanged(c) =>
          val lo = c.lo.map(_.toBigInt).getOrElse(baseMin(bt))
          val hi = c.hi.map(h => if c.exclusiveHi then h.toBigInt - 1 else h.toBigInt).getOrElse(baseMax(bt))

          (lo, hi)
        case _ => (baseMin(bt), baseMax(bt))

  private def baseMin(t: Type.Integer): BigInt = if t.signed then -(BigInt(2).pow(t.bits - 1)) else BigInt(0)

  private def baseMax(t: Type.Integer): BigInt =
    if t.signed then BigInt(2).pow(t.bits - 1) - 1 else BigInt(2).pow(t.bits) - 1

  /** One comparison, over two values the caller is holding: an instruction where the operand type
   * has one, the method its `Eq`/`Ord` supplies otherwise.
   */
  protected def comparison(c: TCmp, ty: Type, av: Val, bv: Val): Val =
    c.dispatch match
      // A comparison stays homogeneous — `Eq` and `Ord` take no right-hand type — so both operands
      // are the one type here, unlike the arithmetic traits.
      case Some(d) => dispatchValue(d, ty, ty, av, bv, Type.Bool)
      // A constrained value compares through its base's instruction — the derived type has no
      // ordering of its own, it inherits the base's.
      case None    => compareValue(c.op, Type.underlying(ty), av, bv)

  /** Applies an operator's trait method to two **values** rather than two expressions
   * (`reference/expressions.md § Operator dispatch`).
   *
   * That is the whole distinction from lowering a `TCall`: the forms that reach here — a comparison
   * chain and a compound assignment — each use one operand twice from a single evaluation, and the
   * value is already in a register. `swap` and `negate` carry the derivation of the comparisons the
   * catalog declares no method for (`library/core.md § What is in it`), so `a > b` calls the one `lt` its `impl` wrote.
   *
   * The two operands carry their **own** types, which an arithmetic trait no longer requires to be
   * the same one (`library/core.md § Walking a type of your own`): `c *= 2.0` passes a complex
   * number and a `real`. A swap exchanges the values and their types together, since it is the
   * values that are being reordered.
   *
   * **Having the operands as values changes nothing about the convention they cross the call
   * under.** A value past `layout.indirect`'s boundary is handed over as an address whatever route
   * the call was written by, because that is what the callee's parameters say — so a large operand
   * is staged into a slot here exactly as `CallEmitter.argValue` stages one it cannot take the
   * address of, and a large result comes back through the `sret` pointer `genSyslCall` supplies.
   * Passing the value itself instead produces a `call` whose argument types disagree with the
   * definition's, which LLVM does not diagnose: the callee reads the first word of the aggregate as
   * the address it was promised and faults on it.
   */
  private def dispatchValue(d: TDispatch, aty: Type, bty: Type, av: Val, bv: Val, resultTy: Type): Val = {
    val (l, lty, r, rty) = if d.swap then (bv, bty, av, aty) else (av, aty, bv, bty)
    val (what, callee)   = calleeParts(d.name, resultTy)
    val res              = genSyslCall(what, callee, List(operandArg(lty, l), operandArg(rty, r)), resultTy, None)

    if !d.negate then res
    else
      val n = freshReg(); emit(Inst.Bin(n, BinOp.Xor, LType.I(1), res, Val.Bool(true))); n
  }

  /** One operand of a dispatched operator, in the form the callee receives it: itself where it is a
   * first-class value, and the address of a slot holding it where it is not.
   *
   * The slot is staging and nothing more — the callee copies out of it at entry and takes its own
   * count there, exactly as it does for an argument whose address the caller already had, so
   * nothing here owns anything and nothing releases it.
   */
  private def operandArg(ty: Type, v: Val): Arg =
    if !layout.indirect(ty) then Arg(ty.lty, v)
    else
      val slot = emitAlloca(freshReg(), ty.lty)

      emit(Inst.Store(ty.lty, v, slot, Access.Plain))
      Arg(LType.Ptr, slot)

  /** The zero value of a type — what a slot holds before anything is stored into it, and what a
   * function with no trailing expression returns.
   */
  protected def zero(ty: Type): Val = ty match
    // Nothing is stored for a zero-sized value, so its zero is nothing at all — the same empty
    // register every other read of one yields.
    case t if Type.zeroSized(t) => Val.Nothing
    // A constrained subtype is laid out as its base, so its zero is the base's zero, and a qualified
    // one is laid out as what it qualifies.
    case c: Type.Constrained  => zero(Type.underlying(c))
    case Type.Volatile(inner) => zero(inner)
    case _: Type.Integer  => Val.Int(0)
    case _: Type.Floating => Val.float(0.0)
    case Type.Char        => Val.Int(0)
    case Type.Bool        => Val.Int(0)
    // A trait object is two words, so its zero is a zeroed pair rather than a null address — and,
    // like every null pointer, calling through one is the programmer's business.
    case _: Type.Ptr | _: Type.Ref if Type.erased(ty) => Val.Zero
    case Type.Weak(inner) => if inner.isInstanceOf[Type.Trait] then Val.Zero else Val.Null
    case _: Type.Ptr      => Val.Null
    case _: Type.Ref      => Val.Null
    // C's null callback, which is a value several of its interfaces read as "do the default".
    case _: Type.CFn      => Val.Null
    case _: Type.Struct   => Val.Zero
    case _: Type.Array    => Val.Zero
    // Every lane zeroed, which `zeroinitializer` says for a vector as it does for an array — a
    // zeroed mask is every lane false, and a zeroed `<N>f32` is positive zero in each.
    case _: Type.Vector   => Val.Zero
    case _: Type.View     => Val.Zero
    case Type.VaList      => Val.Zero
    case e: Type.Enum     => if e.simple then Val.Int(0) else Val.Zero
    case Type.Unit        => Val.Nothing
    // Nothing is lowered from a program that has an error, and a type the analyzer could not work
    // out is only ever produced by one, so reaching here would mean codegen ran on a broken tree.
    case Type.Unknown     => sys.error("unreachable zero of an unknown type")
    // `never` has no values, so nothing ever starts at one: every path that would need this
    // diverges before reaching it.
    case Type.Never       => sys.error("unreachable zero of 'never'")
    // A type parameter reaches codegen from nowhere: the pass that stands one in for itself is
    // for diagnostics and throws its tree away, and monomorphization substitutes a concrete type
    // before anything is emitted.
    case _: Type.Abstract => sys.error("unreachable zero of a type parameter")
    // A **value** parameter reaches codegen from nowhere for the same reason and one step earlier:
    // its argument is folded into the length or the expression naming it while the instantiation is
    // still being built, so nothing is ever laid out at one.
    case _: Type.ConstArg => sys.error("unreachable zero of a value parameter")
    // A **pack** never reaches codegen either: `(..A)` becomes the ordinary tuple of whatever the
    // subject matched while the instantiation is still being built, so nothing is laid out at one.
    case _: Type.Pack     => sys.error("unreachable zero of a type pack")
    // A result list belongs to a signature. The analyzer's one funnel turns it into the tuple its
    // parts lay out as before anything holds it, so codegen is only ever handed that tuple.
    case _: Type.Results  => sys.error("unreachable zero of a result list")
    // A trait is only ever the pointee of a `*Trait` / `&Trait`, both handled above; resolving a
    // bare trait name is a diagnostic, so nothing of this type is ever laid out.
    case _: Type.Trait    => sys.error("unreachable zero of a trait")

  /** A call to an LLVM intrinsic overloaded on one integer width, declared on first use.
   *
   * These are the compiler's own reach for an instruction, not the library's — `Intrinsics` is the
   * table an `extern "llvm.…"` is checked against, and nothing in `sysl.math` could have declared
   * these: `Bits`' membership covers an open family of widths, so there is no finite set of
   * `extern`s to write. What the two mechanisms share is the reason the name carries the width,
   * which is that LLVM overloads on the operand type and spells the choice in the name — and they
   * share `Llvm`, which declares the base names both of them complete.
   *
   * `zeroFlag` is the extra `i1` that `ctlz` and `cttz` take and no other intrinsic here does. It
   * is always `false`, meaning a zero operand is defined rather than poison — see the call sites.
   */
  protected def intrinsic(base: String, ty: LType, args: List[Val], zeroFlag: Boolean = false): Val = {
    val name   = Llvm.bits(base).at(ty)
    val params = List.fill(args.length)(ir.Param(ty)) ++ Option.when(zeroFlag)(ir.Param(LType.I(1)))

    satDecls += ir.FuncSig(name, ir.FnType(ty, params))

    val ops = args.map(a => Arg(ty, a)) ++ Option.when(zeroFlag)(Arg(LType.I(1), Val.Bool(false)))
    val r   = freshReg()
    emit(Inst.Call(Some(r), ty, Val.Global(name), ops))
    r
  }

  /** An integer value moved between two widths, unsigned. Nothing is emitted where they agree,
   * which is the usual case and keeps the IR of a 32-bit count readable.
   */
  protected def resize(v: Val, from: Type, to: Type): Val = {
    val (a, b) = (from.asInstanceOf[Type.Integer].bits, to.asInstanceOf[Type.Integer].bits)

    // A non-negative immediate is its own value at every width it fits in, so **widening** one is a
    // `zext` of a constant that says nothing. The optimizer folds it either way; what this is for is
    // `emit-llvm`, where a rotation by a literal should read as the constant funnel shift it is
    // rather than as a register one whose register is a constant. Narrowing still goes through the
    // instruction, since an immediate too wide for the type it is written at is not IR at all.
    val immediate = v match
      case Val.Int(n) => n >= 0
      case _          => false

    if a == b || (a < b && immediate) then v
    else
      val r = freshReg()
      emit(Inst.Cast(r, if a < b then CastOp.ZExt else CastOp.Trunc, from.lty, v, to.lty))
      r
  }

  /** A rotation amount, brought from the `u32` it is written as to the width being rotated.
   *
   * The funnel-shift intrinsics already take their amount modulo the width, so most of the time
   * this is the resize alone. The exception is a width that is **not a power of two**, where
   * narrowing to it first would take the amount modulo `2^w` and the two reductions do not compose:
   * an `i5` asked to rotate by 33 would answer as though asked for 1, because 33 survives the
   * narrowing to 1 and 5 does not divide 32. So a width like that reduces first, at the amount's
   * own width, where the answer is still the one the program asked for.
   */
  protected def rotateBy(v: Val, from: Type, bits: Int): Val = {
    val reduced =
      if (bits & (bits - 1)) == 0 || from.asInstanceOf[Type.Integer].bits <= bits then v
      else
        val r = freshReg()
        emit(Inst.Bin(r, BinOp.URem, from.lty, v, Val.Int(bits)))
        r

    resize(reduced, from, Type.Integer(bits, signed = false))
  }
}
