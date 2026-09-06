package sh.sysl

import scala.collection.mutable

/** The AST written out in a form built for reading back quickly, and read back into the tree the
 * parser would have produced, which is what `reference/modules.md § Separate compilation` rests on.
 *
 * **Why this exists.** A library's declarations reach a program as an abstract syntax tree, and the
 * only route to one today is the parser — a packrat combinator grammar, which memoizes every rule at
 * every position and is the slow end of practical parsing. Measured when this was written, over the
 * 592 lines of sysl the library then was: 49 ms warm and 316 ms cold, of which lexing is 11 ms, so
 * the grammar is the cost rather than the reading. The library is larger now and headed further that
 * way, and every program pays it on every compilation. This format is the same tree in a shape that
 * costs a single linear pass.
 *
 * **The shape, and why it is fast.** Every node is written as a **tag followed by its children**, and
 * a tag fixes how many children follow — so the reader never looks ahead, never backtracks, and needs
 * no memo table. Where a node holds a list, the count comes first; where it holds an option, a `0` or
 * a `1` does. Nothing is delimited, because nothing has to be: the arity is known before the children
 * are read.
 *
 * **Every string is in a table at the head and referenced by index**, which is what keeps the body to
 * integers and short tags. Identifiers repeat heavily in a tree, so the table is much smaller than the
 * strings it replaces, and a name read twice is the *same* `String` rather than two equal ones. It is
 * also what makes the body safe to split on whitespace: no token can contain any.
 *
 * **Positions are carried** rather than dropped, and the reason is worth stating: a generic library
 * declaration is monomorphized in the *program* that calls it, so the mistake a diagnostic has to
 * report is often in the caller's arguments and the line that explains it is in the library. A tree
 * with no positions could name the call and never the promise it broke. Each source's text is stored
 * alongside its name, so a decoded tree can quote its own lines with nothing else on hand.
 *
 * **What keeps the format in step with the tree.** Every encoder below is an exhaustive `match` on a
 * sealed trait, so a new AST node makes this file fail to compile cleanly rather than silently
 * writing a tree that cannot be read back. `Version` is the other half: it is stamped into the header
 * and refused on mismatch, so an artifact written by an older compiler is regenerated instead of
 * misread. **Bump it whenever a node's shape changes.**
 *
 * The format is textual on purpose, for a first cut. It is readable when something goes wrong, it
 * needs no separate schema, and the byte-level encoding can be swapped for a packed binary one behind
 * `encode`/`decode` without any caller knowing — the structure is the part that is expensive to
 * change, and it is fixed here.
 */
object AstCodec {

  /** The format's version, stamped into the header and checked on the way back in. Bump it whenever
   * the shape of any node changes, so an artifact from an older compiler is rejected rather than
   * read as something it is not.
   *
   * **Two branches bumping to the same number is the one way this check can be defeated**, and it
   * has happened once: a post-test loop and a function attribute were built in parallel, each moved
   * 15 to 16 against a tree the other had not touched, and the merge kept one 16 standing for a
   * format neither branch alone could read. An artifact stamped by either would have passed the
   * check and then been decoded as something it was not. The merge takes the next number for that
   * reason — the value has to be later than every version any compiler has ever stamped, not merely
   * later than the one this branch started from.
   *
   * **It has now happened twice, and this is the merge that took 39.** A block initializer and a
   * struct's C name were built in parallel, each moving 37 to 38 against a tree the other had not
   * touched. Caught by reading dev's value rather than by anything failing — which is the point
   * worth keeping: the collision is invisible to the compiler, because both numbers are perfectly
   * valid on their own branch.
   *
   * **Three times, and 40 is the vector branch's.** `VectorType` moved 37 to 38 in parallel with
   * *both* of the above, so by the time it fetched dev the number it had taken was two behind.
   * This one was caught by `git merge` raising a conflict on the line rather than by anybody
   * reading — which is luck rather than a mechanism, and only happened because the branch that took
   * 39 edited the same line. **Two branches that bump this from different starting values do not
   * conflict**, and that is the case the rule above is written for: read dev's number, take the one
   * after it, and do not assume a clean merge means the versions agree.
   */
  val Version: Int = 54

  private val Magic = "sysl-ast"

  // ---------------------------------------------------------------- encoding

  /** The programs of one library, written out. */
  def encode(programs: List[Program]): String = {
    val enc = new Encoder

    enc.write(programs)
  }

  private class Encoder {
    private val body    = new StringBuilder
    private val strings = mutable.LinkedHashMap.empty[String, Int]
    private val sources = mutable.LinkedHashMap.empty[Source, Int]

    /** A string's index in the table, adding it the first time it is seen. Insertion order is the
     * table's order, so the same tree always writes the same artifact.
     */
    private def str(s: String): Int = strings.getOrElseUpdate(s, strings.size)

    private def src(s: Source): Int = sources.getOrElseUpdate(s, sources.size)

    private def tok(s: String): Unit = { body.append(s); body.append(' ') }
    private def int(n: Int): Unit    = { body.append(n); body.append(' ') }
    private def bool(b: Boolean): Unit = tok(if b then "1" else "0")
    private def sref(s: String): Unit  = int(str(s))
    private def big(n: BigInt): Unit   = tok(n.toString)

    private def opt[A](o: Option[A])(f: A => Unit): Unit = o match
      case None    => tok("0")
      case Some(a) => tok("1"); f(a)

    private def list[A](xs: List[A])(f: A => Unit): Unit = { int(xs.length); xs.foreach(f) }

    /** A map is written in key order so that one tree always produces one artifact — `Map` itself
     * has no order, and an artifact that changed between runs could not be cached or diffed.
     */
    private def map[A](m: Map[String, A])(f: A => Unit): Unit =
      list(m.toList.sortBy(_._1)) { (k, v) => sref(k); f(v) }

    /** Where a diagnostic about the node should point, which every `Positioned` carries and a
     * synthesized node may lack.
     *
     * **A node's `extent` is deliberately not written, and a decoded node falls back to `pos` for
     * it.** What positions are *in* an artifact for is a diagnostic quoting the library, and that
     * is `pos`; an extent answers what an editor asks — which construct is the cursor inside — of a
     * file the editor has open, which is parsed from source rather than read back from here. Adding
     * it is a version bump and five more integers on every node, and the work that would read them
     * does not exist yet. The commit that indexes the library for a language server is the one to
     * reconsider this in.
     */
    private def pos(p: Positioned): Unit = p.pos match
      case None                             => tok("0")
      case Some(Pos(s, ln, cl, endLn, endCl)) =>
        tok("1"); int(src(s)); int(ln); int(cl); int(endLn); int(endCl)

    def write(programs: List[Program]): String = {
      // The tables are filled while the body is written, so the header can only be assembled once
      // the body is finished — which is why this is not streamed.
      programs.foreach(program)

      // A source's name, text and directory segments are themselves strings, and interning one
      // grows the table. They are all claimed here, before the table's size is written, so the
      // count at the head and the entries under it cannot disagree.
      for s <- sources.keys do
        str(s.name)
        str(s.text)
        s.dir.foreach(_.foreach(str))

      val out = new StringBuilder

      out.append(s"$Magic $Version\n")
      out.append(s"${strings.size}\n")
      strings.keys.foreach(s => out.append(s"${s.length}:$s\n"))
      out.append(s"${sources.size}\n")

      for s <- sources.keys do
        out.append(s"${strings(s.name)} ${strings(s.text)} ")
        s.dir match
          case None       => out.append("0\n")
          case Some(segs) => out.append(s"1 ${segs.length} ${segs.map(strings).mkString(" ")}\n")

      out.append(s"${programs.length}\n")
      out.append(body)
      out.toString
    }

    private def program(p: Program): Unit = {
      int(src(p.source))
      opt(p.module)(m => { pos(m); list(m.parts)(sref) })
      list(p.capabilities)(c => { pos(c); tok(if c.direction == CapabilityDirection.Narrows then "no" else "req"); sref(c.name) })
      // A library's link directives travel with it. Without this a binding works from source and
      // stops working the moment it ships as an artifact — which is the worst shape the bug could
      // take, since the build that breaks is the one nobody ran.
      list(p.links)(l => { pos(l); sref(l.name) })
      // `@tests` travels for completeness rather than for use: `LibraryArtifact.build` drops such a
      // file before any of this runs, which is what keeps a package's scaffolding out of what it
      // ships. Writing the flag anyway is what keeps this a *codec* — a tree that came back
      // different from the one that went in is the failure `Version` exists to catch, and a field
      // silently defaulted is exactly that failure with nobody to notice it.
      bool(p.testOnly)
      list(p.body)(stmt)
      body.append('\n')
    }

    // -------------------------------------------------------------- pieces

    private def param(p: Param): Unit = {
      // `byName` is carried because it is not recoverable from the type: `x: -> T` and `x: () -> T`
      // are the same `Fn() -> T`, and only this says which of the two a caller was written against.
      // `rest` is carried for `byName`'s reason: `xs: ...T` and `xs: []const T` are the same
      // parameter type, and only this says which of the two a caller was written against.
      pos(p); sref(p.name); typ(p.typ); vis(p.vis); opt(p.default)(expr); bool(p.byName); bool(p.rest)
    }

    private def bound(b: BoundRef): Unit = { pos(b); sref(b.name); list(b.args)(typ) }

    private def assocDecl(a: AssocDecl): Unit = { pos(a); sref(a.name); list(a.bounds)(bound) }

    private def assocBind(a: AssocBind): Unit = { pos(a); sref(a.name); typ(a.typ) }

    private def bounds(m: Map[String, List[BoundRef]]): Unit = map(m)(bs => list(bs)(bound))

    private def tdefaults(m: Map[String, TypeRef]): Unit = map(m)(typ)

    private def vis(v: Visibility): Unit = v match
      case Visibility.Public    => tok("0")
      case Visibility.File      => tok("1")
      case Visibility.Scoped(m) => tok("2"); sref(m)

    // A shipped library carries no tests — `Library.withoutTests` is what drops them, on the way in
    // rather than here. What this is for is the codec's own promise: a tree reads back as the tree
    // that was written, and a field left out silently is how that stops being true.
    private def testAttr(a: TestAttr): Unit = { pos(a); opt(a.display)(sref); bool(a.shouldTrap); opt(a.expected)(sref) }

    /** A hook attribute travels as its `HookKind`'s spelling, which is the word the grammar reads
      * and the one a refusal names — so an artifact and a source say the same thing about the same
      * declaration. It reaches an artifact only where something kept a hook past `Tests.stripSource`,
      * and the round trip is what says the two paths agree.
      */
    private def hookAttr(a: HookAttr): Unit = { pos(a); tok(a.kind.word) }

    private def asmArm(a: AsmArm): Unit = {
      pos(a); list(a.archs)(sref)

      a.body match
        case AsmUnavailable(r)      => tok("0"); sref(r)
        case AsmCode(ls, ops, clbs) => tok("1"); list(ls)(sref); list(ops)(asmOperand); list(clbs)(sref)
    }

    private def asmOperand(o: AsmOperand): Unit = {
      pos(o)
      o.dir match
        case AsmDir.In  => tok("0")
        case AsmDir.Out => tok("1")
      sref(o.name); opt(o.reg)(sref)
    }

    private def recv(r: RecvMode): Unit = r match
      case RecvMode.ByValue   => tok("0")
      case RecvMode.ByPtr     => tok("1")
      case RecvMode.ByRef(sy) => tok("2"); bool(sy)

    private def method(m: MethodDecl): Unit = {
      pos(m)
      sref(m.name)
      opt(m.receiver)(recv)
      bool(m.isProperty)
      list(m.tparams)(sref)
      list(m.params)(param)
      opt(m.retType)(typ)
      list(m.body)(stmt)
      bounds(m.bounds)
      tdefaults(m.tdefaults)
      vis(m.vis)
      bool(m.variadic)
      bool(m.overrides)
      // The four a member may carry, and they travel for the reason `FuncDecl`'s do: each is
      // checked at the **call**, and the calls an artifact is read for are all in the consumer.
      // `@borrows` most sharply of all — a consumer passing a frame-backed slice through a trait
      // object of an artifact's trait is exactly the call the promise licenses.
      list(m.crossing)(sref)
      opt(m.reads)(ns => list(ns)(sref))
      opt(m.writes)(ns => list(ns)(sref))
      list(m.borrows)(sref)
      bool(m.isStatic)
    }

    private def variant(v: EnumVariantDecl): Unit = {
      pos(v); sref(v.name); opt(v.value)(expr); list(v.fields)(param)
    }

    private def arm(a: MatchArm): Unit = {
      pos(a); list(a.patterns)(pattern); opt(a.guard)(expr); list(a.body)(stmt)
    }

    private def lambdaParam(p: LambdaParam): Unit = { pos(p); sref(p.name); opt(p.typ)(typ) }

    private def withField(f: WithField): Unit = { pos(f); sref(f.name); expr(f.value) }

    private def selector(s: ImportSelector): Unit = { pos(s); sref(s.name); opt(s.alias)(sref) }

    private def rangeBound(r: RangeBound): Unit = {
      pos(r); expr(r.lo); expr(r.hi); bool(r.exclusiveHi)
    }

    // -------------------------------------------------------------- types

    private def typ(t: TypeRef): Unit = {
      pos(t)
      t match
        case NamedType(n, args)   => tok("tn"); sref(n); list(args)(typ)
        case PtrType(inner)       => tok("tp"); typ(inner)
        case RefType(inner, sy)   => tok("tr"); typ(inner); bool(sy)
        case WeakType(inner)      => tok("tw"); typ(inner)
        case ArrayType(len, elem, ro) => tok("ta"); opt(len)(expr); typ(elem); bool(ro)
        case VectorType(lanes, elem)  => tok("tvec"); expr(lanes); typ(elem)
        case VolatileType(inner)  => tok("tv"); typ(inner)
        case TupleType(ps, res)   => tok("tt"); list(ps)(typ); bool(res)
        case PackType(n)          => tok("pk"); sref(n)
        case FnType(ps, ret, bar) => tok("tf"); list(ps)(typ); typ(ret); bool(bar)
        case CFnType(ps, ret)     => tok("tc"); list(ps)(typ); typ(ret)
        case ValueArgType(v)      => tok("tva"); expr(v)
        case AssocType(base, m)   => tok("tas"); typ(base); sref(m)
        case AssocArgType(n, t)   => tok("taa"); sref(n); typ(t)
        case SomeType(bs)         => tok("tsome"); list(bs)(bound)
    }

    // ------------------------------------------------------------ patterns

    private def pattern(p: Pattern): Unit = p match
      case LitPattern(v)          => tok("plit"); expr(v)
      case RangePattern(lo, h, i) => tok("prng"); expr(lo); expr(h); bool(i)
      case WildcardPattern        => tok("pwld")
      case IdentPattern(n)        => tok("pid"); sref(n)
      case EqPattern(n)           => tok("peq"); sref(n)
      case VariantPattern(n, as)  => tok("pvar"); sref(n); list(as)(pattern)
      case StructPattern(n, fs)   => tok("pstr"); sref(n); list(fs) { (f, sub) => sref(f); pattern(sub) }
      case TuplePattern(as)       => tok("ptup"); list(as)(pattern)
      case BindPattern(n, inner)  => tok("pat"); sref(n); pattern(inner)

    // --------------------------------------------------------- expressions

    private def expr(e: Expr): Unit = {
      pos(e)
      e match
        case IntLit(v, sfx)          => tok("il"); big(v); opt(sfx)(sref)
        case FloatLit(t, sfx)        => tok("fl"); sref(t); opt(sfx)(sref)
        case CharLit(cp)             => tok("cl"); int(cp)
        case StrLit(v)               => tok("sl"); sref(v)
        case CStrLit(v)              => tok("csl"); sref(v)
        case BoolLit(v)              => tok("bl"); bool(v)
        case UnitLit()               => tok("ul")
        case NullLit()               => tok("nl")
        case Ident(n)                => tok("id"); sref(n)
        case Unary(op, x)            => tok("un"); sref(op); expr(x)
        case PreIncDec(op, x)        => tok("pre"); sref(op); expr(x)
        case PostIncDec(op, x)       => tok("post"); sref(op); expr(x)
        case Binary(op, l, r)        => tok("bin"); sref(op); expr(l); expr(r)
        case Compare(ops, os)        => tok("cmp"); list(ops)(expr); list(os)(sref)
        case RangeExpr(lo, hi, inc)  => tok("rng"); opt(lo)(expr); opt(hi)(expr); bool(inc)
        case Assign(op, t, v)        => tok("asg"); sref(op); expr(t); expr(v)
        case Call(callee, as)        => tok("call"); expr(callee); list(as)(expr)
        case NamedArg(n, v)          => tok("narg"); sref(n); expr(v)
        case DefaultArg(o, v)        => tok("darg"); opt(o)(sref); expr(v)
        case Index(recv, i)          => tok("idx"); expr(recv); expr(i)
        case TypeArgs(recv, as)      => tok("targs"); expr(recv); list(as)(expr)
        case Field(recv, n)          => tok("fld"); expr(recv); sref(n)
        case TypeAttr(recv, a)       => tok("tat"); expr(recv); sref(a)
        case ImplicitMember(n)       => tok("imem"); sref(n)
        case WithExpr(b, fs)         => tok("with"); expr(b); list(fs)(withField)
        case LayoutOf(what, t)       => tok("lay"); sref(what); typ(t)
        case OffsetOf(t, f)          => tok("off"); typ(t); sref(f)
        case TryExpr(x)              => tok("try"); expr(x)
        case Tuple(es)               => tok("tup"); list(es)(expr)
        case Lambda(ps, b, _)        => tok("lam"); list(ps)(lambdaParam); list(b)(stmt)
        case BlockArg(b)             => tok("barg"); list(b)(stmt)
        case ArrayLit(es)            => tok("arr"); list(es)(expr)
        case Spread(v)               => tok("sprd"); expr(v)
        case ArrayFill(v, c)         => tok("afl"); expr(v); expr(c)
        case Block(ss)               => tok("blk"); list(ss)(stmt)
        case IfExpr(c, t, e2)        => tok("if"); expr(c); list(t)(stmt); opt(e2)(b => list(b)(stmt))
        case MatchExpr(s, arms)      => tok("mat"); expr(s); list(arms)(arm)
        case IsPattern(s, ps, neg)   => tok("is"); expr(s); list(ps)(pattern); bool(neg)
        case ResultList(vs)          => tok("rl"); list(vs)(expr)
        case While(l, c, b, e2)      => tok("whl"); opt(l)(sref); expr(c); list(b)(stmt); opt(e2)(x => list(x)(stmt))
        case DoWhile(l, b, c, e2)    => tok("dwl"); opt(l)(sref); list(b)(stmt); expr(c); opt(e2)(x => list(x)(stmt))
        case Loop(l, b)              => tok("lop"); opt(l)(sref); list(b)(stmt)
        case For(l, n, it, b, e2)    => tok("for"); opt(l)(sref); sref(n); expr(it); list(b)(stmt); opt(e2)(x => list(x)(stmt))
        case ConstFor(n, it, b)      => tok("ufor"); sref(n); expr(it); list(b)(stmt)
        case CFor(l, i, c, s, b, e2) =>
          tok("cfor"); opt(l)(sref); opt(i)(stmt); opt(c)(expr); opt(s)(stmt); list(b)(stmt)
          opt(e2)(x => list(x)(stmt))
        case Quantifier(u, n, it, p) => tok("qnt"); bool(u); sref(n); expr(it); expr(p)
    }

    // ---------------------------------------------------------- statements

    private def stmt(s: Stmt): Unit = {
      pos(s)
      s match
        case ImportDecl(path, sels, wild, alias) =>
          tok("imp"); list(path)(sref); list(sels)(selector); bool(wild); opt(alias)(sref)
        // A section travels for the reason a layout does, one line below: it is a property of the
        // storage this declaration lays down, so a program reading the declaration back has to lay it
        // down in the same place the library's own build would have.
        case VarDecl(n, t, i, vs, al, sc) =>
          tok("var"); sref(n); opt(t)(typ); opt(i)(expr); vis(vs); opt(al)(expr); opt(sc)(sref)
        case ConstDecl(n, t, v, vs)       => tok("cst"); sref(n); typ(t); expr(v); vis(vs)
        case ValDecl(n, t, v, vs, al, sc) =>
          tok("val"); sref(n); opt(t)(typ); expr(v); vis(vs); opt(al)(expr); opt(sc)(sref)
        // No token, and no version bump to give it one: `static` is legal only in the file a program
        // starts in, a library has no such file, and the analyzer has already said so by the time
        // anything is encoded. Reaching here would mean that check stopped running.
        case StaticDecl(_) =>
          sys.error("a 'static' declaration reached a library artifact, which is a file a program " +
            "starts in reaching one — the analyzer refuses 'static' everywhere else")
        // No token either, and for a stronger reason than `static`'s: a `c const` is *lowered* to an
        // ordinary constant before a library is analyzed or encoded (`CProbe`), so what an
        // artifact carries is the measured value. Reaching here means a tree skipped that lowering,
        // and the loud failure is the point — the quiet alternative is a library shipping without
        // constants a program is about to name.
        case _: CConstBlock =>
          sys.error("a 'c const' block reached a library artifact — it is lowered to an ordinary " +
            "constant before anything is encoded, so this is a path that skipped 'CProbe.lower'")
        // The same again for the type half, measured by the same probe and lowered by the same pass.
        // What an artifact carries is the `TypeDecl` the measurement produced, which is the only
        // honest thing to ship: an artifact is built for one target, and re-measuring the typedef
        // somewhere else would be answering a different question under the same name.
        case _: CTypeBlock =>
          sys.error("a 'c type' block reached a library artifact — it is lowered to a type " +
            "declaration before anything is encoded, so this is a path that skipped 'CProbe.lower'")
        case RefDecl(n, p)                => tok("ref"); sref(n); expr(p)
        case MultiAssign(op, ts, vs)      => tok("masg"); sref(op); list(ts)(expr); list(vs)(expr)
        case MultiDecl(ns, mut, vs)       => tok("mdcl"); list(ns)(sref); bool(mut); list(vs)(expr)
        case PatternDecl(p, mut, v)       => tok("pdcl"); pattern(p); bool(mut); expr(v)
        case ExprStmt(x)                  => tok("es"); expr(x)
        case Return(v)                    => tok("ret"); opt(v)(expr)
        case Become(c)                    => tok("bcm"); expr(c)
        case Break(l, v)                  => tok("brk"); opt(l)(sref); opt(v)(expr)
        case Continue(l)                  => tok("cnt"); opt(l)(sref)
        case Defer(s)                     => tok("dfr"); stmt(s)
        case AsmStmt(arms)                => tok("asm"); list(arms)(asmArm)
        // Carried rather than dropped, although the library's own build has already settled it. A
        // consumer re-checks it against the target *it* is building for, which is the answer that
        // matters: `sizeof` is per target, so a layout a library verified on one machine is not a
        // layout verified on the consumer's.
        case AssertDecl(c, m)             => tok("asrt"); expr(c); opt(m)(sref)
        case Require(c, m)                => tok("req"); expr(c); opt(m)(sref)
        case Ensure(c, m)                 => tok("ens"); expr(c); opt(m)(sref)
        case Invariant(c, m)              => tok("inv"); expr(c); opt(m)(sref)
        case Variant(e)                   => tok("vnt"); expr(e)

        case FuncDecl(n, tps, ps, rt, b, bs, va, vs, tds, tvs, tpk, t, hk, cv, tr, pu, gh, rd, wr, ex, sc,
                      cr, nd) =>
          tok("fn"); sref(n); list(tps)(sref); list(ps)(param); opt(rt)(typ); list(b)(stmt)
          bounds(bs); bool(va); vis(vs); tdefaults(tds); tdefaults(tvs); list(tpk.toList)(sref)
          opt(t)(testAttr); opt(hk)(hookAttr)
          opt(cv)(c => { pos(c); sref(c.name); opt(c.arg)(sref) }); bool(tr); bool(pu); bool(gh)
          // A frame is carried as written rather than as resolved: an archive holds declarations, and
          // the names are resolved against the importing program's view exactly as the body's are.
          opt(rd)(ns => list(ns)(sref)); opt(wr)(ns => list(ns)(sref))
          // `@export` travels for the reason a layout does: a module has to publish the same symbol
          // whether it was compiled from source or read back from an artifact, and one that dropped
          // the mark would quietly mangle the name again.
          opt(ex)(e => { pos(e); opt(e.symbol)(sref) })
          // And `@section` for the same reason: a definition the library placed somewhere is placed
          // there in the program that reads it back, or the linker script gathers nothing.
          opt(sc)(sref)
          // `@crossing` travels because the check it asks for is made at the **call**, and the calls
          // an artifact is read for are in the consumer. A declaration that dropped it would be the
          // same signature with the rule silently off for everybody but the library's own tests.
          list(cr)(sref)
          // `@needs` travels for the reason `@crossing` does, and more sharply: the check it asks
          // for is made at the **call**, and the calls an artifact is read for are all in the
          // consumer. A declaration that dropped it would be a capability requirement that held
          // inside the library and nowhere else.
          list(nd)(sref)

        case ExternDecl(n, ps, rt, va, lk, vs, nd) =>
          tok("ext"); sref(n); list(ps)(param); opt(rt)(typ); bool(va); opt(lk)(sref); vis(vs)
          list(nd)(sref)

        case ExternVarDecl(n, t, lk, vs) =>
          tok("extv"); sref(n); typ(t); opt(lk)(sref); vis(vs)

        case StructDecl(n, tps, fs, ms, bs, invs, vs, tds, op, tvs, pk, al, cn, dv) =>
          tok("sd"); sref(n); list(tps)(sref); list(fs)(param); list(ms)(method)
          bounds(bs); list(invs)(expr); vis(vs); tdefaults(tds); bool(op); tdefaults(tvs)
          // A layout travels with the declaration: an importing module computes an instantiation's
          // layout for itself (`reference/generics.md § Monomorphization`), so a struct whose
          // padding or alignment an attribute decided would otherwise be laid out two different
          // ways either side of an artifact.
          bool(pk); opt(al)(expr)
          // And the C name travels for the reason a function's exported symbol does: the header a
          // consumer generates has to spell a package's type the way the package chose, whether it
          // was compiled from source or read back from an artifact.
          opt(cn)(e => { pos(e); opt(e.symbol)(sref) })
          // The clause travels rather than the blocks it makes, because expansion happens once, on
          // the way in to the walk — so an artifact holding the clause and an artifact holding the
          // source both arrive at the same program.
          list(dv)(bound)

        case EnumDecl(n, tps, und, vars, ms, bs, vs, tds, tvs, dv) =>
          tok("ed"); sref(n); list(tps)(sref); opt(und)(typ); list(vars)(variant); list(ms)(method)
          bounds(bs); vis(vs); tdefaults(tds); tdefaults(tvs); list(dv)(bound)

        case TypeDecl(n, base, der, rng, pred, vs, fromC) =>
          tok("td"); sref(n); typ(base); bool(der); opt(rng)(rangeBound); opt(pred)(expr); vis(vs)
          bool(fromC)

        case TraitDecl(n, tps, ms, bs, sups, vs, tds, as) =>
          tok("trt"); sref(n); list(tps)(sref); list(ms)(method); bounds(bs); list(sups)(bound)
          vis(vs); tdefaults(tds); list(as)(assocDecl)

        case ImplDecl(tn, ft, ms, tps, bs, targs, tds, ov, tvs, tpk, as) =>
          tok("impl"); sref(tn); typ(ft); list(ms)(method); list(tps)(sref); bounds(bs)
          list(targs)(typ); tdefaults(tds); bool(ov); tdefaults(tvs); list(tpk.toList)(sref)
          list(as)(assocBind)
    }
  }

  // ---------------------------------------------------------------- decoding

  /** Reads back what `encode` wrote, rebuilding the `Source` objects from the names and text the
   * artifact carries. This is the ordinary path: a library artifact is self-contained.
   */
  def decode(text: String): Either[String, List[Program]] = decode(text, Map.empty)

  /** The same, with named sources supplied by the caller. A name present in `known` binds to that
   * `Source` object rather than to a reconstructed one, which is what lets a round-trip test compare
   * positions — a `Pos` holds its `Source`, and sources compare by identity.
   */
  def decode(text: String, known: Map[String, Source]): Either[String, List[Program]] =
    try Right(new Decoder(text, known).read())
    catch case e: CodecError => Left(e.getMessage)

  private class CodecError(msg: String) extends RuntimeException(msg)

  private class Decoder(text: String, known: Map[String, Source]) {
    private var i = 0

    private def fail(what: String): Nothing = throw new CodecError(what)

    private def line(): String = {
      val start = text.indexOf('\n', i)

      if start < 0 then fail("the artifact ends in the middle of its header")

      val s = text.substring(i, start)

      i = start + 1
      s
    }

    private def skipWs(): Unit = while i < text.length && text.charAt(i) <= ' ' do i += 1

    /** One whitespace-delimited token. Tags are the only thing read this way; an integer goes
     * through `int`, which reads its digits without building a `String` for them.
     */
    private def tok(): String = {
      skipWs()

      val start = i

      while i < text.length && text.charAt(i) > ' ' do i += 1

      if i == start then fail("the artifact ends where a value was expected")

      text.substring(start, i)
    }

    private def int(): Int = {
      skipWs()

      if i >= text.length then fail("the artifact ends where a number was expected")

      var n   = 0
      var neg = false

      if text.charAt(i) == '-' then { neg = true; i += 1 }

      val start = i

      while i < text.length && text.charAt(i) > ' ' do
        val c = text.charAt(i)

        if c < '0' || c > '9' then fail(s"'${text.substring(start, i + 1)}' is not a number")

        n = n * 10 + (c - '0')
        i += 1

      if i == start then fail("the artifact ends where a number was expected")

      if neg then -n else n
    }

    private def bool(): Boolean = int() == 1
    private def big(): BigInt   = BigInt(tok())

    private var strings: Array[String] = Array.empty
    private var sources: Array[Source] = Array.empty

    private def sref(): String = {
      val n = int()

      if n < 0 || n >= strings.length then fail(s"string $n is not in the artifact's table")

      strings(n)
    }

    private def opt[A](f: => A): Option[A] = if bool() then Some(f) else None

    private def list[A](f: => A): List[A] = {
      val n = int()

      if n < 0 then fail("a negative length")

      val b = List.newBuilder[A]
      var k = 0

      while k < n do { b += f; k += 1 }

      b.result()
    }

    private def map[A](f: => A): Map[String, A] =
      list { val k = sref(); (k, f) }.toMap

    /** Reads a node's position, then the node, then stamps the one onto the other.
     *
     * **The node is by name because the order matters**: the encoder writes a position ahead of the
     * fields it belongs to, so the position has to be read first — and an argument passed by value
     * would have consumed the fields before this method ran. `setPos` keeps the first position it is
     * given and a freshly built node has none, so what the artifact recorded is what it ends up with.
     */
    private def at[A <: Positioned](build: => A): A = {
      val present = bool()
      val s       = if present then int() else 0
      val ln      = if present then int() else 0
      val cl      = if present then int() else 0
      val endLn   = if present then int() else 0
      val endCl   = if present then int() else 0
      val node    = build

      if present then
        if s < 0 || s >= sources.length then fail(s"source $s is not in the artifact's table")

        node.setPos(Pos(sources(s), ln, cl, endLn, endCl))
      else node
    }

    def read(): List[Program] = {
      val header = line().split(" ")

      if header.length != 2 || header(0) != Magic then fail("this is not a sysl AST artifact")

      val version = header(1).toIntOption.getOrElse(fail(s"'${header(1)}' is not a version"))

      if version != Version then
        fail(s"the artifact is version $version and this compiler writes version $Version — regenerate it")

      strings = Array.fill(line().toIntOption.getOrElse(fail("the string count is not a number")))("")

      for k <- strings.indices do
        val raw   = line()
        val colon = raw.indexOf(':')

        if colon < 0 then fail(s"string $k has no length")

        val len = raw.substring(0, colon).toIntOption.getOrElse(fail(s"string $k has no length"))

        // A string may hold newlines, which the line above will have cut short — the recorded
        // length is what says where it really ends, so the rest is taken from the raw text.
        val from = i - raw.length + colon
        val to   = from + len

        if to > text.length then fail(s"string $k runs past the end of the artifact")

        strings(k) = text.substring(from, to)
        i = to + 1

      sources = Array.fill(line().toIntOption.getOrElse(fail("the source count is not a number")))(null)

      for k <- sources.indices do
        val parts = line().split(" ")
        val name  = strings(parts(0).toInt)
        val body  = strings(parts(1).toInt)
        val dir   = if parts(2) == "1" then Some(parts.drop(4).toList.map(x => strings(x.toInt))) else None

        // What was stored is the text the *positions* were recorded against, which for a literate
        // file is its program with the left margin already gone (`Literate`). The margin is how far
        // a reported column has to move to name the file again, and it is read back off the name for
        // the same reason it is read off the name anywhere else — nothing inside the text says.
        sources(k) =
          known.getOrElse(name, new Source(name, body, dir, if Literate.named(name) then Literate.Indent else 0))

      val count = line().toIntOption.getOrElse(fail("the program count is not a number"))
      val b     = List.newBuilder[Program]

      for _ <- 0 until count do b += program()

      b.result()
    }

    private def program(): Program = {
      val s = int()

      if s < 0 || s >= sources.length then fail(s"source $s is not in the artifact's table")

      val module = opt(at(ModuleName(list(sref()))))
      val caps = list(at(tok() match
        case "no"  => CapabilityClause(CapabilityDirection.Narrows, sref())
        case "req" => CapabilityClause(CapabilityDirection.Requires, sref())
        case t     => fail(s"'$t' is not a capability clause's direction")))
      val links = list(at(LinkClause(sref())))
      val tests = bool()

      Program(list(stmt()), module, caps, links, sources(s), tests)
    }

    // -------------------------------------------------------------- pieces

    private def param(): Param  = at(Param(sref(), typ(), vis(), opt(expr()), bool(), bool()))
    private def bound(): BoundRef = at(BoundRef(sref(), list(typ())))
    private def assocDecl(): AssocDecl = at(AssocDecl(sref(), list(bound())))
    private def assocBind(): AssocBind = at(AssocBind(sref(), typ()))
    private def bounds(): Map[String, List[BoundRef]] = map(list(bound()))
    private def tdefaults(): Map[String, TypeRef]     = map(typ())

    private def vis(): Visibility = tok() match
      case "0"   => Visibility.Public
      case "1"   => Visibility.File
      case "2"   => Visibility.Scoped(sref())
      case other => fail(s"'$other' is not a visibility")

    private def testAttr(): TestAttr = at(TestAttr(opt(sref()), bool(), opt(sref())))

    private def hookAttr(): HookAttr = at {
      val word = tok()

      HookAttr(HookKind.values.find(_.word == word)
        .getOrElse(fail(s"'$word' is not one of the hooks a module may write")))
    }

    private def asmArm(): AsmArm = at {
      val archs = list(sref())

      AsmArm(archs, tok() match
        case "0"   => AsmUnavailable(sref())
        case "1"   => AsmCode(list(sref()), list(asmOperand()), list(sref()))
        case other => fail(s"'$other' is not an assembly arm"))
    }

    private def asmOperand(): AsmOperand = at {
      val dir = tok() match
        case "0"   => AsmDir.In
        case "1"   => AsmDir.Out
        case other => fail(s"'$other' is not an assembly operand direction")

      AsmOperand(dir, sref(), opt(sref()))
    }

    private def recv(): RecvMode = tok() match
      case "0"   => RecvMode.ByValue
      case "1"   => RecvMode.ByPtr
      case "2"   => RecvMode.ByRef(bool())
      case other => fail(s"'$other' is not a receiver mode")

    // The three trailing fields are named rather than positional because `tvalues` and `tpacks`
    // sit between them and `overrides` in the constructor and are not encoded — a member's are
    // solved where it is lowered rather than carried.
    private def method(): MethodDecl =
      at(MethodDecl(sref(), opt(recv()), bool(), list(sref()), list(param()), opt(typ()),
        list(stmt()), bounds(), tdefaults(), vis(), bool(), bool(),
        crossing = list(sref()), reads = opt(list(sref())), writes = opt(list(sref())),
        borrows = list(sref()), isStatic = bool()))

    private def variant(): EnumVariantDecl =
      at(EnumVariantDecl(sref(), opt(expr()), list(param())))

    private def arm(): MatchArm = at(MatchArm(list(pattern()), opt(expr()), list(stmt())))

    private def lambdaParam(): LambdaParam = at(LambdaParam(sref(), opt(typ())))

    private def withField(): WithField = at(WithField(sref(), expr()))

    private def selector(): ImportSelector = at(ImportSelector(sref(), opt(sref())))

    private def rangeBound(): RangeBound = at(RangeBound(expr(), expr(), bool()))

    // -------------------------------------------------------------- types

    private def typ(): TypeRef = at {
      tok() match
        case "tn"  => NamedType(sref(), list(typ()))
        case "tp"  => PtrType(typ())
        case "tr"  => RefType(typ(), bool())
        case "tw"  => WeakType(typ())
        case "ta"  => ArrayType(opt(expr()), typ(), bool())
        case "tvec" => VectorType(expr(), typ())
        case "tv"  => VolatileType(typ())
        case "tt"  => TupleType(list(typ()), bool())
        case "pk"  => PackType(sref())
        case "tf"  => FnType(list(typ()), typ(), bool())
        case "tc"  => CFnType(list(typ()), typ())
        case "tva" => ValueArgType(expr())
        case "tas" => AssocType(typ(), sref())
        case "taa" => AssocArgType(sref(), typ())
        case "tsome" => SomeType(list(bound()))
        case other => fail(s"'$other' is not a type tag")
    }

    // ------------------------------------------------------------ patterns

    private def pattern(): Pattern = tok() match
      case "plit" => LitPattern(expr())
      case "prng" => RangePattern(expr(), expr(), bool())
      case "pwld" => WildcardPattern
      case "pid"  => IdentPattern(sref())
      case "peq"  => EqPattern(sref())
      case "pvar" => VariantPattern(sref(), list(pattern()))
      case "pstr" => StructPattern(sref(), list { val f = sref(); (f, pattern()) })
      case "ptup" => TuplePattern(list(pattern()))
      case "pat"  => BindPattern(sref(), pattern())
      case other  => fail(s"'$other' is not a pattern tag")

    // --------------------------------------------------------- expressions

    private def expr(): Expr = at {
      tok() match
        case "il"   => IntLit(big(), opt(sref()))
        case "fl"   => FloatLit(sref(), opt(sref()))
        case "cl"   => CharLit(int())
        case "sl"   => StrLit(sref())
        case "csl"  => CStrLit(sref())
        case "bl"   => BoolLit(bool())
        case "ul"   => UnitLit()
        case "nl"   => NullLit()
        case "id"   => Ident(sref())
        case "un"   => Unary(sref(), expr())
        case "pre"  => PreIncDec(sref(), expr())
        case "post" => PostIncDec(sref(), expr())
        case "bin"  => Binary(sref(), expr(), expr())
        case "cmp"  => Compare(list(expr()), list(sref()))
        case "rng"  => RangeExpr(opt(expr()), opt(expr()), bool())
        case "asg"  => Assign(sref(), expr(), expr())
        case "call" => Call(expr(), list(expr()))
        case "narg" => NamedArg(sref(), expr())
        case "darg" => DefaultArg(opt(sref()), expr())
        case "idx"   => Index(expr(), expr())
        case "targs" => TypeArgs(expr(), list(expr()))
        case "fld"  => Field(expr(), sref())
        case "tat"  => TypeAttr(expr(), sref())
        case "imem" => ImplicitMember(sref())
        case "with" => WithExpr(expr(), list(withField()))
        case "lay"  => LayoutOf(sref(), typ())
        case "off"  => OffsetOf(typ(), sref())
        case "try"  => TryExpr(expr())
        case "tup"  => Tuple(list(expr()))
        case "lam"  => Lambda(list(lambdaParam()), list(stmt()))
        case "barg" => BlockArg(list(stmt()))
        case "arr"  => ArrayLit(list(expr()))
        case "sprd" => Spread(expr())
        case "afl"  => ArrayFill(expr(), expr())
        case "blk"  => Block(list(stmt()))
        case "if"   => IfExpr(expr(), list(stmt()), opt(list(stmt())))
        case "mat"  => MatchExpr(expr(), list(arm()))
        case "is"   => IsPattern(expr(), list(pattern()), bool())
        case "rl"   => ResultList(list(expr()))
        case "whl"  => While(opt(sref()), expr(), list(stmt()), opt(list(stmt())))
        case "dwl"  => DoWhile(opt(sref()), list(stmt()), expr(), opt(list(stmt())))
        case "lop"  => Loop(opt(sref()), list(stmt()))
        case "for"  => For(opt(sref()), sref(), expr(), list(stmt()), opt(list(stmt())))
        case "ufor" => ConstFor(sref(), expr(), list(stmt()))
        case "cfor" => CFor(opt(sref()), opt(stmt()), opt(expr()), opt(stmt()), list(stmt()), opt(list(stmt())))
        case "qnt"  => Quantifier(bool(), sref(), expr(), expr())
        case other  => fail(s"'$other' is not an expression tag")
    }

    // ---------------------------------------------------------- statements

    private def stmt(): Stmt = at {
      tok() match
        case "imp"  => ImportDecl(list(sref()), list(selector()), bool(), opt(sref()))
        case "var"  => VarDecl(sref(), opt(typ()), opt(expr()), vis(), opt(expr()), opt(sref()))
        case "cst"  => ConstDecl(sref(), typ(), expr(), vis())
        case "val"  => ValDecl(sref(), opt(typ()), expr(), vis(), opt(expr()), opt(sref()))
        case "ref"  => RefDecl(sref(), expr())
        case "masg" => MultiAssign(sref(), list(expr()), list(expr()))
        case "mdcl" => MultiDecl(list(sref()), bool(), list(expr()))
        case "pdcl" => PatternDecl(pattern(), bool(), expr())
        case "es"   => ExprStmt(expr())
        case "ret"  => Return(opt(expr()))
        case "bcm"  => Become(expr())
        case "brk"  => Break(opt(sref()), opt(expr()))
        case "cnt"  => Continue(opt(sref()))
        case "dfr"  => Defer(stmt())
        case "asm"  => AsmStmt(list(asmArm()))
        case "asrt" => AssertDecl(expr(), opt(sref()))
        case "req"  => Require(expr(), opt(sref()))
        case "ens"  => Ensure(expr(), opt(sref()))
        case "inv"  => Invariant(expr(), opt(sref()))
        case "vnt"  => Variant(expr())
        case "fn" =>
          FuncDecl(sref(), list(sref()), list(param()), opt(typ()), list(stmt()),
            bounds(), bool(), vis(), tdefaults(), tdefaults(), list(sref()).toSet, opt(testAttr()),
            opt(hookAttr()),
            opt(at(CallConv(sref(), opt(sref())))), bool(), bool(), bool(),
            opt(list(sref())), opt(list(sref())), opt(at(ExportAttr(opt(sref())))), opt(sref()),
            list(sref()), list(sref()))
        case "ext" =>
          ExternDecl(sref(), list(param()), opt(typ()), bool(), opt(sref()), vis(), list(sref()))
        case "extv" =>
          ExternVarDecl(sref(), typ(), opt(sref()), vis())
        case "sd" =>
          StructDecl(sref(), list(sref()), list(param()), list(method()),
            bounds(), list(expr()), vis(), tdefaults(), bool(), tdefaults(), bool(), opt(expr()),
            opt(at(ExportAttr(opt(sref())))), list(bound()))
        case "ed" =>
          EnumDecl(sref(), list(sref()), opt(typ()), list(variant()), list(method()),
            bounds(), vis(), tdefaults(), tdefaults(), list(bound()))
        case "td" =>
          TypeDecl(sref(), typ(), bool(), opt(rangeBound()), opt(expr()), vis(), bool())
        case "trt" =>
          TraitDecl(sref(), list(sref()), list(method()), bounds(), list(bound()), vis(), tdefaults(),
            list(assocDecl()))
        case "impl" =>
          ImplDecl(sref(), typ(), list(method()), list(sref()), bounds(), list(typ()), tdefaults(), bool(),
            tdefaults(), list(sref()).toSet, list(assocBind()))
        case other => fail(s"'$other' is not a statement tag")
    }
  }
}
