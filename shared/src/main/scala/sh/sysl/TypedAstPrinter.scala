package sh.sysl

/** The *typed* tree, written out as deterministic, human-readable text — `sysl emit-typed`'s whole
 * job, and the analysed counterpart of `AstPrinter`'s `emit-ast`.
 *
 * **Why this exists.** A second compiler is being written for sysl, in sysl
 * (`~/dev/sysl-lang/sysl`), and the cheapest way to know the two analyzers agree is to diff what
 * each one built for the same file. `emit-ast` already lets the two *parsers* be compared before
 * either one resolves a name; this is the same idea one stage later, once every name is bound,
 * every type is solved and every rule that could fail (unknown name, type mismatch, wrong arity)
 * has already been checked. It runs the whole front end — parsing, then analysis against the
 * standard module exactly as `sysl emit-llvm` loads it — and stops there: no pruning, no lowering,
 * no codegen, no clang. `Compiler.typedWith` is that same stopping point; `sysl prove` is the other
 * command that already stands on it.
 *
 * **The format is `AstPrinter`'s, unchanged**: one node per line, its type name and (where it
 * carries one) its span, then every field two spaces deeper as `fieldName: value`. A list of
 * scalars prints inline, a list of nodes as `- ` items, a map sorted by key. See `AstPrinter`'s own
 * header for the full grammar — nothing about it differs here except what a *node* is.
 *
 * **The one real difference is `Type`.** Every typed node carries at least one resolved `Type` —
 * an expression's `ty`, a pattern's, a function's parameter and return types — and printing one
 * structurally, field by field, would be both the wrong level of detail (an oracle wants to know a
 * call resolved to `add(int, int) -> int`, not the LLVM layout underneath it) and often impossible:
 * `Type.Struct` and `Type.Enum` are plain mutable classes rather than case classes, holding no
 * `Product` a reflective walk could read. So a `Type` value anywhere in the tree — a field, an
 * element of a `List[Type]`, a map's value — is rendered as one line of the compiler's own
 * diagnostic text, `Type.show`: `i32`, `a slice of u8`, `Point`, `a closure`. That is also the
 * right level for a diff: two analyzers that solved a call to the same overload should agree on
 * what `Type.show` says about it, whether or not they represent a type the same way internally.
 *
 * **Completeness is structural for everything else, exactly as `AstPrinter` argues.** Every field
 * of every node outside the four sealed hierarchies below is read off it with
 * `productElementNames`/`productIterator`. What stays hand-written is one **exhaustive match per
 * hierarchy** — `TExpr` (`tast.scala`), `TStmt` (`tastStmts.scala`), `TPattern`
 * (`tastPatterns.scala`), `TCondTerm` (`tast.scala`) — naming every case with no wildcard arm, so a
 * node kind added to one of those three files without a matching line here is a compiler warning on
 * this file. `TArm`, `TBlock`, `TFunc`, `TVtable`, `TVSlot`, `TVal`, `TEntry`, `THook`, `THooks`,
 * `TAsmOperand`, `TWrite`, `TDispatch`, `TCmp`, `TProgram` and the rest have no sealed hierarchy of
 * their own — like `AstPrinter`'s `Param`/`MethodDecl`/`BoundRef` — and are read generically.
 *
 * **`RecvMode`, `AsmDir` and `HookKind` are matched by hand in `tag`**, for the identical reason
 * `AstPrinter.tag`'s own comment gives: a parameterless `enum` case's `getClass.getSimpleName`
 * disagrees between the JVM, Scala.js and Scala Native (`scala-native/scala-native#5030`), and these
 * three enums are shared with the untyped tree, so the same three backends the golden test there
 * pins are exactly the ones that would diverge here too.
 */
object TypedAstPrinter {

  /** One module's typed tree, printed. `spans` is false for `emit-typed --no-spans`, which drops
   * every node's source span so a diff does not move when a line does — the same trade `emit-ast
   * --no-spans` makes, for the same reason.
   */
  def print(program: TProgram, spans: Boolean = true): String = {
    val printer = new Printer(spans)

    printer.top(program)
    printer.result
  }

  /** `emit-typed --tables` — the module's declaration tables rather than its tree: every struct,
   * enum, trait implementation, extern, module `val` and function `TProgram` carries, sorted by
   * name, with every field and parameter's type resolved through `Type.show`.
   *
   * This is not a second walk of the same nodes: a struct or an enum never appears as a *statement*
   * in the typed tree the way it does in the untyped one (`StructDecl`/`EnumDecl` are `Stmt` cases
   * that `emit-ast` prints; there is no `TStructDecl`/`TEnumDecl`) — by the time a tree is typed,
   * a struct or an enum has become a `Type.Struct`/`Type.Enum` value, held in `TProgram.structs` and
   * `.enums` rather than standing as a node of its own. A second implementation's analyzer builds
   * exactly these tables before it can type a single expression, which is what this is for: a diff
   * over the tables settles whether the two analyzers agree on what a module *declares*, before
   * either one has to agree on how it *executes*.
   *
   * A trait's own declaration and a type alias carry no separate entry — the typed tree keeps
   * neither: a trait survives only as the `TVtable`s its `impl`s produced, and an alias is
   * substituted away during analysis and leaves nothing to key a table row on. The `traits:`
   * section below is grouped from `vtables` for that reason, and there is no `aliases:` section at
   * all.
   */
  def tables(program: TProgram): String = Tables.render(program)

  /** A double-quoted string with control characters, quotes and backslashes escaped — `AstPrinter
   * .quote`'s twin, kept local rather than shared because the two printers otherwise touch no code
   * in common.
   */
  private def quote(s: String): String = {
    val b = new StringBuilder("\"")

    s.foreach {
      case '"'          => b.append("\\\"")
      case '\\'         => b.append("\\\\")
      case '\n'         => b.append("\\n")
      case '\r'         => b.append("\\r")
      case '\t'         => b.append("\\t")
      case c if c < ' ' => b.append(f"\\u${c.toInt}%04x")
      case c            => b.append(c)
    }

    b.append('"')
    b.toString
  }

  /** A scalar the typed tree can hold: `AstPrinter`'s four widened by `Long` (`TFloatLit.bits`) and
   * by `Type` itself, so a `List[Type]` — a function's parameters, an extern's — prints as one
   * inline line rather than one node per line.
   */
  private def isScalar(v: Any): Boolean = v match
    case _: String | _: Boolean | _: Int | _: Long | _: BigInt | _: Type => true
    case _                                                                => false

  private def scalarText(v: Any): String = v match
    case s: String => quote(s)
    case t: Type   => Type.show(t)
    case other     => other.toString

  /** The whole of the state one `print` call carries, for the reason `AstPrinter.Printer` gives:
   * every method below is a step of one walk, and an instance field says it more plainly than a
   * parameter threaded through each of them would.
   */
  private final class Printer(spans: Boolean) {
    private val sb = new StringBuilder

    def result: String = sb.toString

    def top(program: TProgram): Unit = renderNode(program, "")

    private def tag(v: Any): String = v match
      case RecvMode.ByValue     => "ByValue"
      case RecvMode.ByPtr       => "ByPtr"
      case _: RecvMode.ByRef    => "ByRef"
      case AsmDir.In            => "In"
      case AsmDir.Out           => "Out"
      case HookKind.Setup       => "Setup"
      case HookKind.Teardown    => "Teardown"
      case HookKind.SetupAll    => "SetupAll"
      case HookKind.TeardownAll => "TeardownAll"
      case _ =>
        val name = v.getClass.getSimpleName

        if name.endsWith("$") then name.dropRight(1) else name

    private def span(p: Positioned): String =
      if !spans then ""
      else p.pos.map(s => s" ${s.line}:${s.col}-${s.endLine}:${s.endCol}").getOrElse("")

    /** Every node that is not one of the four sealed hierarchies below, and every node's own
     * fields once its header line is written — the identical two-step `AstPrinter.renderNode`
     * takes, over a different tree.
     */
    private def renderNode(node: Product, indent: String): Unit = {
      sb.append(tag(node))

      node match
        case p: Positioned => sb.append(span(p))
        case _              =>

      sb.append('\n')

      val childIndent = indent + "  "

      node.productElementNames.zip(node.productIterator).foreach { case (name, value) =>
        sb.append(childIndent).append(name).append(": ")
        renderValue(value, childIndent)
      }
    }

    // Every `TExpr` by name, with no wildcard (`tast.scala`). The body is `renderNode` for all of
    // them — nothing in the typed tree needs `AstPrinter`'s `CharLit` decode, because a character
    // literal has already become an ordinary integer of type `char` by the time it is typed.
    private def exprNode(e: TExpr, indent: String): Unit = e match
      case n: TIntLit           => renderNode(n, indent)
      case n: TFloatLit         => renderNode(n, indent)
      case n: TStrLit           => renderNode(n, indent)
      case n: TBoolLit          => renderNode(n, indent)
      case n: TUnitLit          => renderNode(n, indent)
      case n: TNullLit          => renderNode(n, indent)
      case n: TBox              => renderNode(n, indent)
      case n: TDowngrade        => renderNode(n, indent)
      case n: TUpgrade          => renderNode(n, indent)
      case n: TZero             => renderNode(n, indent)
      case n: TArrayLit         => renderNode(n, indent)
      case n: TArrayFill        => renderNode(n, indent)
      case n: TVectorLit        => renderNode(n, indent)
      case n: TSplat            => renderNode(n, indent)
      case n: TLane             => renderNode(n, indent)
      case n: TVecCompare       => renderNode(n, indent)
      case n: TSelect           => renderNode(n, indent)
      case n: TReduce           => renderNode(n, indent)
      case n: TVecLoad          => renderNode(n, indent)
      case n: TVecStore         => renderNode(n, indent)
      case n: TBufLit           => renderNode(n, indent)
      case n: TBufFill          => renderNode(n, indent)
      case n: TIndex            => renderNode(n, indent)
      case n: TLen              => renderNode(n, indent)
      case n: TBytes            => renderNode(n, indent)
      case n: TSlice            => renderNode(n, indent)
      case n: TCast             => renderNode(n, indent)
      case n: TConstrainedCheck => renderNode(n, indent)
      case n: TConstrainedValid => renderNode(n, indent)
      case n: TConstrainedStep  => renderNode(n, indent)
      case n: TLoad             => renderNode(n, indent)
      case n: TResult           => renderNode(n, indent)
      case n: TOld              => renderNode(n, indent)
      case n: TGlobal           => renderNode(n, indent)
      case n: TDeref            => renderNode(n, indent)
      case n: TTypeId           => renderNode(n, indent)
      case n: TAddrOf           => renderNode(n, indent)
      case n: TTempAddr         => renderNode(n, indent)
      case n: TStore            => renderNode(n, indent)
      case n: TUpdate           => renderNode(n, indent)
      case n: TIncDec           => renderNode(n, indent)
      case n: TBinary           => renderNode(n, indent)
      case n: TUnary            => renderNode(n, indent)
      case n: TIntOp            => renderNode(n, indent)
      case n: TAtomic           => renderNode(n, indent)
      case n: TFence            => renderNode(n, indent)
      case n: TLogical          => renderNode(n, indent)
      case n: TCompare          => renderNode(n, indent)
      case n: TSeq              => renderNode(n, indent)
      case n: TStr              => renderNode(n, indent)
      case n: TFromBytes        => renderNode(n, indent)
      case n: TConstView        => renderNode(n, indent)
      case n: TFormat           => renderNode(n, indent)
      case n: TRender           => renderNode(n, indent)
      case n: TCStrLit          => renderNode(n, indent)
      case n: TCall             => renderNode(n, indent)
      case n: TFuncAddr         => renderNode(n, indent)
      case n: TCallPtr          => renderNode(n, indent)
      case n: TErase            => renderNode(n, indent)
      case n: TVCall            => renderNode(n, indent)
      case n: TVaStart          => renderNode(n, indent)
      case n: TVaEnd            => renderNode(n, indent)
      case n: TVaArg            => renderNode(n, indent)
      case n: TVaCopy           => renderNode(n, indent)
      case n: TVaPass           => renderNode(n, indent)
      case n: TStructNew        => renderNode(n, indent)
      case n: TStructInvCheck   => renderNode(n, indent)
      case n: TRecheck          => renderNode(n, indent)
      case n: TEnumNew          => renderNode(n, indent)
      case n: TEnumFromInt      => renderNode(n, indent)
      case n: TEnumTry          => renderNode(n, indent)
      case n: TEnumAttr         => renderNode(n, indent)
      case n: TTry              => renderNode(n, indent)
      case n: TField            => renderNode(n, indent)
      case n: TIf               => renderNode(n, indent)
      case n: TMatch            => renderNode(n, indent)
      case n: TBlockExpr        => renderNode(n, indent)
      case n: TWhile            => renderNode(n, indent)
      case n: TDoWhile          => renderNode(n, indent)
      case n: TLoop             => renderNode(n, indent)
      case n: TFor              => renderNode(n, indent)
      case n: TCFor             => renderNode(n, indent)
      case n: TQuantifier       => renderNode(n, indent)
      case n: TCheckedLoop      => renderNode(n, indent)
      case n: TForEach          => renderNode(n, indent)
      case n: TIterate          => renderNode(n, indent)

    // Every `TStmt` by name (`tastStmts.scala`).
    private def stmtNode(s: TStmt, indent: String): Unit = s match
      case n: TVarDecl      => renderNode(n, indent)
      case n: TExprStmt     => renderNode(n, indent)
      case n: TInvariant    => renderNode(n, indent)
      case n: TVariantCheck => renderNode(n, indent)
      case n: TRefDecl      => renderNode(n, indent)
      case n: TMultiAssign  => renderNode(n, indent)
      case n: TReturn       => renderNode(n, indent)
      case n: TBecome       => renderNode(n, indent)
      case n: TBreak        => renderNode(n, indent)
      case n: TContinue     => renderNode(n, indent)
      case n: TDefer        => renderNode(n, indent)
      case n: TAsm          => renderNode(n, indent)

    // Every `TPattern` by name (`tastPatterns.scala`).
    private def patternNode(p: TPattern, indent: String): Unit = p match
      case n: TWildPattern    => renderNode(n, indent)
      case n: TBindPattern    => renderNode(n, indent)
      case n: TAtPattern      => renderNode(n, indent)
      case n: TLitPattern     => renderNode(n, indent)
      case n: TRangePattern   => renderNode(n, indent)
      case n: TVariantPattern => renderNode(n, indent)
      case n: TStructPattern  => renderNode(n, indent)

    // Every `TCondTerm` by name — the two ways an `if`'s or a `while`'s condition chain answers
    // for one term (`tast.scala`).
    private def condTermNode(c: TCondTerm, indent: String): Unit = c match
      case n: TCondTest => renderNode(n, indent)
      case n: TCondIs   => renderNode(n, indent)

    /** One field's value, whatever shape it is — `AstPrinter.renderValue`'s twin, with `Type`
     * intercepted before the generic `Product` fallback ever sees one (`TypedAstPrinter`'s own
     * header explains why) and a `Map` read by its keys' own runtime shape rather than assumed to
     * be `String`-keyed, since `TVSlot.borrows` is keyed by parameter position (`Map[Int, String]`)
     * and `TProgram.moduleDeps` by module name (`Map[String, Set[String]]`).
     */
    private def renderValue(v: Any, indent: String): Unit = v match
      case null    => sb.append("null\n")
      case None    => sb.append("None\n")
      case Some(x) => renderValue(x, indent)
      case t: Type => sb.append(Type.show(t)).append('\n')
      case s: String  => sb.append(quote(s)).append('\n')
      case b: Boolean => sb.append(b).append('\n')
      case n: BigInt  => sb.append(n).append('\n')
      case l: Long    => sb.append(l).append('\n')
      case i: Int     => sb.append(i).append('\n')

      // Sorted by key so that two analyzers which built one map in different orders — nothing in
      // the tree records an order among `borrows`, `moduleDeps` or `destructors`' keys — still
      // agree on the text. An `Int` key sorts numerically and anything else by its own `toString`,
      // which is the one comparison every key this tree uses can make.
      case m: Map[?, ?] @unchecked =>
        if m.isEmpty then sb.append("{}\n")
        else {
          sb.append('\n')

          val childIndent = indent + "  "
          val entries = m.toList.sortWith { case ((k1, _), (k2, _)) =>
            (k1, k2) match
              case (a: Int, b: Int) => a < b
              case _                => k1.toString < k2.toString
          }

          entries.foreach { case (k, x) =>
            val keyText = k match
              case s: String => quote(s)
              case other     => other.toString

            sb.append(childIndent).append(keyText).append(": ")
            renderValue(x, childIndent)
          }
        }

      case xs: Set[String] @unchecked =>
        sb.append('[').append(xs.toList.sorted.map(quote).mkString(", ")).append("]\n")

      case xs: List[?] =>
        if xs.isEmpty then sb.append("[]\n")
        else if xs.forall(isScalar) then sb.append('[').append(xs.map(scalarText).mkString(", ")).append("]\n")
        else {
          sb.append('\n')

          val childIndent = indent + "  "

          xs.foreach { x =>
            sb.append(childIndent).append("- ")
            renderValue(x, childIndent + "  ")
          }
        }

      case e: TExpr     => exprNode(e, indent)
      case s: TStmt     => stmtNode(s, indent)
      case p: TPattern  => patternNode(p, indent)
      case c: TCondTerm => condTermNode(c, indent)

      // Everything else that carries its own fields and no sealed hierarchy of its own: `TArm`,
      // `TBlock`, `TFunc`, `TVtable`, `TVSlot`, `TVal`, `TEntry`, `THook`, `THooks`, `TAsmOperand`,
      // `TWrite`, `TDispatch`, `TCmp`, `CallConv`, a plain tuple such as `TWrite.check`'s — read
      // generically, exactly as `AstPrinter`'s own fallback reads `Param`/`MethodDecl`/`BoundRef`.
      case node: Product => renderNode(node, indent)

      case other => sb.append(other.toString).append('\n')
  }

  /** `emit-typed --tables`'s renderer, held apart from `Printer` because it walks `TProgram`'s
   * declaration lists directly rather than following the tree — a struct's fields and an enum's
   * variants are read straight off the `Type.Struct`/`Type.Enum` API, not off `productIterator`,
   * since neither is a case class (`Type.scala`).
   */
  private object Tables {
    def render(program: TProgram): String = {
      val sb = new StringBuilder

      sb.append("structs:\n")
      program.structs.sortBy(_.name).foreach { s =>
        sb.append("  ").append(s.name).append('\n')
        s.fields.foreach { case (name, ty) =>
          sb.append("    ").append(name).append(": ").append(Type.show(ty)).append('\n')
        }
      }

      sb.append("enums:\n")
      program.enums.sortBy(_.name).foreach { e =>
        sb.append("  ").append(e.name).append('\n')
        e.variants.sortBy(_.tag).foreach { v =>
          sb.append("    ").append(v.name).append(" = ").append(v.tag)
          if v.carries then
            sb.append(" (")
              .append(v.fields.map((n, t) => s"$n: ${Type.show(t)}").mkString(", "))
              .append(')')
          sb.append('\n')
        }
      }

      // Grouped by the implementing type and the trait, since that pair — not the table's own
      // memory-mode split — is what a declaration is: `*Shape` and `&Shape` are two `TVtable`s for
      // one `impl Shape for Circle`, and a second analyzer's table has only one row for it.
      sb.append("traits:\n")
      program.vtables.map(_.traitName).distinct.sorted.foreach(t => sb.append("  ").append(t).append('\n'))

      sb.append("impls:\n")
      program.vtables
        .map(v => (v.traitName, Type.show(v.forType)))
        .distinct
        .sortBy((t, f) => (t, f))
        .foreach { (t, f) =>
          sb.append("  ").append(t).append(" for ").append(f).append('\n')

          program.vtables
            .filter(v => v.traitName == t && Type.show(v.forType) == f)
            .flatMap(_.slots)
            .distinctBy(_.target)
            .sortBy(_.target)
            .foreach { slot =>
              val params = slot.params.map(Type.show).mkString(", ")
              sb.append("    ").append(slot.target).append('(').append(params).append(") -> ")
                .append(Type.show(slot.retTy)).append('\n')
            }
        }

      sb.append("externs:\n")
      program.externs.sortBy(_.name).foreach { ext =>
        val params = ext.params.map(Type.show).mkString(", ")
        sb.append("  ").append(ext.name).append('(').append(params).append(") -> ")
          .append(Type.show(ext.retTy)).append('\n')
      }

      sb.append("consts:\n")
      program.vals.sortBy(_.symbol).foreach { v =>
        sb.append("  ").append(v.symbol).append(": ").append(Type.show(v.ty)).append('\n')
      }

      sb.append("funcs:\n")
      program.funcs.sortBy(_.name).foreach { f =>
        val params = f.params.map((n, t) => s"$n: ${Type.show(t)}").mkString(", ")
        sb.append("  ").append(f.name).append('(').append(params).append(") -> ")
          .append(Type.show(f.retTy)).append('\n')
      }

      sb.toString
    }
  }
}
