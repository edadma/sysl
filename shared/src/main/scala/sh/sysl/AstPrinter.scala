package sh.sysl

/** The untyped parse tree, written out as deterministic, human-readable text — `sysl emit-ast`'s
 * whole job.
 *
 * **Why this exists.** A second compiler is being written for sysl, in sysl
 * (`~/dev/sysl-lang/sysl`), and the cheapest way to know the two parsers agree is to diff what each
 * one built for the same file. `AstCodec` already writes the tree out, but in a token format built
 * for a fast *read back into this compiler* — tags, string-table indices, no whitespace a human or a
 * `diff` would want. This is the same walk, aimed at a different reader.
 *
 * **The format.** Every node is one line — its type name, then its span where spans are wanted — and
 * every field of that node is a line under it, indented two spaces deeper, `fieldName: value`:
 *
 * {{{
 * Program
 *   source: hello.sysl
 *   module: None
 *   body:
 *     - ExprStmt 3:1-3:14
 *         expr:
 *           Call 3:1-3:14
 *             callee:
 *               Ident 3:1-3:6
 *                 name: "print"
 *             args: [...]
 * }}}
 *
 * A span is `line:col-line:col`, 1-based, from `Positioned.pos` — the same position a diagnostic
 * points at, not necessarily the node's first token (`Diagnostics.scala`'s `Pos`/`Positioned`
 * documents the difference). A node with no position — every `Pattern`, which carries none — prints
 * without one.
 *
 * A field's *value* is one of:
 *   - a decoded scalar: a quoted, escaped string, a number, `true`/`false`, `None` for an absent
 *     option (an option's `Some` is transparent — the field reads as the value it holds).
 *   - `[]` for an empty list, or `[a, b, c]` inline where every element is itself a scalar (this is
 *     what a `List[String]` — `tparams`, `crossing`, a pattern's `archs` — prints as).
 *   - a block of `- ` items, one per element, where a list holds nodes rather than scalars.
 *   - `{}` for an empty map, or a block of `"key": value` entries otherwise. A `Map[String, _]`
 *     (`bounds`, `tdefaults`, `tvalues`) is sorted by key, and a `Set[String]` (`tpacks`) is sorted
 *     and printed as an inline list — both because the tree carries no order among the keys it maps
 *     from a type parameter's name, and iteration order over `scala.collection.immutable`'s own
 *     hashing is not something a second implementation, in a different language, would ever have
 *     reason to reproduce. Sorting is the one thing both sides can agree on independently.
 *
 * **Completeness is structural, not enumerated.** Rather than hand-listing every field of every one
 * of the roughly 130 node types across `ast.scala`, `astFile.scala`, `astStmts.scala`,
 * `astTypes.scala` and `astPatterns.scala` — which is exactly how a field silently goes missing —
 * each node's fields are read off it with `productElementNames`/`productIterator`, the same
 * reflection every case class offers for free. What stays hand-written, mirroring `AstCodec`'s own
 * `expr`/`stmt`/`typ`/`pattern` functions, is one **exhaustive match per sealed hierarchy** — `Expr`,
 * `Stmt`, `TypeRef`, `Pattern`, `AsmBody` — naming every case by hand with no wildcard arm. Every
 * arm's body is the same one call, so the match does no work of its own; its only job is that a node
 * kind added to one of the four `ast*.scala` files without a matching case here is a compiler warning
 * on this file, the same guarantee `AstCodec.Version`'s comment describes for the binary codec.
 * Everything that is not one of those four — `Param`, `MethodDecl`, `BoundRef`, `TestAttr`, a header
 * clause, a doc comment — has no sealed hierarchy to be exhaustive over and is read generically.
 *
 * **`CharLit` is the one literal a `productIterator` walk would get wrong**: its `codepoint` is an
 * `Int`, indistinguishable by type from any other `Int` field the tree might one day carry, so it is
 * decoded by hand into the character it names (`decoded_codepoint '<char>'`) rather than printed as a
 * bare number.
 */
object AstPrinter {

  /** One file's parse, printed. `spans` is false for `sysl emit-ast --no-spans`: a tree with no
   * positions in it diffs cleanly across an edit that only moves lines around, which is what a
   * parser under construction spends most of its time doing.
   */
  def print(program: Program, spans: Boolean = true): String = {
    val printer = new Printer(spans)

    printer.top(program)
    printer.result
  }

  /** A double-quoted string with control characters, quotes and backslashes escaped, so that no
   * field's value can ever introduce a raw newline into what is otherwise one node per line.
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

  private def isScalar(v: Any): Boolean = v match
    case _: String | _: Boolean | _: Int | _: BigInt => true
    case _                                            => false

  private def scalarText(v: Any): String = v match
    case s: String => quote(s)
    case other      => other.toString

  /** The whole of the state one `print` call carries: the buffer being written to, and whether spans
   * are wanted. Held in a class for the reason `AstCodec`'s `Encoder` is: every method below is a
   * step of one walk, and threading the buffer through each as a parameter would say nothing an
   * instance field does not already say.
   */
  private final class Printer(spans: Boolean) {
    private val sb = new StringBuilder

    def result: String = sb.toString

    def top(program: Program): Unit = renderNode(program, "")

    /** A node's own type name.
     *
     * **The tree's five `enum`s are matched by hand, and this is not the generic case rescued by a
     * fallback — it is the only reliable route.** A parameterless case — `Visibility.Public`,
     * `AsmDir.In`, every `HookKind` — compiles to an anonymous class, and the three backends disagree
     * about what `getClass.getSimpleName` answers for it: `""` on the JVM (as the JDK specifies for
     * an anonymous class), `"anon$1"` on Scala.js, and a bare ordinal — `"1"` for `Visibility.Public`
     * — on Scala Native, whose `Class.getSimpleName` splits the binary name (`Visibility$$anon$1`)
     * on `$` and takes the last piece, landing on the anonymous class's own numbering rather than
     * anything naming the case (`scala-native/scala-native#5030`). `toString`, string interpolation,
     * `productPrefix` and `ordinal` all agree with the case's name on every backend — it is only
     * `getSimpleName` on the anonymous class that diverges. Caught by the golden test above, which is
     * exact enough to fail on every `vis: Public` field turning into `vis: 1` under `syslNative/test`
     * and nowhere else — the tell that this was a backend difference and not a logic error, since the
     * same code ran unchanged on all three.
     *
     * Every other node is a real case class or case object, whose `getSimpleName` names it plainly
     * and identically everywhere — `IntLit`, `WildcardPattern` (stripped of the trailing `$` a case
     * object's own carries) — which is what keeps this reflective for the roughly 125 kinds that
     * need nothing more.
     */
    private def tag(v: Any): String = v match
      case Visibility.Public               => "Public"
      case Visibility.File                 => "File"
      case _: Visibility.Scoped            => "Scoped"
      case RecvMode.ByValue                => "ByValue"
      case RecvMode.ByPtr                  => "ByPtr"
      case _: RecvMode.ByRef               => "ByRef"
      case AsmDir.In                       => "In"
      case AsmDir.Out                      => "Out"
      case HookKind.Setup                  => "Setup"
      case HookKind.Teardown               => "Teardown"
      case HookKind.SetupAll               => "SetupAll"
      case HookKind.TeardownAll            => "TeardownAll"
      case CapabilityDirection.Narrows     => "Narrows"
      case CapabilityDirection.Requires    => "Requires"
      case _ =>
        val name = v.getClass.getSimpleName

        if name.endsWith("$") then name.dropRight(1) else name

    private def span(p: Positioned): String =
      if !spans then ""
      else p.pos.map(s => s" ${s.line}:${s.col}-${s.endLine}:${s.endCol}").getOrElse("")

    /** Every node that is not one of the four sealed hierarchies below: a header line — the type
     * name, and a span where the node carries one — then every field of it, read off the case class
     * itself rather than named here one by one.
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

    /** `CharLit`'s `codepoint` is the one raw `Int` in the tree that means something other than a
     * count or an index, so it is the one field printed by hand rather than through `renderNode`.
     */
    private def renderCharLit(n: CharLit, indent: String): Unit = {
      sb.append(tag(n)).append(span(n)).append('\n')

      val cp   = n.codepoint
      val text =
        if Character.isValidCodePoint(cp) && !Character.isISOControl(cp) then
          s"$cp '${new String(Character.toChars(cp))}'"
        else cp.toString

      sb.append(indent).append("  codepoint: ").append(text).append('\n')
    }

    // Every `Expr` by name, with no wildcard: a case added to `ast.scala` without a matching line
    // here is a non-exhaustive-match warning on this file, which is the whole point (see the header
    // comment). The body is `renderNode` for every one of them but `CharLit`, which decodes instead.
    private def exprNode(e: Expr, indent: String): Unit = e match
      case n: CharLit       => renderCharLit(n, indent)
      case n: IntLit         => renderNode(n, indent)
      case n: FloatLit       => renderNode(n, indent)
      case n: StrLit         => renderNode(n, indent)
      case n: CStrLit        => renderNode(n, indent)
      case n: BoolLit        => renderNode(n, indent)
      case n: UnitLit        => renderNode(n, indent)
      case n: NullLit        => renderNode(n, indent)
      case n: Ident          => renderNode(n, indent)
      case n: Unary          => renderNode(n, indent)
      case n: PreIncDec      => renderNode(n, indent)
      case n: PostIncDec     => renderNode(n, indent)
      case n: Binary         => renderNode(n, indent)
      case n: Compare        => renderNode(n, indent)
      case n: RangeExpr      => renderNode(n, indent)
      case n: Assign         => renderNode(n, indent)
      case n: Call           => renderNode(n, indent)
      case n: NamedArg       => renderNode(n, indent)
      case n: Spread         => renderNode(n, indent)
      case n: DefaultArg     => renderNode(n, indent)
      case n: Index          => renderNode(n, indent)
      case n: Field          => renderNode(n, indent)
      case n: TypeArgs       => renderNode(n, indent)
      case n: TypeAttr       => renderNode(n, indent)
      case n: WithExpr       => renderNode(n, indent)
      case n: ImplicitMember => renderNode(n, indent)
      case n: LayoutOf       => renderNode(n, indent)
      case n: OffsetOf       => renderNode(n, indent)
      case n: TryExpr        => renderNode(n, indent)
      case n: Tuple          => renderNode(n, indent)
      case n: Lambda         => renderNode(n, indent)
      case n: BlockArg       => renderNode(n, indent)
      case n: ArrayLit       => renderNode(n, indent)
      case n: ArrayFill      => renderNode(n, indent)
      case n: Block          => renderNode(n, indent)
      case n: IfExpr         => renderNode(n, indent)
      case n: MatchExpr      => renderNode(n, indent)
      case n: IsPattern      => renderNode(n, indent)
      case n: ResultList     => renderNode(n, indent)
      case n: While          => renderNode(n, indent)
      case n: DoWhile        => renderNode(n, indent)
      case n: Loop           => renderNode(n, indent)
      case n: For            => renderNode(n, indent)
      case n: ConstFor       => renderNode(n, indent)
      case n: CFor           => renderNode(n, indent)
      case n: Quantifier     => renderNode(n, indent)

    // Every `Stmt` by name. `StaticDecl`, `CConstBlock` and `CTypeBlock` reach here — unlike in
    // `AstCodec`, which only ever sees a tree already lowered past them (`CProbe`) — because
    // `emit-ast` prints the tree the parser built, before analysis lowers anything.
    private def stmtNode(s: Stmt, indent: String): Unit = s match
      case n: ImportDecl     => renderNode(n, indent)
      case n: VarDecl         => renderNode(n, indent)
      case n: ConstDecl       => renderNode(n, indent)
      case n: ValDecl         => renderNode(n, indent)
      case n: StaticDecl      => renderNode(n, indent)
      case n: CConstBlock     => renderNode(n, indent)
      case n: CTypeBlock      => renderNode(n, indent)
      case n: RefDecl         => renderNode(n, indent)
      case n: MultiAssign     => renderNode(n, indent)
      case n: MultiDecl       => renderNode(n, indent)
      case n: PatternDecl     => renderNode(n, indent)
      case n: ExprStmt        => renderNode(n, indent)
      case n: Return          => renderNode(n, indent)
      case n: Become          => renderNode(n, indent)
      case n: Break           => renderNode(n, indent)
      case n: Continue        => renderNode(n, indent)
      case n: Defer           => renderNode(n, indent)
      case n: AsmStmt         => renderNode(n, indent)
      case n: AssertDecl      => renderNode(n, indent)
      case n: Require         => renderNode(n, indent)
      case n: Ensure          => renderNode(n, indent)
      case n: Invariant       => renderNode(n, indent)
      case n: Variant         => renderNode(n, indent)
      case n: FuncDecl        => renderNode(n, indent)
      case n: ExternDecl      => renderNode(n, indent)
      case n: ExternVarDecl   => renderNode(n, indent)
      case n: StructDecl      => renderNode(n, indent)
      case n: EnumDecl        => renderNode(n, indent)
      case n: TypeDecl        => renderNode(n, indent)
      case n: TraitDecl       => renderNode(n, indent)
      case n: ImplDecl        => renderNode(n, indent)

    // Every `TypeRef` by name.
    private def typeNode(t: TypeRef, indent: String): Unit = t match
      case n: NamedType    => renderNode(n, indent)
      case n: ValueArgType => renderNode(n, indent)
      case n: PtrType      => renderNode(n, indent)
      case n: RefType      => renderNode(n, indent)
      case n: WeakType     => renderNode(n, indent)
      case n: ArrayType    => renderNode(n, indent)
      case n: VectorType   => renderNode(n, indent)
      case n: VolatileType => renderNode(n, indent)
      case n: TupleType    => renderNode(n, indent)
      case n: PackType     => renderNode(n, indent)
      case n: FnType       => renderNode(n, indent)
      case n: CFnType      => renderNode(n, indent)
      case n: AssocType    => renderNode(n, indent)
      case n: AssocArgType => renderNode(n, indent)
      case n: SomeType     => renderNode(n, indent)

    // Every `Pattern` by name. None of them is `Positioned` (`astPatterns.scala`), so none prints a
    // span — a pattern is complained about at the `MatchArm` or `IsPattern` that holds it.
    private def patternNode(p: Pattern, indent: String): Unit = p match
      case n: LitPattern     => renderNode(n, indent)
      case n: RangePattern   => renderNode(n, indent)
      case WildcardPattern    => renderNode(WildcardPattern, indent)
      case n: IdentPattern   => renderNode(n, indent)
      case n: EqPattern      => renderNode(n, indent)
      case n: VariantPattern => renderNode(n, indent)
      case n: StructPattern  => renderNode(n, indent)
      case n: TuplePattern   => renderNode(n, indent)
      case n: BindPattern    => renderNode(n, indent)

    // Every `AsmBody` by name — the two ways an `AsmArm` may answer for its architectures.
    private def asmBodyNode(a: AsmBody, indent: String): Unit = a match
      case n: AsmCode        => renderNode(n, indent)
      case n: AsmUnavailable => renderNode(n, indent)

    /** One field's value, whatever shape it is. This is the one place an `Option` is unwrapped, a
     * collection is laid out, and a nested node is dispatched back to one of the five functions
     * above — or, for everything outside those five hierarchies (`Param`, `MethodDecl`, `BoundRef`,
     * a header clause, a doc comment, an enum case such as `Visibility` or `RecvMode`), to
     * `renderNode` directly, generically.
     */
    private def renderValue(v: Any, indent: String): Unit = v match
      case null    => sb.append("null\n")
      case None    => sb.append("None\n")
      case Some(x) => renderValue(x, indent)
      case s: String  => sb.append(quote(s)).append('\n')
      case b: Boolean => sb.append(b).append('\n')
      case n: BigInt  => sb.append(n).append('\n')
      case i: Int     => sb.append(i).append('\n')

      // `StructPattern.fields: List[(String, Pattern)]` is the only tuple in the tree, so this is
      // the one place a pair is read as a pair rather than as a two-field product.
      case (k: String, x) =>
        sb.append(quote(k)).append(" -> ")
        renderValue(x, indent)

      // `bounds`/`tdefaults`/`tvalues`, sorted by key: nothing in the tree orders the type
      // parameters a map is keyed by, and a key's own alphabetical order is the one ordering a
      // second implementation would arrive at without having read this compiler's insertion order.
      case m: Map[String, ?] @unchecked =>
        if m.isEmpty then sb.append("{}\n")
        else {
          sb.append('\n')

          val childIndent = indent + "  "

          m.toList.sortBy(_._1).foreach { case (k, x) =>
            sb.append(childIndent).append(quote(k)).append(": ")
            renderValue(x, childIndent)
          }
        }

      // `tpacks`, sorted for the same reason a map's keys are, and printed inline since every
      // element is a name.
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

      case e: Expr    => exprNode(e, indent)
      case s: Stmt    => stmtNode(s, indent)
      case t: TypeRef => typeNode(t, indent)
      case p: Pattern => patternNode(p, indent)
      case a: AsmBody => asmBodyNode(a, indent)

      // Everything else that carries its own fields: `Param`, `MethodDecl`, `BoundRef`, a header
      // clause, a doc comment, an enum case (`Visibility`, `RecvMode`, `HookKind`, `AsmDir`,
      // `CapabilityDirection`) — read generically, exactly as `renderNode`'s own caller reads it.
      case node: Product => renderNode(node, indent)

      // `Source` (by `toString`, which is its name — `Diagnostics.scala`) and a last-resort fallback
      // for anything this walk has no other case for.
      case other => sb.append(other.toString).append('\n')
  }
}
