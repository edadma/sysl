package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `sysl emit-typed`'s printer: the typed tree written out as deterministic text, and the
 * declaration tables `--tables` prints instead (`TypedAstPrinter`'s own header has the format).
 *
 * The golden fixture is a real parse-and-analyze — `Compiler.typedWith` over two functions and a
 * struct — filtered down to just what those declarations resolved to, so the assertion is exact
 * without pinning the whole standard module's declaration set (`typedWith` never prunes, so an
 * unfiltered `TProgram` also carries every struct, enum and `val` the standard module declares).
 * Everything the fixture's own syntax does not reach — every expression and statement kind, every
 * pattern, every condition term — is covered separately by `"every typed node kind is reachable"`,
 * which builds a `TProgram` directly rather than through analysis, so it needs no real program to
 * reach a `TAtomic` or a `TQuantifier`.
 */
class TypedAstPrinterTests extends AnyFreeSpec with Matchers {

  private def typed(src: String, name: String = "fixture.sysl"): TProgram =
    Compiler.typedWith(List(Source(name, src)), Nil) match
      case Right((program, _)) => program
      case Left(err)           => fail(s"the fixture does not analyze: $err")

  private val fixtureSrc =
    """struct Point
      |    x: int
      |    y: int
      |
      |add(a: int, b: int) -> int = a + b
      |
      |f(p: Point) -> int =
      |    if p.x > 0 then add(p.x, p.y) else 0
      |""".stripMargin

  /** The fixture's own declarations, with the standard module's carried alongside them by every
   * ordinary compilation stripped back out. `TProgram` is a case class, so this is a fresh value
   * built from the pieces that matter rather than a filter over one that also carries them.
   */
  private def fixtureProgram: TProgram = {
    val program = typed(fixtureSrc)
    val point   = program.structs.find(_.base == "Point").getOrElse(fail("Point did not resolve"))
    val add     = program.funcs.find(_.name == "add").getOrElse(fail("add did not resolve"))
    val f       = program.funcs.find(_.name == "f").getOrElse(fail("f did not resolve"))

    TProgram(List(point), Nil, Nil, Nil, Nil, List(add, f), Nil)
  }

  "the format" - {

    "prints the golden text for a small program, exactly, with spans off" in {
      TypedAstPrinter.print(fixtureProgram, spans = false) shouldBe TypedAstPrinterTests.goldenBare
    }

    "is deterministic: the same tree prints the same text twice" in {
      val program = fixtureProgram

      TypedAstPrinter.print(program) shouldBe TypedAstPrinter.print(program)
    }

    "with spans on, an expression's line carries one, as 'line:col-line:col'; with spans off, none does" in {
      val program = fixtureProgram

      TypedAstPrinter.print(program, spans = true) should include regex """TIf \d+:\d+-\d+:\d+"""
      TypedAstPrinter.print(program, spans = false) should not include regex("""\d+:\d+-\d+:\d+""")
    }

    "--no-spans drops spans and nothing else: the two texts agree once spans are stripped" in {
      val program  = fixtureProgram
      val withSpan = TypedAstPrinter.print(program, spans = true)
      val without  = TypedAstPrinter.print(program, spans = false)

      withSpan.replaceAll(""" \d+:\d+-\d+:\d+""", "") shouldBe without
    }

    "an expression's type is the compiler's own diagnostic text, not the type's structure" in {
      val printed = TypedAstPrinter.print(fixtureProgram, spans = false)

      // `Type.Int`'s diagnostic spelling is `int`; its structural fields (`bits`, `signed`) never
      // appear, which is the one thing this suite could not tell apart from a bug that printed the
      // right answer for the wrong reason.
      printed should include("ty: int")
      printed should not include "bits: 32"
      printed should not include "signed: true"
    }
  }

  "--tables" - {

    "prints the golden declaration tables for the same small program, exactly" in {
      TypedAstPrinter.tables(fixtureProgram) shouldBe TypedAstPrinterTests.goldenTables
    }

    "is deterministic" in {
      val program = fixtureProgram

      TypedAstPrinter.tables(program) shouldBe TypedAstPrinter.tables(program)
    }
  }

  "every typed node kind is reachable" - {

    // One instance of every case class in the three sealed hierarchies `tast.scala` (`TExpr`, 86),
    // `tastStmts.scala` (`TStmt`, 12) and `tastPatterns.scala` (`TPattern`, 7) declare, plus
    // `TCondTerm`'s two (`tast.scala`) — 107 in all, counted the same way `AstPrinterTests`' own
    // "every node kind" test counts the untyped tree's: `grep -c "^case class"` over each file's
    // sealed hierarchy, which is `AstPrinter`'s and `TypedAstPrinter`'s own exhaustive matches. Built
    // directly, node by node, rather than through analysis — a real `TAtomic` or a real `TIterate`
    // would need real hardware intrinsics and a real `Iterate` implementation on the receiver, which
    // is real syntax risk for no benefit: this test is about the printer's coverage of the tree, not
    // about what the analyzer accepts.
    val expectedTags = Set(
      // TExpr
      "TIntLit", "TFloatLit", "TStrLit", "TBoolLit", "TUnitLit", "TNullLit", "TBox", "TDowngrade",
      "TUpgrade", "TZero", "TArrayLit", "TArrayFill", "TVectorLit", "TSplat", "TLane", "TVecCompare",
      "TSelect", "TReduce", "TVecLoad", "TVecStore", "TBufLit", "TBufFill", "TIndex", "TLen",
      "TBytes", "TSlice", "TCast", "TConstrainedCheck", "TConstrainedValid", "TConstrainedStep",
      "TLoad", "TResult", "TOld", "TGlobal", "TDeref", "TTypeId", "TAddrOf", "TTempAddr", "TStore",
      "TUpdate", "TIncDec", "TBinary", "TUnary", "TIntOp", "TAtomic", "TFence", "TLogical",
      "TCompare", "TSeq", "TStr", "TFromBytes", "TConstView", "TFormat", "TRender", "TCStrLit",
      "TCall", "TFuncAddr", "TCallPtr", "TErase", "TVCall", "TVaStart", "TVaEnd", "TVaArg", "TVaCopy",
      "TVaPass", "TStructNew", "TStructInvCheck", "TRecheck", "TEnumNew", "TEnumFromInt", "TEnumTry",
      "TEnumAttr", "TTry", "TField", "TIf", "TMatch", "TBlockExpr", "TWhile", "TDoWhile", "TLoop",
      "TFor", "TCFor", "TQuantifier", "TCheckedLoop", "TForEach", "TIterate",
      // TStmt
      "TVarDecl", "TExprStmt", "TInvariant", "TVariantCheck", "TRefDecl", "TMultiAssign", "TReturn",
      "TBecome", "TBreak", "TContinue", "TDefer", "TAsm",
      // TPattern
      "TWildPattern", "TBindPattern", "TAtPattern", "TLitPattern", "TRangePattern", "TVariantPattern",
      "TStructPattern",
      // TCondTerm
      "TCondTest", "TCondIs",
    )

    "the reference count is 107, the same total the typed-tree files themselves grep to" in {
      expectedTags should have size 107
    }

    val i1     = TIntLit(BigInt(1), Type.Int)
    val i2     = TIntLit(BigInt(2), Type.Int)
    val bTrue  = TBoolLit(true)
    val ld     = TLoad("x", Type.Int)
    val vecLd  = TLoad("v", Type.Vector(4, Type.Int))
    val age    = Type.Constrained("Age", Type.Int, derived = false, None, None, exclusiveHi = false, None)
    val blk    = TBlock(Nil, Some(i1), Type.Int)

    val point = new Type.Struct("Point", Nil)
    point.fields = List("x" -> Type.Int, "y" -> Type.Int)

    val colorEnum = new Type.Enum("Color", Nil)
    colorEnum.simple = false
    val redVariant = Type.EnumVariant("Red", 0, Nil, carries = false)
    colorEnum.variants = List(redVariant)

    val optEnum = new Type.Enum("Option", List(Type.Ref(Type.Int, sync = false)))
    optEnum.simple = false
    val someVariant = Type.EnumVariant("Some", 0, List("0" -> Type.Ref(Type.Int, sync = false)), carries = true)
    val noneVariant = Type.EnumVariant("None", 1, Nil, carries = false)
    optEnum.variants = List(someVariant, noneVariant)

    val resultEnum = new Type.Enum("Result", List(Type.Int, Type.Str))
    resultEnum.simple = false
    val okVariant   = Type.EnumVariant("Ok", 0, List("0" -> Type.Int), carries = true)
    val failVariant = Type.EnumVariant("Err", 1, List("0" -> Type.Str), carries = true)
    resultEnum.variants = List(okVariant, failVariant)

    val dummyExprs: List[TExpr] = List(
      TIntLit(BigInt(1), Type.Int),
      TFloatLit(java.lang.Double.doubleToLongBits(1.5), Type.Floating(64)),
      TStrLit("s"),
      TBoolLit(true),
      TUnitLit(),
      TNullLit(Type.Ptr(Type.Int)),
      TBox(i1, Type.Ref(Type.Int, sync = false)),
      TDowngrade(TBox(i1, Type.Ref(Type.Int, sync = false)), Type.Weak(Type.Int)),
      TUpgrade(ld, optEnum, someVariant, noneVariant),
      TZero(Type.Int),
      TArrayLit(List(i1, i2), Type.Array(2, Type.Int)),
      TArrayFill(i1, Type.Array(3, Type.Int)),
      TVectorLit(List(i1, i2), Type.Vector(2, Type.Int)),
      TSplat(i1, Type.Vector(4, Type.Int)),
      TLane(vecLd, 0, Type.Int),
      TVecCompare("<", vecLd, vecLd, Type.Vector(4, Type.Bool)),
      TSelect(vecLd, vecLd, vecLd, Type.Vector(4, Type.Int)),
      TReduce("+", vecLd, Type.Int),
      TVecLoad(ld, i1, Type.Vector(4, Type.Int)),
      TVecStore(ld, i1, vecLd),
      TBufLit(List(i1, i2), Type.Slice(Type.Int)),
      TBufFill(i1, i2, Type.Slice(Type.Int)),
      TIndex(ld, i1, Type.Int),
      TLen(ld, Type.Int),
      TBytes(ld),
      TSlice(ld, Some(i1), Some(i2), inclusive = false, Type.Slice(Type.Int)),
      TCast(i1, Type.Floating(32)),
      TConstrainedCheck(i1, age),
      TConstrainedValid(i1, age),
      TConstrainedStep(i1, age, up = true, Type.Int),
      ld,
      TResult(Type.Int),
      TOld(0, Type.Int),
      TGlobal("g", Type.Int, writable = true),
      TDeref(ld, Type.Int),
      TTypeId(ld, Type.Int),
      TAddrOf(ld, Type.Ptr(Type.Int)),
      TTempAddr(i1, Type.Ptr(Type.Int)),
      TStore(ld, i1, Type.Int),
      TUpdate(ld, "+", i1, Type.Int, Some(TDispatch("add")), Some(age)),
      TIncDec(ld, "++", pre = true, Type.Int, Some(age)),
      TBinary("+", i1, i2, Type.Int),
      TUnary("-", i1, Type.Int),
      TIntOp("rotl", i1, Some(i2), Type.Int, Type.Int),
      TAtomic("add", ld, List(i1), "seq_cst", Type.Int, Type.Int),
      TFence("seq_cst"),
      TLogical("&&", bTrue, bTrue),
      TCompare(List(i1, i2), List(TCmp("<", Some(TDispatch("lt"))))),
      TSeq(List(i1, i2)),
      TStr(i1),
      TFromBytes(ld),
      TConstView(ld),
      TFormat(i1, "%d"),
      TRender(i1, "fmt", TStrLit("%d"), Some(0)),
      TCStrLit("s"),
      TCall("f", List(i1), Type.Int),
      TFuncAddr("f", "f$entry", Type.Ptr(Type.Int)),
      TCallPtr(ld, List(i1), List(Type.Int), Type.Int),
      TErase(ld, "Shape$Circle", Type.Ptr(Type.Int)),
      TVCall(ld, 0, List(i1), Type.Int),
      TVaStart(ld),
      TVaEnd(ld),
      TVaArg(ld, Type.Int),
      TVaCopy(ld, ld),
      TVaPass(ld),
      TStructNew(point, List(i1, i2)),
      TStructInvCheck(TStructNew(point, List(i1, i2)), point, "Point$inv"),
      TRecheck(i1, ld, point, "Point$inv"),
      TEnumNew(colorEnum, redVariant, Nil),
      TEnumFromInt(i1, colorEnum),
      TEnumTry(i1, colorEnum, optEnum, someVariant, noneVariant),
      TEnumAttr("Pos", colorEnum, i1, Type.Int),
      TTry(ld, okVariant, failVariant, resultEnum, failVariant, Type.Int, Some("From$convert")),
      TField(ld, 0, Type.Int),
      TIf(List(TCondTest(bTrue)), blk, Some(blk), Type.Int),
      TMatch(i1, List(TArm(List(TWildPattern(Type.Int)), None, blk)), Type.Int),
      TBlockExpr(blk),
      TWhile(List(TCondTest(bTrue)), List(TExprStmt(i1)), Some(blk), Type.Unit),
      TDoWhile(List(TExprStmt(i1)), bTrue, None, Type.Unit),
      TLoop(List(TBreak(None, 0)), Type.Unit),
      TFor("i", Type.Int, i1, i2, inclusive = true, List(TExprStmt(ld)), None, Type.Unit),
      TCFor(List(TVarDecl("i", Type.Int, i1)), Some(bTrue), List(TExprStmt(i1)), List(TExprStmt(ld)), None,
        Type.Unit),
      TQuantifier(universal = true, "i", Type.Int, i1, i2, inclusive = true, bTrue),
      TCheckedLoop("v0", Type.Int, TLoop(List(TBreak(None, 0)), Type.Unit)),
      TForEach("e", Type.Int, ld, List(TExprStmt(ld)), None, Type.Unit),
      TIterate("cur", Type.Int, i1, i2, TWildPattern(Type.Int), List(TExprStmt(ld)), None, Type.Unit),
    )

    val dummyStmts: List[TStmt] = List(
      TVarDecl("v", Type.Int, i1),
      TExprStmt(i1),
      TInvariant(bTrue, Some("msg")),
      TVariantCheck("slot", Type.Int, i1),
      TRefDecl("r", Type.Int, ld),
      TMultiAssign(List(TWrite(ld, "+", i1, None, Nil))),
      TReturn(Some(i1)),
      TBecome(TCall("f", Nil, Type.Int)),
      TBreak(Some(i1), 0),
      TContinue(0),
      TDefer(List(TExprStmt(i1))),
      TAsm(List("nop"), List(TAsmOperand(AsmDir.In, "x", "slot0", Type.Int, None)), List("rax")),
    )

    val dummyPatterns: List[TPattern] = List(
      TWildPattern(Type.Int),
      TBindPattern("x", Type.Int),
      TAtPattern("y", TWildPattern(Type.Int)),
      TLitPattern(i1),
      TRangePattern(i1, i2, inclusive = true),
      TVariantPattern(colorEnum, redVariant, Nil),
      TStructPattern(point, List(TWildPattern(Type.Int), TWildPattern(Type.Int))),
    )

    val dummyCondTerms: List[TCondTerm] = List(
      TCondTest(bTrue),
      TCondIs(ld, List(TWildPattern(Type.Int)), negated = false),
    )

    val coverageProgram: TProgram = {
      val patternMatch = TMatch(ld, dummyPatterns.map(p => TArm(List(p), None, blk)), Type.Int)
      val condWhile    = TWhile(dummyCondTerms, Nil, None, Type.Unit)
      val body = TBlock(
        dummyStmts ::: dummyExprs.map(TExprStmt.apply) ::: List(TExprStmt(patternMatch), TExprStmt(condWhile)),
        None,
        Type.Unit,
      )

      TProgram(List(point), List(colorEnum, optEnum, resultEnum), Nil, Nil, Nil,
        List(TFunc("cover", Nil, Type.Unit, body)), Nil)
    }

    "the reachable set is exactly the 107 node kinds the typed tree can hold" in {
      val printed = TypedAstPrinter.print(coverageProgram, spans = false)
      val found   = "[A-Z][A-Za-z0-9_]*".r.findAllIn(printed).toSet
      val missing = expectedTags -- found

      withClue(s"missing from the printed tree: $missing") { missing shouldBe empty }
    }
  }

  "an analysis error" - {

    "exits non-zero, prints the ordinary diagnostic on stderr, and prints nothing on stdout" in {
      val path = createTempFile("sysl-emit-typed-bad-", ".sysl")
      writeFile(path, "f(n: int) -> int = n + \"s\"\n")

      val out    = new java.io.ByteArrayOutputStream
      val err    = new java.io.ByteArrayOutputStream
      val status = Console.withOut(out)(Console.withErr(err)(sh.sysl.execute(Config(command = "emit-typed", file = path))))

      status should not be 0
      out.toString shouldBe ""
      err.toString should not be empty
    }

    "a parse error is reported the same way" in {
      val path = createTempFile("sysl-emit-typed-bad-", ".sysl")
      writeFile(path, "f(n: int -> int\n")

      val out    = new java.io.ByteArrayOutputStream
      val err    = new java.io.ByteArrayOutputStream
      val status = Console.withOut(out)(Console.withErr(err)(sh.sysl.execute(Config(command = "emit-typed", file = path))))

      status should not be 0
      out.toString shouldBe ""
      err.toString should not be empty
    }
  }

  "the CLI" - {

    "prints the tree on standard output, with no diagnostic, for a clean file" in {
      val path = createTempFile("sysl-emit-typed-ok-", ".sysl")
      writeFile(path, fixtureSrc)

      val out    = new java.io.ByteArrayOutputStream
      val err    = new java.io.ByteArrayOutputStream
      val status = Console.withOut(out)(Console.withErr(err)(sh.sysl.execute(Config(command = "emit-typed", file = path))))

      status shouldBe 0
      out.toString should include("TFunc")
      out.toString should include("name: \"add\"")
    }

    "--tables prints the declaration tables instead" in {
      val path = createTempFile("sysl-emit-typed-ok-", ".sysl")
      writeFile(path, fixtureSrc)

      val out    = new java.io.ByteArrayOutputStream
      val status = Console.withOut(out)(sh.sysl.execute(Config(command = "emit-typed", file = path, tables = true)))

      status shouldBe 0
      out.toString should include("funcs:")
      out.toString should include("add(a: int, b: int) -> int")
    }
  }
}

object TypedAstPrinterTests {

  /** The golden text `"prints the golden text …"` pins, held apart so the case above reads as the
   * assertion it is. Generated once against the fixture above and pinned here exactly as
   * `TypedAstPrinter` wrote it.
   */
  val goldenBare: String =
    """TProgram
      |  structs: [Point]
      |  enums: []
      |  vtables: []
      |  externs: []
      |  vals: []
      |  funcs: 
      |    - TFunc
      |        name: "add"
      |        params: 
      |          - Tuple2
      |              _1: "a"
      |              _2: int
      |          - Tuple2
      |              _1: "b"
      |              _2: int
      |        retTy: int
      |        body: TBlock
      |          stmts: []
      |          result: TBinary
      |            op: "+"
      |            left: TLoad
      |              name: "a"
      |              ty: int
      |            right: TLoad
      |              name: "b"
      |              ty: int
      |            ty: int
      |          ty: int
      |        variadic: false
      |        requires: []
      |        ensures: []
      |        olds: []
      |        internal: false
      |        conv: None
      |        tailrec: false
      |        variant: None
      |        pure: false
      |        ghost: false
      |        reads: None
      |        writes: None
      |        exported: None
      |        section: None
      |    - TFunc
      |        name: "f"
      |        params: 
      |          - Tuple2
      |              _1: "p"
      |              _2: Point
      |        retTy: int
      |        body: TBlock
      |          stmts: []
      |          result: TIf
      |            cond: 
      |              - TCondTest
      |                  cond: TCompare
      |                    operands: 
      |                      - TField
      |                          receiver: TLoad
      |                            name: "p"
      |                            ty: Point
      |                          index: 0
      |                          ty: int
      |                      - TIntLit
      |                          value: 0
      |                          ty: int
      |                    cmps: 
      |                      - TCmp
      |                          op: ">"
      |                          dispatch: None
      |            thenBlock: TBlock
      |              stmts: []
      |              result: TCall
      |                name: "add"
      |                args: 
      |                  - TField
      |                      receiver: TLoad
      |                        name: "p"
      |                        ty: Point
      |                      index: 0
      |                      ty: int
      |                  - TField
      |                      receiver: TLoad
      |                        name: "p"
      |                        ty: Point
      |                      index: 1
      |                      ty: int
      |                ty: int
      |                results: false
      |              ty: int
      |            elseBlock: TBlock
      |              stmts: []
      |              result: TIntLit
      |                value: 0
      |                ty: int
      |              ty: int
      |            ty: int
      |          ty: int
      |        variadic: false
      |        requires: []
      |        ensures: []
      |        olds: []
      |        internal: false
      |        conv: None
      |        tailrec: false
      |        variant: None
      |        pure: false
      |        ghost: false
      |        reads: None
      |        writes: None
      |        exported: None
      |        section: None
      |  main: []
      |  entry: None
      |  precompiled: []
      |  entryPoint: true
      |  cArtifact: false
      |  noAllocModules: []
      |  noAllocTestModules: []
      |  mainModule: ""
      |  tests: []
      |  hooks: []
      |  externVars: []
      |  testOnly: []
      |  destructors: {}
      |  warnings: []
      |  moduleDeps: {}
      |""".stripMargin

  /** The golden text `"--tables"`'s golden test pins, generated the same way. */
  val goldenTables: String =
    """structs:
      |  Point
      |    x: int
      |    y: int
      |enums:
      |traits:
      |impls:
      |externs:
      |consts:
      |funcs:
      |  add(a: int, b: int) -> int
      |  f(p: Point) -> int
      |""".stripMargin
}
