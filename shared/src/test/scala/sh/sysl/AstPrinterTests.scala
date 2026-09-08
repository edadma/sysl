package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `sysl emit-ast`'s printer: the untyped parse tree written out as deterministic text, which is
 * the whole of what an oracle over a second parser needs (`AstPrinter`'s own header comment has the
 * format).
 *
 * The golden fixture below is deliberately unexotic — a module header, a `const`, a `struct`, an
 * `enum`, a generic bounded function, and a block body — because it is asserted **exactly**, and an
 * exact assertion is only worth writing against syntax this suite already knows parses (every line
 * in it is lifted from `library/sysl/range.sysl`, `library/sysl/option.sysl` or
 * `AstCodecTests`'s own fixtures). The corners that syntax does not reach — every loop, every
 * pattern, `asm`, a `c const`/`c type` block, `static`, a vector type — are covered by
 * `"every node kind the tree can hold is reachable"` below, which builds its trees directly rather
 * than through the parser, so it needs no fragile syntax to reach a `Quantifier` or an `AsmBody`.
 */
class AstPrinterTests extends AnyFreeSpec with Matchers {

  private def parsed(src: String, name: String = "<t>"): Program =
    SyslParser.parse(Source(name, src)) match
      case Right(p) => p
      case Left(e)  => fail(s"the fixture does not parse: $e")

  private val fixture =
    """module demo
      |
      |const limit: int = 10
      |
      |struct Point
      |    x: int
      |    y: int
      |
      |enum Color
      |    Red
      |    Green
      |
      |double(n: int) -> int = n * 2
      |
      |larger[T: Ord](a: T, b: T) -> T = if a < b then b else a
      |
      |add(a: int, b: int) -> int
      |    val sum = a + b
      |    return sum
      |""".stripMargin

  "the format" - {

    "prints the golden text for a small program, exactly, with spans off" in {
      val program = parsed(fixture, "fixture.sysl")

      AstPrinter.print(program, spans = false) shouldBe AstPrinterTests.goldenBare
    }

    "is deterministic: the same tree prints the same text twice" in {
      val program = parsed(fixture)

      AstPrinter.print(program) shouldBe AstPrinter.print(program)
    }

    "with spans on, every node's line carries one, as 'line:col-line:col'" in {
      val program = parsed(fixture, "fixture.sysl")
      val printed = AstPrinter.print(program, spans = true)

      // Not every line: field lines (`name: "demo"`) carry no span of their own, only header lines
      // do. What is pinned is that the shape appears at all, and on the node it is expected on.
      printed should include regex """FuncDecl \d+:\d+-\d+:\d+"""
      printed should include regex """StructDecl \d+:\d+-\d+:\d+"""

      // And that turning them off really does turn them off, rather than merely being untested by
      // the golden case above.
      AstPrinter.print(program, spans = false) should not include regex("""\d+:\d+-\d+:\d+""")
    }

    "decodes a character literal's codepoint rather than printing the bare Int" in {
      val program = parsed("const c: char = 'A'\n")

      AstPrinter.print(program, spans = false) should include("codepoint: 65 'A'")
    }
  }

  /** Every node the grammar builds points at the source it was built from — which is a property of
   * the **parser** rather than of the printer, and is read here because a printed tree with spans on
   * is the one place the whole of it is visible at once.
   *
   * Two ways of losing a position are pinned, and neither shows up in what a program *means*:
   *
   *   - **A constant node shared between parses.** `^^^` evaluates its right side once, so a rule
   *     written `op("true") ^^^ BoolLit(true)` hands the same object back for every `true` in the
   *     process — and `setPos` keeps the first claim, so all of them report the first one's line.
   *   - **A node nothing in the source stands one-to-one for.** An interpolated string is a
   *     concatenation and an `elif` is a nested `if`, and neither is a token the enclosing rule's
   *     `at` reaches: only the outermost node of a rule's result is stamped.
   */
  "every node a rule builds carries its own position" - {

    /** The lines a node of one kind was reported on, in the order they print. */
    def linesOf(kind: String, src: String): List[Int] =
      (kind + """ (\d+):\d+-\d+:\d+""").r
        .findAllMatchIn(AstPrinter.print(parsed(src), spans = true))
        .map(_.group(1).toInt)
        .toList

    /** Node header lines printed with no span at all — the type name and nothing else.
     *
     * A field prints as `name: value`, and a node in a list prints under a `- `, so a bare
     * capitalised word on a line of its own is a node that was asked for its position and had none.
     * `Program` is the one legitimate answer: it is the file rather than anything in it, and carries
     * no position by design. Patterns carry none either, which is why no fixture here holds one.
     */
    def positionless(src: String): List[String] =
      AstPrinter
        .print(parsed(src), spans = true)
        .linesIterator
        .map(_.trim.stripPrefix("- "))
        .filter(_.matches("[A-Z][A-Za-z0-9]*"))
        .filterNot(_ == "Program")
        .toList

    "two 'true' literals on different lines each report their own line" in {
      linesOf("BoolLit", "val a = true\nval b = false\nval c = true\n") shouldBe List(1, 2, 3)
    }

    "and the span is the literal itself, not merely the right line" in {
      val printed = AstPrinter.print(parsed("val a = true\nval b = true\n"), spans = true)

      printed should include("BoolLit 1:9-1:13")
      printed should include("BoolLit 2:9-2:13")
    }

    "so do two 'null's" in {
      linesOf("NullLit", "val a = null\nval b = null\nval c = null\n") shouldBe List(1, 2, 3)
    }

    "so do two '()'s" in {
      linesOf("UnitLit", "val a = ()\nval b = ()\n") shouldBe List(1, 2)
    }

    "and two 'self's, in different methods" in {
      val src =
        """struct S
          |    n: int
          |
          |    a(self) -> int = self.n
          |    b(self) -> int = self.n
          |end S
          |""".stripMargin

      val selfs =
        """Ident (\d+):\d+-\d+:\d+\n\s+name: "self"""".r
          .findAllMatchIn(AstPrinter.print(parsed(src), spans = true))
          .map(_.group(1).toInt)
          .toList

      selfs shouldBe List(4, 5)
    }

    // The concatenation an interpolated string desugars to is built out of `StrLit`s, `Ident`s,
    // `Call`s and `Binary`s that no rule reads tokens for, so every one of them but the outermost
    // used to print bare.
    "an interpolated string's concatenation is positioned throughout" in {
      val src = "val n = 1\nval s = s\"a${n}b${n}c\"\nval g = f\"${n}%04d\"\n"

      positionless(src) shouldBe empty
    }

    // Each `elif` is a nested `if` in the else branch of the one before it, and the outer `if` is
    // the only node the rule's own `at` reaches.
    "an if/elif/else chain is positioned throughout" in {
      val src =
        """f(n: int) -> int =
          |    if n < 1
          |        10
          |    elif n < 2
          |        20
          |    elif n < 3
          |        30
          |    else
          |        40
          |""".stripMargin

      positionless(src) shouldBe empty
    }

    "and each nested 'if' points at the 'elif' that introduced it" in {
      val src =
        """f(n: int) -> int =
          |    if n < 1
          |        10
          |    elif n < 2
          |        20
          |    elif n < 3
          |        30
          |    else
          |        40
          |""".stripMargin

      linesOf("IfExpr", src) shouldBe List(2, 4, 6)
    }

    // `(..A)`, a type pack's tuple form, is read by `packTuple` as a `TupleType(List(PackType(n)))`
    // — the enclosing `at` around `coreType` reaches only the `TupleType`, so the `PackType` inside
    // it used to print bare.
    "a type pack's tuple form is positioned throughout" in {
      val src = "count[..A](t: (..A)) -> usize = 0\n"

      positionless(src) shouldBe empty
    }
  }

  "every node kind the tree can hold is reachable" - {

    // One instance of every case class/case object in `ast.scala` (`Expr`, 46), `astStmts.scala`
    // (`Stmt` and `AsmBody`, 31 + 2), `astTypes.scala` (`TypeRef`, 15) and `astPatterns.scala`
    // (`Pattern`, 9) — 103 in all, counted the same way `AstPrinter`'s own exhaustive matches are
    // written: `grep -c "^case class\|^case object"` over the four files, minus the handful of
    // `Positioned`-only helper types (`Param`, `MethodDecl`, `BoundRef`, …) that carry no sealed
    // hierarchy of their own and so have no exhaustiveness to pin. Built directly, node by node,
    // rather than parsed — a `Quantifier` or an `asm` block would cost real syntax risk for no
    // benefit, since this test is about the printer's coverage of the tree and not about the
    // grammar.
    val expectedTags = Set(
      // Expr
      "IntLit", "FloatLit", "CharLit", "StrLit", "CStrLit", "BoolLit", "UnitLit", "NullLit", "Ident",
      "Unary", "PreIncDec", "PostIncDec", "Binary", "Compare", "RangeExpr", "Assign", "Call",
      "NamedArg", "Spread", "DefaultArg", "Index", "Field", "TypeArgs", "TypeAttr", "WithExpr",
      "ImplicitMember", "LayoutOf", "OffsetOf", "TryExpr", "Tuple", "Lambda", "BlockArg", "ArrayLit",
      "ArrayFill", "Block", "IfExpr", "MatchExpr", "IsPattern", "ResultList", "While", "DoWhile",
      "Loop", "For", "ConstFor", "CFor", "Quantifier",
      // Stmt
      "ImportDecl", "VarDecl", "ConstDecl", "ValDecl", "StaticDecl", "CConstBlock", "CTypeBlock",
      "RefDecl", "MultiAssign", "MultiDecl", "PatternDecl", "ExprStmt", "Return", "Become", "Break",
      "Continue", "Defer", "AsmStmt", "AssertDecl", "Require", "Ensure", "Invariant", "Variant",
      "FuncDecl", "ExternDecl", "ExternVarDecl", "StructDecl", "EnumDecl", "TypeDecl", "TraitDecl",
      "ImplDecl",
      // TypeRef
      "NamedType", "ValueArgType", "PtrType", "RefType", "WeakType", "ArrayType", "VectorType",
      "VolatileType", "TupleType", "PackType", "FnType", "CFnType", "AssocType", "AssocArgType",
      "SomeType",
      // Pattern
      "LitPattern", "RangePattern", "WildcardPattern", "IdentPattern", "EqPattern", "VariantPattern",
      "StructPattern", "TuplePattern", "BindPattern",
      // AsmBody
      "AsmCode", "AsmUnavailable",
    )

    "the reference count is 103, the same total the AST files themselves grep to" in {
      expectedTags should have size 103
    }

    val patternArms = List(
      MatchArm(List(LitPattern(IntLit(BigInt(1), None))), None, List(ExprStmt(Ident("a")))),
      MatchArm(List(RangePattern(IntLit(BigInt(1), None), IntLit(BigInt(9), None), true)), None,
        List(ExprStmt(Ident("a")))),
      MatchArm(List(WildcardPattern), None, List(ExprStmt(Ident("a")))),
      MatchArm(List(IdentPattern("x")), None, List(ExprStmt(Ident("a")))),
      MatchArm(List(EqPattern("limit")), None, List(ExprStmt(Ident("a")))),
      MatchArm(List(VariantPattern("Circle", List(IdentPattern("r")))), None, List(ExprStmt(Ident("a")))),
      MatchArm(List(StructPattern("Point", List("x" -> IdentPattern("x")))), None, List(ExprStmt(Ident("a")))),
      MatchArm(List(TuplePattern(List(IdentPattern("a"), IdentPattern("b")))), None, List(ExprStmt(Ident("a")))),
      MatchArm(List(BindPattern("n", VariantPattern("Circle", List(IdentPattern("r"))))), None,
        List(ExprStmt(Ident("a")))),
    )

    val dummyExprs: List[Expr] = List(
      IntLit(BigInt(1), None),
      FloatLit("1.0", None),
      CharLit('A'.toInt),
      StrLit("s"),
      CStrLit("s"),
      BoolLit(true),
      UnitLit(),
      NullLit(),
      Ident("x"),
      Unary("-", Ident("x")),
      PreIncDec("++", Ident("x")),
      PostIncDec("++", Ident("x")),
      Binary("+", Ident("x"), Ident("y")),
      Compare(List(Ident("x"), Ident("y")), List("<")),
      RangeExpr(Some(Ident("x")), Some(Ident("y")), true),
      Assign("=", Ident("x"), Ident("y")),
      Call(Ident("f"), List(Ident("x"))),
      NamedArg("n", Ident("x")),
      Spread(Ident("xs")),
      DefaultArg(None, Ident("x")),
      Index(Ident("xs"), IntLit(BigInt(0), None)),
      Field(Ident("x"), "y"),
      TypeArgs(Ident("f"), List(Ident("x"))),
      TypeAttr(Ident("T"), "Attr"),
      WithExpr(Ident("x"), List(WithField("y", Ident("z")))),
      ImplicitMember("red"),
      LayoutOf("sizeof", NamedType("int")),
      OffsetOf(NamedType("T"), "f"),
      TryExpr(Ident("x")),
      Tuple(List(Ident("x"), Ident("y"))),
      Lambda(List(LambdaParam("x", None)), List(ExprStmt(Ident("x")))),
      BlockArg(List(ExprStmt(Ident("x")))),
      ArrayLit(List(Ident("x"))),
      ArrayFill(Ident("x"), IntLit(BigInt(3), None)),
      Block(List(ExprStmt(Ident("x")))),
      IfExpr(Ident("c"), List(ExprStmt(Ident("x"))), Some(List(ExprStmt(Ident("y"))))),
      MatchExpr(Ident("x"), patternArms),
      IsPattern(Ident("x"), List(WildcardPattern), false),
      ResultList(List(Ident("x"), Ident("y"))),
      While(None, Ident("c"), List(ExprStmt(Ident("x"))), None),
      DoWhile(None, List(ExprStmt(Ident("x"))), Ident("c"), None),
      Loop(None, List(Break(None, None))),
      For(None, "i", RangeExpr(Some(IntLit(BigInt(0), None)), Some(IntLit(BigInt(10), None)), false),
        List(ExprStmt(Ident("i"))), None),
      ConstFor("i", RangeExpr(Some(IntLit(BigInt(0), None)), Some(IntLit(BigInt(3), None)), false),
        List(ExprStmt(Ident("i")))),
      CFor(None, None, None, None, List(ExprStmt(Ident("x"))), None),
      Quantifier(true, "i", RangeExpr(Some(IntLit(BigInt(0), None)), Some(IntLit(BigInt(10), None)), false),
        BoolLit(true)),
    )

    val dummyTypes: List[TypeRef] = List(
      NamedType("int"),
      ValueArgType(IntLit(BigInt(4), None)),
      PtrType(NamedType("u8")),
      RefType(NamedType("T"), false),
      WeakType(NamedType("T")),
      ArrayType(Some(IntLit(BigInt(4), None)), NamedType("u8"), false),
      VectorType(IntLit(BigInt(4), None), NamedType("f32")),
      VolatileType(NamedType("u32")),
      TupleType(List(NamedType("int"), NamedType("int")), false),
      PackType("A"),
      FnType(List(NamedType("int")), NamedType("int"), true),
      CFnType(List(NamedType("int")), NamedType("int")),
      AssocType(NamedType("T"), "Body"),
      AssocArgType("Item", NamedType("string")),
      SomeType(List(BoundRef("View"))),
    )

    val dummyStmts: List[Stmt] = List(
      ImportDecl(List("a", "b")),
      VarDecl("v", Some(NamedType("int")), Some(IntLit(BigInt(1), None))),
      ConstDecl("c", NamedType("int"), IntLit(BigInt(1), None)),
      ValDecl("d", Some(NamedType("int")), IntLit(BigInt(1), None)),
      StaticDecl(ValDecl("e", None, IntLit(BigInt(1), None))),
      CConstBlock(List(CConstDecl("X", NamedType("int"), "1"))),
      CTypeBlock(List(CTypeDecl("Foo", "int"))),
      RefDecl("r", Ident("place")),
      MultiAssign("=", List(Ident("a"), Ident("b")), List(Ident("b"), Ident("a"))),
      MultiDecl(List("a", "b"), true, List(IntLit(BigInt(1), None), IntLit(BigInt(2), None))),
      PatternDecl(TuplePattern(List(IdentPattern("a"), IdentPattern("b"))), false,
        Tuple(List(IntLit(BigInt(1), None), IntLit(BigInt(2), None)))),
      ExprStmt(Ident("x")),
      Return(Some(Ident("x"))),
      Become(Call(Ident("f"), Nil)),
      Break(None, None),
      Continue(None),
      Defer(ExprStmt(Ident("x"))),
      AsmStmt(List(
        AsmArm(List("x86_64"), AsmCode(List("nop"), List(AsmOperand(AsmDir.In, "x", None)), List("rax"))),
        AsmArm(List("aarch64"), AsmUnavailable("no")),
      )),
      AssertDecl(BoolLit(true), Some("msg")),
      Require(BoolLit(true), Some("msg")),
      Ensure(BoolLit(true), Some("msg")),
      Invariant(BoolLit(true), Some("msg")),
      Variant(IntLit(BigInt(1), None)),
      FuncDecl("f", Nil, Nil, Some(NamedType("int")), List(ExprStmt(IntLit(BigInt(1), None)))),
      ExternDecl("g", Nil, Some(NamedType("int"))),
      ExternVarDecl("v2", NamedType("int")),
      StructDecl("S", Nil, List(Param("f", NamedType("int")))),
      EnumDecl("E", Nil, None, List(EnumVariantDecl("A", None, Nil))),
      TypeDecl("T", NamedType("int"), false, None, None),
      TraitDecl("Tr", Nil, List(MethodDecl("m", Some(RecvMode.ByValue), false, Nil, Nil, None, Nil))),
      ImplDecl("Tr", NamedType("S"), Nil),
    )

    val coverageProgram: Program = {
      val exprStmts = dummyExprs.map(ExprStmt.apply)
      val typeStmts = dummyTypes.map(t => VarDecl("t", Some(t), None))

      Program(dummyStmts ::: exprStmts ::: typeStmts, None, Nil, Nil, Source("<synthetic>", ""))
    }

    "the reachable set is exactly the 103 node kinds the tree can hold" in {
      val printed = AstPrinter.print(coverageProgram, spans = false)

      // A node's tag is not always at the start of its line: a list element's is, on its own
      // `- Tag` line, but a node standing directly in a field — `body: AsmCode`, with no list
      // between them — shares its line with the field name, as `fieldName: Tag`. So every
      // capitalized word anywhere on a line is a candidate, not only the first: none of the strings
      // this fixture writes happens to capitalize a word, so nothing but a real tag can match.
      val found = "[A-Z][A-Za-z0-9_]*".r.findAllIn(printed).toSet
      val missing = expectedTags -- found

      withClue(s"missing from the printed tree: $missing") { missing shouldBe empty }
    }
  }

  // Regression for a real cross-platform bug (`AstPrinter.tag`'s own comment has the story): a
  // parameterless `enum` case's Scala 3 `toString` agrees with its name on the JVM and on Scala.js
  // and answers its **ordinal** on Scala Native, so `vis: Public` printed as `vis: 1` under
  // `syslNative/test` alone. Found by the golden test above going red on exactly one platform with
  // exactly the same code — these five pin every case of every one of the tree's `enum`s by name, so
  // a reflective shortcut re-introduced here fails on the JVM as loudly as it fails on Native.
  "the five enums the tree carries print every case by name" - {

    "Visibility: Public, File, Scoped" in {
      val program = Program(
        List(
          VarDecl("a", None, None, Visibility.Public),
          VarDecl("b", None, None, Visibility.File),
          VarDecl("c", None, None, Visibility.Scoped("m")),
        ),
        None, Nil, Nil, Source("<t>", ""))
      val printed = AstPrinter.print(program, spans = false)

      printed should include("vis: Public")
      printed should include("vis: File")
      printed should include("vis: Scoped")
      printed should include("module: \"m\"")
      printed should not include "vis: 1"
      printed should not include "vis: 2"
    }

    "RecvMode: ByValue, ByPtr, ByRef" in {
      val members = List(
        MethodDecl("a", Some(RecvMode.ByValue), false, Nil, Nil, None, Nil),
        MethodDecl("b", Some(RecvMode.ByPtr), false, Nil, Nil, None, Nil),
        MethodDecl("c", Some(RecvMode.ByRef(true)), false, Nil, Nil, None, Nil),
      )
      val program = Program(List(StructDecl("S", Nil, Nil, members)), None, Nil, Nil, Source("<t>", ""))
      val printed = AstPrinter.print(program, spans = false)

      printed should include("ByValue")
      printed should include("ByPtr")
      printed should include("ByRef")
      printed should include("sync: true")
    }

    "AsmDir: In, Out" in {
      val arm = AsmArm(List("x86_64"),
        AsmCode(Nil, List(AsmOperand(AsmDir.In, "a", None), AsmOperand(AsmDir.Out, "b", None)), Nil))
      val program = Program(List(AsmStmt(List(arm))), None, Nil, Nil, Source("<t>", ""))
      val printed = AstPrinter.print(program, spans = false)

      printed should include regex "dir: In\\b"
      printed should include regex "dir: Out\\b"
    }

    "HookKind: Setup, Teardown, SetupAll, TeardownAll" in {
      val kinds = List(HookKind.Setup, HookKind.Teardown, HookKind.SetupAll, HookKind.TeardownAll)
      val decls = kinds.zipWithIndex.map((k, i) => FuncDecl(s"f$i", Nil, Nil, None, Nil, hook = Some(HookAttr(k))))
      val printed = AstPrinter.print(Program(decls, None, Nil, Nil, Source("<t>", "")), spans = false)

      printed should include("Setup")
      printed should include("Teardown")
      printed should include("SetupAll")
      printed should include("TeardownAll")
    }

    "CapabilityDirection: Narrows, Requires" in {
      val clauses = List(
        CapabilityClause(CapabilityDirection.Narrows, "alloc"),
        CapabilityClause(CapabilityDirection.Requires, "os"),
      )
      val printed = AstPrinter.print(Program(Nil, None, clauses, Nil, Source("<t>", "")), spans = false)

      printed should include("Narrows")
      printed should include("Requires")
    }
  }

  "a parse error" - {

    "exits non-zero, prints the ordinary diagnostic on stderr, and prints nothing on stdout" in {
      val path = createTempFile("sysl-ast-bad-", ".sysl")
      writeFile(path, "f(x: int -> int\n")

      val out    = new java.io.ByteArrayOutputStream
      val err    = new java.io.ByteArrayOutputStream
      val status = Console.withOut(out)(Console.withErr(err)(sh.sysl.execute(Config(command = "emit-ast", file = path))))

      status should not be 0
      out.toString shouldBe ""
      err.toString should not be empty
    }
  }
}

object AstPrinterTests {

  /** The golden text `"prints the golden text …"` pins, held apart so the case above reads as the
   * assertion it is rather than as the string. Generated once against the fixture above and pinned
   * here exactly as `AstPrinter` wrote it — see that test's own comment for why an exact assertion
   * is only written against syntax already known to parse.
   */
  val goldenBare: String =
    """Program
      |  body: 
      |    - ConstDecl
      |        name: "limit"
      |        typ: NamedType
      |          name: "int"
      |          args: []
      |        value: IntLit
      |          value: 10
      |          suffix: None
      |        vis: Public
      |    - StructDecl
      |        name: "Point"
      |        tparams: []
      |        fields: 
      |          - Param
      |              name: "x"
      |              typ: NamedType
      |                name: "int"
      |                args: []
      |              vis: Public
      |              default: None
      |              byName: false
      |              rest: false
      |          - Param
      |              name: "y"
      |              typ: NamedType
      |                name: "int"
      |                args: []
      |              vis: Public
      |              default: None
      |              byName: false
      |              rest: false
      |        members: []
      |        bounds: {}
      |        invariants: []
      |        vis: Public
      |        tdefaults: {}
      |        opaque: false
      |        tvalues: {}
      |        packed: false
      |        alignment: None
      |        cname: None
      |        deriving: []
      |    - EnumDecl
      |        name: "Color"
      |        tparams: []
      |        underlying: None
      |        variants: 
      |          - EnumVariantDecl
      |              name: "Red"
      |              value: None
      |              fields: []
      |          - EnumVariantDecl
      |              name: "Green"
      |              value: None
      |              fields: []
      |        members: []
      |        bounds: {}
      |        vis: Public
      |        tdefaults: {}
      |        tvalues: {}
      |        deriving: []
      |    - FuncDecl
      |        name: "double"
      |        tparams: []
      |        params: 
      |          - Param
      |              name: "n"
      |              typ: NamedType
      |                name: "int"
      |                args: []
      |              vis: Public
      |              default: None
      |              byName: false
      |              rest: false
      |        retType: NamedType
      |          name: "int"
      |          args: []
      |        body: 
      |          - ExprStmt
      |              expr: Binary
      |                op: "*"
      |                left: Ident
      |                  name: "n"
      |                right: IntLit
      |                  value: 2
      |                  suffix: None
      |        bounds: {}
      |        variadic: false
      |        vis: Public
      |        tdefaults: {}
      |        tvalues: {}
      |        tpacks: []
      |        test: None
      |        hook: None
      |        conv: None
      |        tailrec: false
      |        pure: false
      |        ghost: false
      |        reads: None
      |        writes: None
      |        exported: None
      |        section: None
      |        crossing: []
      |        needs: []
      |    - FuncDecl
      |        name: "larger"
      |        tparams: ["T"]
      |        params: 
      |          - Param
      |              name: "a"
      |              typ: NamedType
      |                name: "T"
      |                args: []
      |              vis: Public
      |              default: None
      |              byName: false
      |              rest: false
      |          - Param
      |              name: "b"
      |              typ: NamedType
      |                name: "T"
      |                args: []
      |              vis: Public
      |              default: None
      |              byName: false
      |              rest: false
      |        retType: NamedType
      |          name: "T"
      |          args: []
      |        body: 
      |          - ExprStmt
      |              expr: IfExpr
      |                cond: Compare
      |                  operands: 
      |                    - Ident
      |                        name: "a"
      |                    - Ident
      |                        name: "b"
      |                  ops: ["<"]
      |                thenBody: 
      |                  - ExprStmt
      |                      expr: Ident
      |                        name: "b"
      |                elseBody: 
      |                  - ExprStmt
      |                      expr: Ident
      |                        name: "a"
      |        bounds: 
      |          "T": 
      |            - BoundRef
      |                name: "Ord"
      |                args: []
      |        variadic: false
      |        vis: Public
      |        tdefaults: {}
      |        tvalues: {}
      |        tpacks: []
      |        test: None
      |        hook: None
      |        conv: None
      |        tailrec: false
      |        pure: false
      |        ghost: false
      |        reads: None
      |        writes: None
      |        exported: None
      |        section: None
      |        crossing: []
      |        needs: []
      |    - FuncDecl
      |        name: "add"
      |        tparams: []
      |        params: 
      |          - Param
      |              name: "a"
      |              typ: NamedType
      |                name: "int"
      |                args: []
      |              vis: Public
      |              default: None
      |              byName: false
      |              rest: false
      |          - Param
      |              name: "b"
      |              typ: NamedType
      |                name: "int"
      |                args: []
      |              vis: Public
      |              default: None
      |              byName: false
      |              rest: false
      |        retType: NamedType
      |          name: "int"
      |          args: []
      |        body: 
      |          - ValDecl
      |              name: "sum"
      |              typ: None
      |              value: Binary
      |                op: "+"
      |                left: Ident
      |                  name: "a"
      |                right: Ident
      |                  name: "b"
      |              vis: Public
      |              align: None
      |              section: None
      |          - Return
      |              value: Ident
      |                name: "sum"
      |        bounds: {}
      |        variadic: false
      |        vis: Public
      |        tdefaults: {}
      |        tvalues: {}
      |        tpacks: []
      |        test: None
      |        hook: None
      |        conv: None
      |        tailrec: false
      |        pure: false
      |        ghost: false
      |        reads: None
      |        writes: None
      |        exported: None
      |        section: None
      |        crossing: []
      |        needs: []
      |  module: ModuleName
      |    parts: ["demo"]
      |  capabilities: []
      |  links: []
      |  source: fixture.sysl
      |  testOnly: false
      |  includes: []
      |  docs: []
      |""".stripMargin
}
