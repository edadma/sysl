package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** How a **large** aggregate is handed about (`Layout.indirect`).
 *
 * A small one is a value: LLVM carries it in registers, and the `insertvalue` and `extractvalue`
 * instructions that build and read one cost nothing. Above `Layout.DirectBytes` that stops being
 * true — the back end is copying memory whatever the IR says, and a first-class value only obliges
 * every pass to reason about each byte of it. `guide/kernel` builds a 20 KB struct and hands it
 * about at thirty-five call sites; that module took 408 seconds to compile at `-O1` as a value and
 * 0.93 seconds through memory.
 *
 * So there are four lowerings here rather than one, and the tests come in pairs: what a large
 * aggregate does, and that a small one still does what it always did. A threshold with only one
 * side tested is a threshold that could be anywhere.
 */
class AggregateLoweringTests extends AnyFreeSpec with CodegenSupport with RunSupport {

  /** 512 bytes of cells and a tag — comfortably over the threshold, and with a field at each end so
   * a read of the last one cannot pass by reading the first.
   */
  private val big =
    """struct Big
      |    cells: [64]i64
      |    tag: int
      |""".stripMargin

  private val small =
    """struct Small
      |    x: int
      |    y: int
      |""".stripMargin

  /** A large struct that carries a counted reference, for the pointer-side ARC walk. */
  private val holder =
    """struct Node
      |    value: int
      |struct Holder
      |    pad: [64]i64
      |    node: &Node
      |""".stripMargin

  "a large result comes back through an out-pointer" - {
    "which is what the definition receives" in {
      val out = ir(big + "make(t: int) -> Big = Big([0; 64], t)\nprint(make(3).tag)")

      out should include("define void @make(ptr noalias sret(%struct.Big) align 8 %sret.out, i32 %t.param)")
      out should not include "ret %struct.Big"
    }

    "and the caller's own storage is what it points at" in {
      val out = irMain(big + "make(t: int) -> Big = Big([0; 64], t)\nvar b = make(7)\nprint(b.tag)")

      out should include("call void @make(ptr sret(%struct.Big) align 8 %b.addr, i32 7)")
      // Nothing anywhere makes a `%struct.Big` value: not the call, not the store into `b`.
      out should not include "load %struct.Big"
      out should not include "store %struct.Big"
    }

    "while a small one is still returned in registers" in {
      val out = ir(small + "make(x: int) -> Small = Small(x, 2)\nprint(make(1).y)")

      out should include("define %struct.Small @make(i32 %x.param)")
      out should include("ret %struct.Small")
      out should not include "sret(%struct.Small)"
    }
  }

  "a large literal is built where it is going to live" - {
    "field by field, with no chain of inserts" in {
      val out = ir(big + "make(t: int) -> Big = Big([0; 64], t)\nprint(make(1).tag)")

      out should not include "insertvalue %struct.Big"
      defineOf(out, "make") should include("getelementptr %struct.Big, ptr %sret.out, i32 0, i32 1")
    }

    "and a repeated element fills the destination rather than a buffer of its own" in {
      val out = defineOf(ir(big + "make(t: int) -> Big = Big([7; 64], t)\nprint(make(1).tag)"), "make")

      out should include("fill.test")
      out should not include "load [64 x i64]"
    }

    "while a small one is still the chain it always was" in {
      irMain(small + "var s = Small(1, 2)\nprint(s.x)") should include("insertvalue %struct.Small")
    }
  }

  "a field of a large aggregate is read at its address" - {
    "rather than by producing the whole value first" in {
      val out = irMain(big + "var b = Big([0; 64], 5)\nprint(b.tag)")

      out should include("getelementptr %struct.Big, ptr %b.addr, i32 0, i32 1")
      out should not include "extractvalue %struct.Big"
    }

    "while a field of a small one is still lifted out of it" in {
      val out = irMain(small + "var s = Small(1, 2)\nvar t = s\nprint(t.y)")

      out should include("extractvalue %struct.Small")
    }
  }

  "a copy of a large aggregate is a copy of bytes" - {
    "from one local to another" in {
      val out = irMain(big + "var a = Big([0; 64], 1)\nvar b = a\nprint(b.tag)")

      out should include("call void @llvm.memcpy.p0.p0.i64(ptr align 8 %b.addr, ptr align 8 %a.addr, i64 520, i1 false)")
    }

    "while a small one is still a load and a store" in {
      val out = irMain(small + "var a = Small(1, 2)\nvar b = a\nprint(b.x)")

      out should include("load %struct.Small, ptr %a.addr")
      out should not include "llvm.memcpy"
    }
  }

  "a large argument crosses the call in memory" - {
    "so the parameter is declared as an address" in {
      val out = ir(big + "tag(b: Big) -> int = b.tag\nprint(tag(Big([0; 64], 6)))")

      out should include("define i32 @tag(ptr %b.param)")
      defineOf(out, "tag") should include(
        "call void @llvm.memcpy.p0.p0.i64(ptr align 8 %b.addr, ptr align 8 %b.param, i64 520, i1 false)")
    }

    "and the caller hands over storage it already has" in {
      val out = irMain(big + "tag(b: Big) -> int = b.tag\nvar b = Big([0; 64], 6)\nprint(tag(b))")

      out should include("call i32 @tag(ptr %b.addr)")
    }

    "while a small one is still passed in registers" in {
      ir(small + "sum(s: Small) -> int = s.x + s.y\nprint(sum(Small(1, 2)))") should
        include("define i32 @sum(%struct.Small %s.param)")
    }

    // Handing over an address is only a lowering if the callee's copy is genuinely its own. A
    // callee that wrote through the caller's storage would pass every test above and this one is
    // what it would fail.
    "and what the callee does to its copy stays in the callee" in {
      run(big +
        """spoil(b: Big) -> int
          |    var c = b
          |    c.tag = 99
          |    c.cells[0] = 99
          |    c.tag
          |var b = Big([1; 64], 2)
          |print(spoil(b), b.tag, b.cells[0])""".stripMargin) shouldBe "99 2 1\n"
    }
  }

  /** A comparison and a compound assignment reach an operator's trait method with the operands
   * already evaluated (`ArithEmitter.dispatchValue`), which is a **second route** to a call the
   * definition knows nothing about — so it owes the boundary everything the ordinary route owes it.
   *
   * Nothing else would catch getting that wrong. A `call` naming the aggregate where the definition
   * names a pointer assembles and links: the callee reads the first word of the value as the
   * address it was promised, and what the program does then is whatever that word happened to be.
   */
  "an operator dispatched to a trait method crosses the boundary the same way" - {
    val comparable =
      big +
        """impl Eq for Big
          |    eq(self, rhs: Self) -> bool = self.tag == rhs.tag
          |""".stripMargin

    "so '==' hands its operands over at an address" in {
      val out = ir(comparable + "var b = Big([0; 64], 1)\nprint(b == b)")

      out should include("define zeroext i1 @Big.eq(ptr %self.param, ptr %rhs.param)")
      mainOf(out) should include("call zeroext i1 @Big.eq(ptr")
      mainOf(out) should not include "@Big.eq(%struct.Big"
    }

    "and a compound assignment takes its result back through the out-pointer" in {
      val out = ir(big +
        """impl Add for Big
          |    add(self, rhs: Self) -> Self = Big([0; 64], self.tag + rhs.tag)
          |var b = Big([0; 64], 1)
          |b += b
          |print(b.tag)""".stripMargin)

      out should include("define void @Big.add(ptr noalias sret(%struct.Big) align 8 %sret.out, ptr %self.param, ptr %rhs.param)")
      mainOf(out) should include("call void @Big.add(ptr sret(%struct.Big) align 8 ")
      mainOf(out) should not include "@Big.add(%struct.Big"
    }

    "while a small operand is still handed over as a value" in {
      val out = ir(small +
        """impl Eq for Small
          |    eq(self, rhs: Self) -> bool = self.x == rhs.x
          |var s = Small(1, 2)
          |print(s == s)""".stripMargin)

      out should include("define zeroext i1 @Small.eq(%struct.Small %self.param, %struct.Small %rhs.param)")
      mainOf(out) should include("call zeroext i1 @Small.eq(%struct.Small")
    }
  }

  // The threshold is a number, so the test that matters is the one either side of it. Sixteen
  // 8-byte cells are exactly `Layout.DirectBytes`; seventeen are one word past it.
  "the threshold falls where Layout says it does" - {
    "an aggregate of exactly DirectBytes is still a value" in {
      Layout.DirectBytes shouldBe 128

      val out = ir("struct At\n    cells: [16]i64\nmake() -> At = At([0; 16])\nprint(make().cells[0])")

      out should include("ret %struct.At")
    }

    "and one word past it is not" in {
      val out = ir("struct Over\n    cells: [17]i64\nmake() -> Over = Over([0; 17])\nprint(make().cells[0])")

      out should include("sret(%struct.Over)")
    }
  }

  "the values it produces are the values the program wrote" - {
    "a large struct survives being returned, bound and read" in {
      run(big +
        """make(t: int) -> Big
          |    var c: [64]i64 = [0; 64]
          |    c[0] = 11
          |    c[63] = 22
          |    Big(c, t)
          |var b = make(33)
          |print(b.cells[0], b.cells[63], b.tag)""".stripMargin) shouldBe "11 22 33\n"
    }

    // The memcpy has to be a *copy*. Aliasing here would be invisible until something wrote.
    "a copy is independent of what it was copied from" in {
      run(big +
        """var a = Big([0; 64], 1)
          |var b = a
          |a.tag = 9
          |a.cells[3] = 4
          |print(b.tag, b.cells[3], a.tag, a.cells[3])""".stripMargin) shouldBe "1 0 9 4\n"
    }

    "an assignment over an existing one replaces all of it" in {
      run(big +
        """var a = Big([1; 64], 1)
          |var b = Big([2; 64], 2)
          |a = b
          |print(a.cells[0], a.cells[63], a.tag)""".stripMargin) shouldBe "2 2 2\n"
    }

    "an early return writes the same storage the fall-through would have" in {
      run(big +
        """pick(which: bool) -> Big
          |    if which
          |        return Big([5; 64], 1)
          |    Big([6; 64], 2)
          |print(pick(true).cells[0], pick(false).cells[0])""".stripMargin) shouldBe "5 6\n"
    }

    "a large array is returned the same way a large struct is" in {
      run(
        """make() -> [64]i64
          |    var c: [64]i64 = [0; 64]
          |    c[63] = 8
          |    c
          |print(make()[63])""".stripMargin) shouldBe "8\n"
    }

    "and a large one nested inside a larger one still lands in the right place" in {
      run(
        """struct Inner
          |    cells: [32]i64
          |struct Outer
          |    a: Inner
          |    b: Inner
          |    tag: int
          |make() -> Outer = Outer(Inner([1; 32]), Inner([2; 32]), 3)
          |var o = make()
          |print(o.a.cells[0], o.b.cells[31], o.tag)""".stripMargin) shouldBe "1 2 3\n"
    }
  }

  // Five lowerings changed at once, so what is worth asking is not whether each works but whether
  // anything a large aggregate can still *reach* was left behind. Each of these is a path where the
  // value form is still what runs, or where the destination is not known until the expression is
  // half-emitted — and each has to keep producing the right answer either way.
  "everywhere else a large one can turn up still works" - {
    "a branch that yields one" in {
      run(big +
        """pick(which: bool) -> Big = if which then Big([1; 64], 1) else Big([2; 64], 2)
          |print(pick(true).cells[0], pick(false).cells[0])""".stripMargin) shouldBe "1 2\n"
    }

    "an arm of a match that yields one" in {
      run(big +
        """pick(n: int) -> Big = n match
          |    0 -> Big([5; 64], 0)
          |    _ -> Big([6; 64], 1)
          |print(pick(0).cells[0], pick(1).cells[0])""".stripMargin) shouldBe "5 6\n"
    }

    // The box's address does not exist until `malloc` has returned, so this is the one destination
    // that cannot be handed to the expression before the expression starts.
    "one put on the heap behind a reference" in {
      run(big +
        """var b: &Big = Big([3; 64], 4)
          |var c = b
          |c.tag = 5
          |print(b.cells[63], b.tag)""".stripMargin) shouldBe "3 5\n"
    }

    "an element of an array of them" in {
      run(big +
        """var bs: [3]Big = [Big([7; 64], 1); 3]
          |bs[1].tag = 2
          |print(bs[0].cells[0], bs[0].tag, bs[1].tag)""".stripMargin) shouldBe "7 1 2\n"
    }

    "one a closure captured and returns" in {
      run(big +
        """call(f: () -> Big) -> int = f().tag
          |var b = Big([8; 64], 9)
          |print(call(() -> b))""".stripMargin) shouldBe "9\n"
    }

    "one bound at module level" in {
      run(big +
        """val shared: Big = Big([4; 64], 5)
          |print(shared.cells[0], shared.tag)""".stripMargin) shouldBe "4 5\n"
    }

    "one behind a pointer, written through" in {
      run(big +
        """bump(p: *Big)
          |    p.tag = p.tag + 1
          |var b = Big([0; 64], 1)
          |bump(&b)
          |print(b.tag)""".stripMargin) shouldBe "2\n"
    }

    "and the two the edge-case pass turned up are destinations too" in {
      val boxed = irMain(big + "var b: &Big = Big([3; 64], 4)\nprint(b.tag)")

      boxed should not include "insertvalue %struct.Big"
      boxed should not include "store %struct.Big"

      val tagged = ir(big +
        """enum Slot
          |    Empty
          |    Full(b: Big)
          |take(s: Slot) -> int = 0
          |print(take(Full(Big([0; 64], 6))))""".stripMargin)

      mainOf(tagged) should not include "load %enum.Slot"
    }

    "and a large payload inside a data enum" in {
      run(big +
        """enum Slot
          |    Empty
          |    Full(b: Big)
          |take(s: Slot) -> int = s match
          |    Full(b) -> b.tag
          |    Empty -> 0
          |print(take(Full(Big([0; 64], 6))), take(Empty))""".stripMargin) shouldBe "6 0\n"
    }
  }

  /** `?` out of a function whose result is large, which is an early `return` and has to leave the
    * same way the written one does — through the out-pointer, with the function itself `void`.
    *
    * It did not, and the failure was **clang refusing the compiler's own IR**: a `ret` of the
    * aggregate out of a function the ABI had already made `void`. The message named LLVM's textual
    * IR in a temporary file the driver deletes, and named `void`, so it read as a fault in the C
    * toolchain rather than in the sysl that was written. The analysis passed all the way to clang,
    * so a package could be type-correct, reviewed and unbuildable.
    *
    * Where it bites is bindings: a `?` on a status code followed by `Ok(<something big>)` is the
    * shape of every constructor in a binding to a C library with caller-placed storage. It was found
    * in `sysl-lang/libuv`, whose address type is a 128-byte `sockaddr_storage`.
    */
  "an early return out of one leaves through the out-pointer too" - {
    val chk = "chk(c: int) -> Result[unit, int] = if c < 0 then Err(c) else Ok(())\n"

    "the value arrives when nothing fails" in {
      run(big + chk +
        """mk(x: int) -> Result[Big, int]
          |    chk(x)?
          |    Ok(Big([1; 64], 2))
          |print(mk(3).unwrap().tag)""".stripMargin) shouldBe "2\n"
    }

    "and the failure arrives when something does" in {
      run(big + chk +
        """mk(x: int) -> Result[Big, int]
          |    chk(x)?
          |    Ok(Big([1; 64], 2))
          |mk(-7) match
          |    Err(e) -> print("err", e)
          |    Ok(_) -> print("ok")""".stripMargin) shouldBe "err -7\n"
    }

    // Two of them, since each emits its own early return and the second one's block is reached only
    // after the first has already terminated one.
    "twice in one function" in {
      run(big + chk +
        """mk(x: int) -> Result[Big, int]
          |    chk(x)?
          |    chk(x)?
          |    Ok(Big([1; 64], 3))
          |print(mk(3).unwrap().tag, mk(-1).is_err())""".stripMargin) shouldBe "3 true\n"
    }

    // The IR side of the same claim: the function is `void` with an `sret` parameter, so no `ret`
    // in it may carry the enum. Asserted on the text because the run tier above passes on any IR
    // clang happens to accept, and what was wrong here was IR clang did not.
    "and no return in it hands the result back directly" in {
      val text = ir(big + chk +
        """mk(x: int) -> Result[Big, int]
          |    chk(x)?
          |    Ok(Big([1; 64], 2))
          |print(mk(3).unwrap().tag)""".stripMargin)

      text should include("sret")
      // Named at the *big* instantiation: `chk` returns a small `Result[unit, int]` and hands it
      // back directly, which is correct and is what a bare `Result` in the pattern would have
      // caught instead.
      text should not include "ret %enum.sysl$Result.Big.int"
    }
  }

  /** Rendering one, which hands it to `display` as an **argument** — and a large argument crosses at
    * an address, so producing it as a first-class value put the struct's first word where the callee
    * reads a pointer.
    *
    * `print(x)` went through an ordinary call and worked; `str(x)` and an interpolation built the
    * string first and took the broken path, so a type rendered all through development and died the
    * first time it was put in a message — as a bare `SIGSEGV`, since a crash discards what the
    * program had already printed.
    *
    * There was no workaround at the `impl` site either: a reference receiver is refused as differing
    * from the trait's, so the rule as it stood was that a type over the threshold could not implement
    * `Display` at all.
    */
  "rendering one hands it over at an address, like any other large argument" - {
    val shown =
      "impl Display for Big\n" +
        "    display(self, out: *Writer, fmt: FormatSpec) = self.tag.display(out, fmt)\n"

    "print, which was always the working path" in {
      run(big + shown + "var b = Big([0; 64], 7)\nprint(b)") shouldBe "7\n"
    }

    "str, which built the string first and did not" in {
      run(big + shown + "var b = Big([0; 64], 7)\nprint(str(b))") shouldBe "7\n"
    }

    "an interpolation, which is the shape a message is actually written in" in {
      run(big + shown + "var b = Big([0; 64], 7)\nprint(s\"<$b>\")") shouldBe "<7>\n"
    }

    "and one through a specifier, which is the other renderer" in {
      run(big + shown + "var b = Big([0; 64], 7)\nprint(f\"${b}%3s|\")") shouldBe "  7|\n"
    }

    // A small one keeps crossing as a value, which is the other side of the threshold and the reason
    // the fix is a branch rather than a change of convention.
    "while a small one still crosses as a value" in {
      run(small +
        "impl Display for Small\n" +
          "    display(self, out: *Writer, fmt: FormatSpec) = self.y.display(out, fmt)\n" +
          "var s = Small(1, 4)\nprint(str(s))") shouldBe "4\n"
    }
  }

  // The value a postcondition is written about is read back out of the out-pointer, which is the
  // one whole-aggregate load this lowering still emits — and only on a function that asked for it.
  "a contract on one still sees the value it is written about" in {
    run(big +
      """make(t: int) -> Big
        |    ensure result.tag == t
        |    Big([0; 64], t)
        |print(make(4).tag)""".stripMargin) shouldBe "4\n"
  }

  "a large result reached through a trait object goes through the same out-pointer" in {
    run(big +
      """trait Source
        |    supply(self) -> Big
        |struct Ones
        |    n: int
        |struct Twos
        |    n: int
        |impl Source for Ones
        |    supply(self) -> Big = Big([1; 64], self.n)
        |impl Source for Twos
        |    supply(self) -> Big = Big([2; 64], self.n)
        |var a = Ones(10)
        |var b = Twos(20)
        |var s: *Source = &a
        |print(s.supply().cells[0], s.supply().tag)
        |s = &b
        |print(s.supply().cells[0], s.supply().tag)""".stripMargin) shouldBe "1 10\n2 20\n"
  }

  // The counts are what the pointer-side `retainAt`/`releaseAt` exist for: a large struct never
  // becomes a value, so there is nothing to hand the value-side walk. A weak reference is the
  // observable — it reports the object gone exactly when the last strong count went.
  "a large aggregate that carries references still counts them" - {
    "a copy takes a share, so the object outlives the one it was copied from" in {
      run(holder +
        """keep() -> weak Node
          |    var n: &Node = Node(5)
          |    var w: weak Node = n
          |    var a = Holder([0; 64], n)
          |    var b = a
          |    print(str(b.node.value))
          |    w
          |var w = keep()
          |print(str(w.get().is_none()))""".stripMargin) shouldBe "5\ntrue\n"
    }

    "and one built by a callee keeps what the caller handed it" in {
      run(holder +
        """make(n: &Node) -> Holder = Holder([0; 64], n)
          |var n: &Node = Node(7)
          |var h = make(n)
          |print(str(h.node.value))""".stripMargin) shouldBe "7\n"
    }

    "an assignment over one lets go of what was there" in {
      run(holder +
        """probe() -> weak Node
          |    var first: &Node = Node(1)
          |    var w: weak Node = first
          |    var h = Holder([0; 64], first)
          |    h = Holder([0; 64], Node(2))
          |    print(str(h.node.value))
          |    w
          |print(str(probe().get().is_none()))""".stripMargin) shouldBe "2\ntrue\n"
    }
  }
}
