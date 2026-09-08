package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** What an `if` or a `match` used as a value does when one of its branches is a bare literal.
 *
 * A branch is one of two — or of many — positions that have to agree on one type, which is the
 * shape a binary operator's operands and a range's two ends already have: the side that knows what
 * it is supplies the type, and the side that has none takes it. `n + 1` has never needed a suffix
 * for that reason, and `if n == 0 then 1 else n` had to have one until this.
 *
 * The other half is what has *not* moved, and it is what the rule was protecting: two branches that
 * each genuinely know what they are still have to agree, in the same words as before. A literal is
 * the exception because it has no opinion to be overridden — nothing is being silently decided by
 * whichever branch the reader looked at second.
 */
class BranchTypingTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  "a bare literal takes the type of the branch beside it" - {
    // The seam is the *call*, not the arithmetic: `width` takes a `usize`, so an `if` that came out
    // as `int` does not compile at all. Asserting only that the program runs would pass on a
    // program where both branches had quietly become `int`.
    "in the else branch, checked by handing the result to something that takes a usize" in {
      val src =
        """width(n: usize) -> usize = n
          |
          |gap(a: usize, b: usize) -> usize =
          |    width(if a > b then a - b else 0)
          |
          |print(gap(9, 2))
          |print(gap(2, 9))""".stripMargin

      run(src) shouldBe "7\n0\n"
    }

    "in the then branch, which is the direction that needs the else read first" in {
      val src =
        """width(n: usize) -> usize = n
          |
          |atLeastOne(n: usize) -> usize =
          |    width(if n == 0 then 1 else n)
          |
          |print(atLeastOne(0))
          |print(atLeastOne(4))""".stripMargin

      run(src) shouldBe "1\n4\n"
    }

    // The declaration is where the annotation used to have to go, and it said nothing a reader
    // wanted to know — `val w: usize` above a line whose other branch is already a `usize`.
    "a val takes the width without being told it" in {
      val src =
        """f(n: usize) -> usize =
          |    val w = if n == 0 then 1 else n
          |    w
          |
          |print(f(0))""".stripMargin

      run(src) shouldBe "1\n"
    }

    "an elif chain settles from the one branch that knows" in {
      val src =
        """width(n: usize) -> usize = n
          |
          |f(n: usize) -> usize =
          |    width(if n == 0 then 1 elif n == 1 then 2 else n)
          |
          |print(f(0))
          |print(f(1))
          |print(f(6))""".stripMargin

      run(src) shouldBe "1\n2\n6\n"
    }

    // Not an integer-only rule: a float literal has no width of its own either, and `0.0` beside an
    // `f32` used to fall to `real` and be refused for it.
    "a float literal takes its sibling's width" in {
      val src =
        """take(x: f32) -> f32 = x
          |
          |clamp(x: f32) -> f32 =
          |    take(if x > 0.0f32 then x else 0.0)
          |
          |print(clamp(2.5f32))
          |print(clamp(-1.0f32))""".stripMargin

      run(src) shouldBe "2.5\n0\n"
    }
  }

  "a match arm is the same rule over as many alternatives as the form has" - {
    "a literal arm takes what the arms that know settled" in {
      val src =
        """width(n: usize) -> usize = n
          |
          |f(n: usize) -> usize =
          |    val w = n match
          |        0 -> 1
          |        else n
          |    width(w)
          |
          |print(f(0))
          |print(f(3))""".stripMargin

      run(src) shouldBe "1\n3\n"
    }

    // The arm that knows is the *last* one here, so the rule cannot be "the first arm decides".
    "the arm that knows may be written after the ones that do not" in {
      val src =
        """width(n: usize) -> usize = n
          |
          |f(n: usize) -> usize =
          |    val w = n match
          |        0 -> 1
          |        1 -> 2
          |        else n
          |    width(w)
          |
          |print(f(0))
          |print(f(1))
          |print(f(8))""".stripMargin

      run(src) shouldBe "1\n2\n8\n"
    }

    // An arm that does not finish settles nothing, so it must not be mistaken for the arm that
    // knows — `never` is set aside here exactly as `matchResultType` sets it aside.
    "an arm that does not finish still settles nothing" in {
      val src =
        """width(n: usize) -> usize = n
          |
          |f(n: usize) -> usize =
          |    val w = n match
          |        0 -> 1
          |        1 -> return 99
          |        else n
          |    width(w)
          |
          |print(f(0))
          |print(f(1))
          |print(f(5))""".stripMargin

      run(src) shouldBe "1\n99\n5\n"
    }
  }

  /** The one pair of *known* types that has a meeting point, and so the one exception to the section
    * below: a `[]T` and a `[]const T` are one type with a bit, not two types, and the writable one
    * is accepted wherever the read-only one is wanted.
    *
    * They meet at the read-only view, which is the only view either side converts to. Without it the
    * form's type depended on **which branch was written first** — the same defect a pair of operands
    * had, and `OperatorDomainTests` is where that half is pinned.
    */
  "two views of one slice meet at the read-only one" - {
    val setup =
      """var a = [1, 2, 3]
        |var b = [1, 2, 3]
        |val c: []const int = b[..]
        |""".stripMargin

    "however the branches are ordered" in {
      run(setup +
        """val j = if a.len > 0 then a[..] else c
          |val k = if a.len > 0 then c else a[..]
          |print(j.len, k.len)
          |""".stripMargin) shouldBe "3 3\n"
    }

    // The result is the read-only view, not the writable one, which is what makes the meeting safe
    // rather than a way of laundering a `[]const T` into a `[]T`.
    "and the result may not be written through" in {
      err(setup +
        """var j = if a.len > 0 then a[..] else c
          |j[0] = 9
          |""".stripMargin) should include("views elements it may not write")
    }

    "a match settles the same way" in {
      run(setup +
        """val k = a.len match
          |    0 -> c
          |    else a[..]
          |print(k.len)
          |""".stripMargin) shouldBe "3\n"
    }
  }

  "what the rule does not do" - {
    // The property the refusal exists for: neither side is silently decided by the other, because
    // both of them arrived with an opinion.
    "two branches that each know their type still have to agree" in {
      err("""f(a: int, b: usize) =
            |    val w = if a == 0 then a else b
            |    print(w)
            |""".stripMargin) should include("if branches have different types: int and usize")
    }

    "and so do two arms" in {
      err("""f(a: int, b: usize) =
            |    val w = a match
            |        0 -> a
            |        else b
            |    print(w)
            |""".stripMargin) should include("match arms have different types: int and usize")
    }

    // A literal is adaptable, not universal: nothing makes an integer literal into a string, so the
    // refusal is the one it always was.
    "a literal beside something it cannot be is refused as before" in {
      err("""f(s: string) =
            |    val w = if true then 1 else s
            |    print(w)
            |""".stripMargin) should include("if branches have different types")
    }

    // With a type already written, both branches were being told it before this rule existed and
    // still are — the literal is checked against what was asked for rather than against its sibling.
    "a written type still overrules both branches" in {
      err("""f(b: usize) =
            |    val w: string = if true then 1 else b
            |    print(w)
            |""".stripMargin) should not be empty
    }
  }

  // Card `0294`, from slate. A bare `None` is the same shape as a bare literal one tier along: it is
  // not a literal, so the rule above never reached it, and analyzed alone it is asking what an
  // `Option` of nothing in particular holds. The sibling says.
  "a payload-free variant of a generic enum takes its sibling's type too" - {
    "in the else branch, which is where it was refused" in {
      val src =
        """struct Expr
          |    n: int
          |
          |maybe(c: bool) -> Option[Expr]
          |    val b = if c then Some(Expr(1)) else None
          |    b
          |
          |print(maybe(true).unwrap().n, maybe(false).is_none())
          |""".stripMargin

      run(src) shouldBe "1 true\n"
    }

    "and in the then branch, which is the same rule read the other way" in {
      val src =
        """maybe(c: bool) -> Option[int]
          |    val b = if c then None else Some(4)
          |    b
          |
          |print(maybe(false).unwrap(), maybe(true).is_none())
          |""".stripMargin

      run(src) shouldBe "4 true\n"
    }

    "a match arm settles it the same way" in {
      val src =
        """pick(n: int) -> Option[int]
          |    val b = n match
          |        0 -> None
          |        _ -> Some(n)
          |    b
          |
          |print(pick(0).is_none(), pick(5).unwrap())
          |""".stripMargin

      run(src) shouldBe "true 5\n"
    }

    // A `Result` is the other generic enum with a payload-free variant in reach, and nothing about
    // the rule is special to `Option` — it is asked of the declaration.
    "and it is not special to Option" in {
      val src =
        """enum Step[T]
          |    Done
          |    Going(v: T)
          |
          |go(c: bool) -> Step[int]
          |    val s = if c then Going(2) else Done
          |    s
          |
          |go(true) match
          |    Going(v) -> print(v)
          |    Done -> print("done")
          |""".stripMargin

      run(src) shouldBe "2\n"
    }

    // What has NOT moved: with both branches payload-free there is nothing to settle it, so the
    // annotation is still the only thing that can say — exactly as two bare literals still fall to
    // `int`. The refusal is the same one, and it is right.
    "while two variants that both carry nothing still have nothing to go on" in {
      err("""f(c: bool) =
            |    val b = if c then None else None
            |    print(b.is_none())
            |""".stripMargin) should include("cannot infer the type argument")
    }

    // And a variant that CARRIES something is an answer rather than a question, so it keeps supplying
    // the type rather than taking one — which is what makes the first case above work at all. The
    // payload is a `usize` here rather than a bare `1`, because a literal inside the carrying variant
    // is the *older* tiering and would fall to `int` with nothing to say otherwise: `Some(1)` beside
    // a `None` settles the pair at `Option[int]`, correctly, and that is a separate rule from this
    // one.
    "and a carrying variant still supplies the type rather than taking one" in {
      val src =
        """f(c: bool, n: usize) -> Option[usize]
          |    val b = if c then Some(n) else None
          |    b
          |
          |print(f(true, 1).unwrap() + 2usize)
          |""".stripMargin

      run(src) shouldBe "3\n"
    }

    // The same pair with a bare literal in the payload, pinned because it is the case the comment
    // above is about: nothing here says `usize`, so the whole thing is an `Option[int]`.
    "with a bare payload the pair settles at int, which is the older rule and unchanged" in {
      val src =
        """f(c: bool) -> Option[int]
          |    val b = if c then Some(1) else None
          |    b
          |
          |print(f(true).unwrap(), f(false).is_none())
          |""".stripMargin

      run(src) shouldBe "1 true\n"
    }
  }

  /** A **nullary generic call** is the third form with no type of its own, and it is the one neither
    * tier above can recognise from the syntax.
    *
    * `buf()` has nothing in its argument list to fix `T`, so it is solved from the expected type
    * (`reference/generics.md § Inference is bidirectional`) and simply fails where there is none.
    * Under a declared result the `if` already compiled, because the result *was* that expectation;
    * standing on its own it was refused — *"cannot infer the type argument 'T' of 'sysl.buf.buf'"* —
    * for a type the branch beside it knew all along, and the annotation it asked for restated what
    * the other branch already returns.
    *
    * The reading is what tells the forms apart: a bare literal is known to be adaptable before
    * anything is analyzed, and a call is known only by being tried. So a branch with nothing to go
    * on is tried, and one that cannot stand alone is held over until its sibling has spoken.
    */
  "a branch that cannot be read alone takes its sibling's type" - {
    "the else branch, which is the order it was reported in" in {
      val src =
        """import sysl.buf.*
          |
          |mk() -> Buf[int]
          |    var b: Buf[int] = buf()
          |    b.push(7)
          |    b
          |
          |f(c: bool) -> usize
          |    var xs = if c then mk() else buf()
          |    xs.push(1)
          |    xs.len()
          |
          |print(f(true), f(false))
          |""".stripMargin

      run(src) shouldBe "2 1\n"
    }

    // The other order, which is the one that needs the *then* branch read a second time: it is
    // analyzed first, fails for want of an expectation, and is re-read with what the `else` settled.
    "the then branch, which is read twice" in {
      val src =
        """import sysl.buf.*
          |
          |mk() -> Buf[int]
          |    var b: Buf[int] = buf()
          |    b.push(7)
          |    b
          |
          |f(c: bool) -> usize
          |    var xs = if c then buf() else mk()
          |    xs.push(1)
          |    xs.len()
          |
          |print(f(true), f(false))
          |""".stripMargin

      run(src) shouldBe "1 2\n"
    }

    // Nothing about it is `buf`'s: the rule is asked of the declaration, so an ordinary nullary
    // generic written in the program behaves the same way.
    "a nullary generic written in the program, not only a library one" in {
      val src =
        """empty[T]() -> Option[T] = None
          |
          |f(c: bool) -> Option[int]
          |    val b = if c then Some(3) else empty()
          |    b
          |
          |print(f(true).unwrap(), f(false).is_none())
          |""".stripMargin

      run(src) shouldBe "3 true\n"
    }

    "and with the generic branch written first" in {
      val src =
        """empty[T]() -> Option[T] = None
          |
          |f(c: bool) -> Option[int]
          |    val b = if c then empty() else Some(3)
          |    b
          |
          |print(f(false).unwrap(), f(true).is_none())
          |""".stripMargin

      run(src) shouldBe "3 true\n"
    }

    // An `elif` nests into the `else` branch, so the settling has to reach through one `if` into the
    // next — the branch that knows is in the middle here and the two that do not are on either side.
    "an elif chain settles from the branch in the middle" in {
      val src =
        """empty[T]() -> Option[T] = None
          |
          |f(n: int) -> Option[int]
          |    val b = if n == 0 then empty() elif n == 1 then Some(9) else empty()
          |    b
          |
          |print(f(1).unwrap(), f(0).is_none(), f(2).is_none())
          |""".stripMargin

      run(src) shouldBe "9 true true\n"
    }

    "a match arm is the same rule over as many alternatives as the form has" in {
      val src =
        """empty[T]() -> Option[T] = None
          |
          |pick(n: int) -> Option[int]
          |    val b = n match
          |        0 -> empty()
          |        _ -> Some(n)
          |    b
          |
          |print(pick(0).is_none(), pick(5).unwrap())
          |""".stripMargin

      run(src) shouldBe "true 5\n"
    }

    // The arm that knows may be written first as well as last, which is what the held-over reading
    // buys over analyzing the arms in source order and stopping at the first refusal.
    "and a match arm settled by an arm written before it" in {
      val src =
        """empty[T]() -> Option[T] = None
          |
          |pick(n: int) -> Option[int]
          |    val b = n match
          |        0 -> Some(n)
          |        _ -> empty()
          |    b
          |
          |print(pick(0).unwrap(), pick(5).is_none())
          |""".stripMargin

      run(src) shouldBe "0 true\n"
    }

    // What has NOT moved, and the rule is worth nothing without it: with neither branch able to
    // stand alone there is nothing to settle it, and the refusal is the one it always was.
    "two branches that neither can be read alone give the same diagnostic" in {
      err("""import sysl.buf.*
            |
            |f(c: bool)
            |    val xs = if c then buf() else buf()
            |    print(xs.len())
            |""".stripMargin) should include("cannot infer the type argument")
    }

    "and so do two arms" in {
      err("""empty[T]() -> Option[T] = None
            |
            |f(n: int)
            |    val b = n match
            |        0 -> empty()
            |        _ -> empty()
            |    print(b.is_none())
            |""".stripMargin) should include("cannot infer the type argument")
    }

    // A branch read speculatively and abandoned must leave nothing of its own behind, its **scope**
    // included: the reading that failed declared `inner`, and the line after the `if` may not see
    // it. Without the block closing its scope on the way out, the abandoned scope stays on the
    // stack and `print(inner)` resolves against a binding of a branch that was thrown away — which
    // compiles, so the seam is that this program is refused at all.
    "a branch tried and abandoned closes its scope on the way out" in {
      err("""import sysl.buf.*
            |
            |mk() -> Buf[int]
            |    var b: Buf[int] = buf()
            |    b.push(7)
            |    b
            |
            |f(c: bool) -> usize
            |    var xs =
            |        if c then
            |            mk()
            |        else
            |            val inner = 1
            |            buf()
            |    print(inner)
            |    xs.len()
            |
            |print(f(true))
            |""".stripMargin) should include("inner")
    }

    "and so does an arm" in {
      err("""empty[T]() -> Option[T] = None
            |
            |f(n: int) -> Option[int]
            |    val b = n match
            |        0 ->
            |            val inner = 1
            |            empty()
            |        _ -> Some(n)
            |    print(inner)
            |    b
            |
            |print(f(0).is_none())
            |""".stripMargin) should include("inner")
    }

    // Two branches that each know what they are still have to agree, and the complaint is still the
    // one about the pair rather than one about a branch read at the other's type.
    "two branches that each know their type still disagree in the same words" in {
      err("""import sysl.buf.*
            |
            |f(c: bool, n: usize)
            |    var b: Buf[int] = buf()
            |    val w = if c then b else n
            |    print(w)
            |""".stripMargin) should include("if branches have different types")
    }
  }
}
