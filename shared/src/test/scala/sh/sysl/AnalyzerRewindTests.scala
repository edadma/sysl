package sh.sysl

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** What a speculative walk is allowed to *move*, which is the thing that decides whether a large
 * program can be analyzed at all.
 *
 * The analyzer asks speculative questions everywhere — whether a receiver has a member of some
 * name, which overload the arguments fit, whether a bare name resolves — and each one is a real
 * walk whose registrations have to be dropped again. Taking a copy of the instantiation tables
 * before each question and putting it back after is correct, and it costs the size of the tables
 * once per question: a program's instantiations multiplied by its call sites, which is quadratic in
 * the source. Measured on the generated program below at 14,005 lines, that was 239 million table
 * entries copied and 14.0 GB allocated by analysis alone, against 861 MB after.
 *
 * So the numbers here are counts rather than times, exactly as `ParserMemoTests`' are: they say the
 * same thing on a loaded machine as on an idle one.
 */
class AnalyzerRewindTests extends AnyFreeSpec with Matchers {

  /** `n` structs, each with a function over it, a generic function called at one type, and a body
   * whose arguments have to be tried at their positions — which is what makes the walk speculate.
   */
  private def program(n: Int): String = {
    val b = new StringBuilder

    b ++= "module main\n\n"

    for i <- 1 to n do
      b ++= s"struct P$i\n    a$i: int\n    b$i: string\n\n"
      b ++= s"sum$i(p: P$i) -> int = p.a$i + p.a$i * 2 - ($i + 1)\n\n"
      b ++= s"pick$i[T](x: T, y: T, c: bool) -> T = if c then x else y\n\n"
      b ++= s"job$i(n: int) -> int\n"
      b ++= s"    var p = P$i(n + $i, \"p$i\")\n"
      b ++= s"    var m = pick$i(sum$i(p), n * 2, n > $i)\n"
      b ++= s"    return m + n * 3 - (n / 2) + $i\n\n"

    b ++= "main()\n    var t = 0\n"
    for i <- 1 to n do b ++= s"    t = t + job$i($i)\n"
    b ++= "    print(t)\n"
    b.toString
  }

  /** The undo entries one whole analysis of that program had to write, beside its line count. */
  private def rewinding(n: Int): (Long, Int) = {
    val src    = program(n)
    val target = Target.default
    val unit = SyslParser.checked(Source("gen.sysl", src), target) match
      case Right(p) => p
      case Left(e)  => fail(Diagnostic.report(e))

    val analyzer = new Analyzer(List(unit), Set.empty, Stdlib.fromSource(target), target,
                                Capability.core.toSet, Packages.none, Some(Set("main")), true)

    analyzer.analyze()
    analyzer.errors shouldBe empty

    (analyzer.undoEntries, src.linesIterator.size)
  }

  "a speculative walk is taken back by what it wrote, not by what the tables hold" - {
    "so twice the program costs about twice the rewinding, rather than four times" in {
      val (small, _) = rewinding(60)
      val (large, _) = rewinding(120)

      // Linear would be 2, and the measured answer is 1.07 — most of what a rewind has to remember
      // on this shape is the standard module's, which both programs pay once. A table copy per
      // question grew by 7.8 over the same step, so 3 separates the two by a wide margin while
      // leaving the grammar room to grow.
      large.toDouble / small should be < 3.0
    }

    "and the cost per line of source stays small and flat" in {
      val (entries, lines) = rewinding(120)

      // Measured at one per line. The table copy it replaced moved 8,762 entries per line over the
      // same shape, so 25 leaves room for the analyzer to speculate more without letting the cost
      // back to anything like a copy of the tables.
      entries / lines should be < 25L
    }
  }

  "rewinding restores a table exactly" - {
    "an insertion made inside a region is gone again" in {
      val j = new Journal
      val m = new JournaledMap[String, Int](j)

      m("a") = 1
      val mark = j.enter()
      m("b") = 2
      j.rewind(mark)
      j.leave()

      m.toList shouldBe List("a" -> 1)
    }

    "an overwrite is put back, and does not move the key" in {
      val j = new Journal
      val m = new JournaledMap[String, Int](j)

      m("a") = 1
      m("b") = 2
      val mark = j.enter()
      m("a") = 99
      m("c") = 3
      j.rewind(mark)
      j.leave()

      m.toList shouldBe List("a" -> 1, "b" -> 2)
    }

    "a removal is put back" in {
      val j = new Journal
      val m = new JournaledMap[String, Int](j)

      m("a") = 1
      m("b") = 2
      val mark = j.enter()
      m.remove("a")
      j.rewind(mark)
      j.leave()

      m("a") shouldBe 1
    }

    "and a set is restored the same way" in {
      val j = new Journal
      val s = new JournaledSet[String](j)

      s += "a"
      val mark = j.enter()
      s += "b"
      s -= "a"
      j.rewind(mark)
      j.leave()

      s.toList shouldBe List("a")
    }

    "however the write was spelled" in {
      // Each of these is written directly in the linked collections rather than in terms of `put`
      // and `add`, so each is its own way past the journal. `m(k) = v` is how the analyzer writes
      // to nearly every one of these tables, and a rewind that missed it left the maps holding what
      // the sets had given up — a vtable whose method was no longer reached, and an emitted module
      // naming a function nothing defined.
      val j = new Journal
      val m = new JournaledMap[String, Int](j)
      val s = new JournaledSet[String](j)

      m("keep") = 0
      s += "keep"

      val mark = j.enter()

      m("update") = 1
      m += ("addOne" -> 2)
      m.getOrElseUpdate("orElse", 3)
      m("keep") = 99
      m -= "keep"
      s += "addOne"
      s.add("add")
      s -= "keep"

      j.rewind(mark)
      j.leave()

      m.toList shouldBe List("keep" -> 0)
      s.toList shouldBe List("keep")
    }

    "nested regions are taken back one at a time" in {
      val j = new Journal
      val m = new JournaledMap[String, Int](j)

      val outer = j.enter()
      m("a") = 1
      val inner = j.enter()
      m("b") = 2
      j.rewind(inner)
      j.leave()

      m.toList shouldBe List("a" -> 1)

      j.rewind(outer)
      j.leave()

      m shouldBe empty
    }
  }

  "a write with nobody to answer to is not remembered" - {
    "so the log does not grow over a build that is not speculating" in {
      val j = new Journal
      val m = new JournaledMap[String, Int](j)

      for i <- 1 to 1000 do m(s"k$i") = i

      j.recorded shouldBe 0L
    }

    "and what a region does remember is its own writes, not the table's size" in {
      val j = new Journal
      val m = new JournaledMap[String, Int](j)

      for i <- 1 to 1000 do m(s"k$i") = i

      val mark = j.enter()
      m("one more") = 1
      j.rewind(mark)
      j.leave()

      j.recorded shouldBe 1L
    }
  }
}
