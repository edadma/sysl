package sh.sysl

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** What a parse is allowed to *build*, which is the thing that decides whether a large file can be
 * compiled at all.
 *
 * The packrat memo hangs off the reader, so how many readers a parse makes is how many memo tables
 * it makes — and a reader per step rather than a reader per position is the difference between a
 * table proportional to the file and a table proportional to every path the grammar walked over it.
 * A 4,000-line program used to leave 129 MB live behind a 5 MB tree; the numbers here are the
 * structure that produced it, counted rather than timed, so the assertions say the same thing on a
 * loaded machine as on an idle one.
 */
class ParserMemoTests extends AnyFreeSpec with Matchers {

  /** `n` functions, each a body with parentheses and three operators — enough grammar per line for
   * the precedence ladder to be walked several times over every token.
   */
  private def program(n: Int): String =
    (1 to n).map(i => s"f$i(x: int) -> int\n    x + $i * 2 + (x - $i)").mkString("\n")

  private def readers(src: String): Int = {
    val p = new SyslParser(Source("gen.sysl", src))
    val r = p.parseProgram

    assert(r.successful, r.toString)
    p.readersBuilt
  }

  "a parse builds one reader per token position, not one per step" - {
    "so the count stays a small multiple of the file's lines" in {
      val lines = 2 * 500

      // Twelve tokens to a line is what this shape scans to; twenty-five leaves room for the
      // grammar to grow without the bound becoming a tripwire, and is still two orders of
      // magnitude under a reader per step.
      readers(program(500)) should be <= 25 * lines
    }

    "and grows linearly with the file, which a per-step reader does not" in {
      val small = readers(program(250))
      val large = readers(program(500))

      // Twice the program, twice the positions — the only slack is the file's own beginning.
      (large - 2 * small).abs should be <= 4
    }

    "and a file the grammar cannot read is bounded the same way" in {
      // A line that will not parse is where a grammar backtracks hardest, so this is the case a
      // reader per step costs the most on.
      val src   = program(250) + "\nbroken(x: int) -> int\n    x + ) 1\n"
      val p     = new SyslParser(Source("broken.sysl", src), recovering = true)
      val lines = src.linesIterator.size

      p.parseProgram
      p.readersBuilt should be <= 25 * lines
    }
  }
}
