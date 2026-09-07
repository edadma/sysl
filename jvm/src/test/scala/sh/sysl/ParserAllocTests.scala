package sh.sysl

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** What a parse is allowed to *allocate*, per line of source.
 *
 * `ParserMemoTests` counts the readers a parse builds, which is what says the packrat memo is laid
 * out per position rather than per step. This asks the other half of the same question: what the
 * grammar and the scanner *churn* getting there, which is never retained and is nevertheless what
 * decides whether a large program can be compiled — Scala Native's collector grows the heap to the
 * allocation rate rather than collecting, so peak memory tracks total allocation.
 *
 * It is JVM-only because the instrument is: `ThreadMXBean.getCurrentThreadAllocatedBytes` is the
 * only honest reading of allocation, and live bytes say nothing here. The grammar is shared, so a
 * regression in it is a regression everywhere.
 *
 * **The bounds are generous on purpose.** They are tripwires against a rule that rebuilds itself at
 * every application — the shape of every defect this file has caught — rather than a budget to be
 * tuned. A `rep` handed an expression instead of a rule, a `def` where the scanner expected a fixed
 * parser, a refusal message assembled per declined token: each of those multiplies the number below
 * rather than nudging it.
 */
class ParserAllocTests extends AnyFreeSpec with Matchers {

  private def perLine(f: => (Long, Int)): Long = {
    // The first parse pays for class loading and for every `lazy val` rule in the grammar. What is
    // being measured is what a parse costs once the parser exists.
    f
    f

    val (bytes, lines) = f

    bytes / lines
  }

  "a parse allocates a bounded amount per line of source" - {
    "so a program the size of a library module does not churn a gigabyte" in {
      val src = ParseAllocProbe.program(500)

      // 7,005 lines. Measured at 203,500 B/line before the scanner was made to stop rebuilding
      // itself, and at 101,700 after.
      perLine(ParseAllocProbe.parseAlloc("gen.sysl", src)) should be < 150000L
    }

    "and the scan is a fraction of it rather than half of it" in {
      val src = ParseAllocProbe.program(500)

      // The same file, scanned and not parsed. This was 78,400 B/line with `token` and `delim`
      // rebuilt per token, and is 20,900 with the operator match reading characters instead.
      perLine(ParseAllocProbe.lexAlloc(src)) should be < 40000L
    }

    "and the cost per line does not grow with the file" in {
      val small = perLine(ParseAllocProbe.parseAlloc("small.sysl", ParseAllocProbe.program(125)))
      val large = perLine(ParseAllocProbe.parseAlloc("large.sysl", ParseAllocProbe.program(500)))

      // Four times the program, the same cost per line. A rule rebuilt per application would keep
      // this flat too; what it catches is a table or a copy whose size follows the file.
      large.toDouble / small should be < 1.5
    }
  }
}
