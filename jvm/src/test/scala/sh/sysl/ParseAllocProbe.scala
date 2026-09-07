package sh.sysl

import java.lang.management.ManagementFactory
import com.sun.management.ThreadMXBean

/** What a parse *allocates*, per line of source — the instrument the parser's cost is measured with.
 *
 * Live heap says nothing here: a parse finishes holding a few megabytes and allocates gigabytes on
 * the way, and it is the churn rather than the retention that decides whether a large program can
 * be compiled at all. So this reads
 * `com.sun.management.ThreadMXBean.getCurrentThreadAllocatedBytes` around one parse and divides by
 * the file's lines.
 */
object ParseAllocProbe {

  private val bean = ManagementFactory.getThreadMXBean.asInstanceOf[ThreadMXBean]

  def allocated: Long = bean.getCurrentThreadAllocatedBytes

  /** `n` structs, each with a function over it, a generic function called at one type, and a body
   * whose arguments have to be tried at their positions. The shape the analyzer's own measurements
   * use, so the two are comparable.
   */
  def program(n: Int): String = {
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

  /** Bytes this thread allocated parsing `src`, beside its line count. */
  def parseAlloc(name: String, src: String): (Long, Int) = {
    val before = allocated
    val p      = new SyslParser(Source(name, src))
    val r      = p.parseProgram

    val after = allocated

    if !r.successful then sys.error(s"$name: $r")

    (after - before, src.linesIterator.size)
  }

  /** Bytes this thread allocated *scanning* `src` — the lexer's share, on its own. */
  def lexAlloc(src: String): (Long, Int) = {
    val lex    = new SyslLexical
    val before = allocated
    val ts     = lex.scanPositioned(src)
    val after  = allocated

    if ts.isEmpty then sys.error("no tokens")

    (after - before, src.linesIterator.size)
  }

  def main(args: Array[String]): Unit = {
    val mode = if args.isEmpty then "gen" else args(0)
    val reps = if args.length > 2 then args(2).toInt else 1

    mode match {
      case "gen" =>
        val n   = if args.length > 1 then args(1).toInt else 1000
        val src = program(n)

        for r <- 1 to reps do
          val (bytes, lines) = parseAlloc("gen.sysl", src)
          println(f"gen n=$n lines=$lines rep=$r alloc=${bytes / 1048576.0}%.1f MB perLine=${bytes.toDouble / lines}%.0f B")

      case "lex" =>
        val n   = if args.length > 1 then args(1).toInt else 500
        val src = program(n)

        for r <- 1 to reps do
          val (bytes, lines) = lexAlloc(src)
          println(f"lex n=$n lines=$lines rep=$r alloc=${bytes / 1048576.0}%.1f MB perLine=${bytes.toDouble / lines}%.0f B")

      case "lib" =>
        val dir   = args(1)
        val files = listSysl(new java.io.File(dir))

        for r <- 1 to reps do
          var total = 0L
          var lines = 0

          for f <- files do
            val text           = scala.io.Source.fromFile(f).mkString
            val (bytes, count) = parseAlloc(f.getName, text)

            total += bytes
            lines += count

          println(f"lib files=${files.size} lines=$lines rep=$r alloc=${total / 1048576.0}%.1f MB perLine=${total.toDouble / lines}%.0f B")
    }
  }

  private def listSysl(dir: java.io.File): List[java.io.File] =
    if dir.isDirectory then dir.listFiles.toList.sortBy(_.getName).flatMap(listSysl)
    else if dir.getName.endsWith(".sysl") then List(dir)
    else Nil
}
