package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers

/** Shared helpers for the suites that exercise `@test` and `sysl test` (`reference/attributes.md § @test — a function with a caller nothing else has`).
 *
 * These drive the **same** three steps the CLI drives — compile as a test build, link, run one
 * process per test — through the same functions, so what a suite asserts about is what a user gets.
 * The one thing they leave out is the CLI's argument parsing, which `TestRunnerCliTests` covers by
 * calling `execute` with a `Config`.
 */
trait TestFrameworkSupport extends Matchers { this: Assertions =>

  /** Every test in a source, run, in the order the source declared them. */
  protected def outcomes(src: String, opts: TestRunner.Options = TestRunner.Options()): List[TestRunner.Outcome] =
    outcomesOf(List(Source("<input>", src)), opts)

  /** The same, over several files — which is how a claim about a **module** is asserted, since a
   * module is what a file's header says it contributes to. The hooks are scoped per module, so
   * every question about that scope needs at least two of them.
   */
  protected def outcomesOf(sources: List[Source],
                           opts: TestRunner.Options = TestRunner.Options()): List[TestRunner.Outcome] = {
    assume(Toolchain.clangAvailable, "clang not available")

    val (built, tests) = Compiler.compileTests(sources, Nil) match {
      case Right(result) => result
      case Left(e)       => fail(e)
    }

    val exe = createTempFile("sysl-test-", "")

    try
      Toolchain.build(built.ir, exe, links = built.links) match {
        case Left(e)  => fail(e)
        case Right(_) => TestRunner.execute(exe, tests.filter(TestRunner.matches(_, opts.filter)), opts)
      }
    finally
      try deleteFile(exe)
      catch case _: Exception => ()
  }

  /** The verdict for each test, keyed by the name a report would show it under — which is what a
   * reader of a failing suite has in their hand, and what `@test("…")` changes.
   */
  protected def verdicts(src: String): Map[String, Option[String]] =
    outcomes(src).map(o => o.test.display -> o.detail).toMap

  /** The names of the tests a source declares, in declaration order. */
  protected def discovered(src: String): List[String] =
    Compiler.compileTests(List(Source("<input>", src)), Nil) match {
      case Right((_, tests)) => tests.map(_.display)
      case Left(e)           => fail(e)
    }

  /** The IR of a **test build**, for asserting on the dispatcher without running it. */
  protected def testIr(src: String): String =
    Compiler.compileTests(List(Source("<input>", src)), Nil) match {
      case Right((built, _)) => built.ir
      case Left(e)           => fail(e)
    }

  /** Asserts that every test in a source passed, and says which did not when one did.
   *
   * The message names the failures rather than the count, because a count sends its reader back to
   * run the thing again to find out which one — which is the whole of what this could have told them.
   */
  protected def allPass(src: String): Unit = {
    val ran = outcomes(src)

    ran.filterNot(_.passed) match {
      case Nil => ran should not be empty
      case bad => fail(bad.map(o => s"${o.test.display}: ${o.detail.getOrElse("")}").mkString("\n"))
    }
  }
}
