package sh.sysl

import io.github.edadma.cross_platform.*

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** `sysl run` keeps what it built, so running the same program twice costs the second one nothing
 * (card `0309`, `RunCache`).
 *
 * **What a cache has to be right about is the MISS, not the hit.** A hit that should have been a
 * miss is a stale binary — a program that runs the code it had before the edit, silently — which is
 * worse than any amount of slowness. So most of what is below is about the key changing: every input
 * that reaches the bytes is perturbed in turn and the entry count is what says whether the key
 * noticed.
 *
 * The cache directory is this suite's own, which is what makes counting entries meaningful at all.
 */
class RunCacheTests extends AnyFreeSpec with Matchers {

  private def withCache[T](body: String => T): T = {
    val cache = createTempDirectory("sysl-runcache-")

    Fetch.usingCache(cache)(RunCache.usingCache(cache)(body(cache)))
  }

  /** The driver, run against a cache of the test's own. `Fetch.usingCache` moves the package cache
   * and `RunCache.usingCache` moves this one, so nothing here reaches the developer's.
   */
  private def ran(cfg: Config): String = {
    val out    = new java.io.ByteArrayOutputStream
    val notes  = new java.io.ByteArrayOutputStream
    val status = Console.withOut(out)(Console.withErr(notes)(sh.sysl.execute(cfg)))

    if status != 0 then fail(s"the driver exited with $status:\n${out.toString}${notes.toString}")

    out.toString
  }

  /** A report with its durations removed, so that two of them can be compared. */
  private def untimed(report: String): String = report.replaceAll("[0-9]+ms", "<ms>")

  private def entries(cache: String): Int =
    if isDirectory(s"$cache/sysl/run") then listFiles(s"$cache/sysl/run").length else 0

  private def program(text: String): String = {
    val root = createTempDirectory("sysl-run-")

    writeFile(s"$root/main.sysl", text)
    root
  }

  "the same program run twice is built once" in withCache { cache =>
    {
      val root = program("""print(21 * 2)""")

      ran(Config(command = "run", file = root)) shouldBe "42\n"
      entries(cache) shouldBe 1

      ran(Config(command = "run", file = root)) shouldBe "42\n"
      entries(cache) shouldBe 1
    }
  }

  "and the arguments it is given are not part of the key, which is the point" in withCache { cache =>
    {
      val root = program("""main(args: []string)
                           |    print(args[1])
                           |""".stripMargin)

      ran(Config(command = "run", file = root, programArgs = List("a"))) shouldBe "a\n"
      ran(Config(command = "run", file = root, programArgs = List("b"))) shouldBe "b\n"

      entries(cache) shouldBe 1
    }
  }

  "an edit is a different program" in withCache { cache =>
    {
      val root = program("""print(21 * 2)""")

      ran(Config(command = "run", file = root)) shouldBe "42\n"

      writeFile(s"$root/main.sysl", """print(21 * 3)""")

      ran(Config(command = "run", file = root)) shouldBe "63\n"
      entries(cache) shouldBe 2
    }
  }

  /** **A comment is an edit.** The key is over the file's text rather than over anything the parser
   * decided, which is the conservative direction: a key that tried to see through a comment would be
   * a key that had to be right about what a comment is.
   */
  "including one the program's behaviour does not depend on" in withCache { cache =>
    {
      val root = program("""print(21 * 2)""")

      ran(Config(command = "run", file = root)) shouldBe "42\n"

      writeFile(s"$root/main.sysl", "// a note\nprint(21 * 2)")

      ran(Config(command = "run", file = root)) shouldBe "42\n"
      entries(cache) shouldBe 2
    }
  }

  "and so is a second file beside it, which the first one's text says nothing about" in withCache { cache =>
    {
      val root = program("""import m.*
                           |
                           |print(double(21))
                           |""".stripMargin)

      createDirectories(s"$root/m")
      writeFile(s"$root/m/m.sysl", "module m\n\ndouble(n: int) -> int = n * 2\n")

      ran(Config(command = "run", file = root)) shouldBe "42\n"

      writeFile(s"$root/m/m.sysl", "module m\n\ndouble(n: int) -> int = n * 3\n")

      ran(Config(command = "run", file = root)) shouldBe "63\n"
      entries(cache) shouldBe 2
    }
  }

  "the optimization level changes what is emitted, so it changes the key" in withCache { cache =>
    {
      val root = program("""print(21 * 2)""")

      ran(Config(command = "run", file = root)) shouldBe "42\n"
      ran(Config(command = "run", file = root, optimize = "2")) shouldBe "42\n"

      entries(cache) shouldBe 2
    }
  }

  /** The escape hatch, which is for working **on the compiler** — where the version in the key
   * stands still while the bytes it produces do not.
   */
  "'SYSL_NO_CACHE' builds every time and keeps nothing" in withCache { cache =>
    {
      RunCache.disabledFor {
        val root = program("""print(21 * 2)""")

        ran(Config(command = "run", file = root)) shouldBe "42\n"
        ran(Config(command = "run", file = root)) shouldBe "42\n"

        entries(cache) shouldBe 0
      }
    }
  }

  /** `sysl test` is the same shape and the case the card says the cost is felt in most often — a
   * suite that recompiles on every run. It needs **two** things from the cache, because the
   * executable does not carry what to call in it, so the sidecar is written beside it.
   */
  "a test suite run twice is built once, and the report is the same either way" in withCache { cache =>
    {
      val root = program("""@test("two doubled is four")
                           |doubling() =
                           |    assert(2 * 2 == 4)
                           |
                           |@test
                           |adding() =
                           |    assert(1 + 1 == 2)
                           |""".stripMargin)

      val first = ran(Config(command = "test", file = root))

      entries(cache) shouldBe 2

      val second = ran(Config(command = "test", file = root))

      // Compared with the **timings taken out**, which are the one part of a report that is a clock
      // rather than a fact: two runs of one suite differ by a millisecond and are the same report.
      untimed(second) shouldBe untimed(first)
      first should include("two doubled is four")
      first should include("2 passed")
      entries(cache) shouldBe 2
    }
  }

  /** The filter picks from the list rather than deciding what is compiled, so it is not in the key —
   * and a cached run has to apply it exactly as a fresh one does, which is why `rerun` is the tail
   * of `run` rather than a second implementation.
   */
  "and a filter is applied to a cached suite as it is to a fresh one" in withCache { cache =>
    {
      val root = program("""@test
                           |doubling() =
                           |    assert(2 * 2 == 4)
                           |
                           |@test
                           |adding() =
                           |    assert(1 + 1 == 2)
                           |""".stripMargin)

      ran(Config(command = "test", file = root)) should include("2 passed")

      val filtered = ran(Config(command = "test", file = root, filter = Some("doubling")))

      filtered should include("1 passed")
      filtered should not include "adding"
      entries(cache) shouldBe 2
    }
  }

  "a 'run' and a 'test' of one tree are two entries, since they are two builds" in withCache { cache =>
    {
      val root = program("""print(21 * 2)
                           |
                           |@test
                           |arithmetic() =
                           |    assert(1 + 1 == 2)
                           |""".stripMargin)

      ran(Config(command = "run", file = root)) shouldBe "42\n"
      ran(Config(command = "test", file = root)) should include("1 passed")

      entries(cache) shouldBe 3
    }
  }

  /** **The environment that reaches the toolchain is part of the key** (card `0415`).
   *
   * Everything else in the key is settled by the command line and the source tree; this was the half
   * that was not, and it was missing. The org file's own recipe for checking a binding is *set
   * `SYSL_EXTRA_CFLAGS="-fsanitize=address"`, re-run `sysl test .`* — over an unchanged tree that
   * replayed the **uninstrumented** binary the previous ordinary run had left in the slot, and
   * reported green having looked at nothing.
   *
   * **A sanitizer is the worst place a cache can be stale, because its whole output is an absence.**
   * A wrong number would have been noticed on sight; a clean run is what a clean run looks like.
   *
   * `-fno-omit-frame-pointer` stands in for the sanitizer here: it reaches every clang the build
   * drives exactly as `-fsanitize=address` does, and costs the suite no runtime to link.
   *
   * **`-g` was the obvious stand-in and it is the wrong one**, which is worth a sentence because the
   * failure looks like the fix being broken: on Darwin the driver runs `dsymutil` after the link, so
   * a `-g` build writes a `prog.dSYM` **beside the executable in the cache slot** and the entry count
   * comes back one too high. Anything that counts what a build left on disk wants a flag that leaves
   * one file.
   */
  "the extra clang flags" - {

    "are part of the key, so a run under a new value builds again" in withCache { cache =>
      {
        val root = program("""print(21 * 2)""")

        ran(Config(command = "run", file = root)) shouldBe "42\n"
        entries(cache) shouldBe 1

        Toolchain.usingEnvironment(Map("SYSL_EXTRA_CFLAGS" -> "-fno-omit-frame-pointer")) {
          ran(Config(command = "run", file = root)) shouldBe "42\n"
        }

        entries(cache) shouldBe 2
      }
    }

    // The key is over the flags' *value*, not over the fact that an environment was consulted — so
    // the second run of a sanitizer build is still free, which is what makes the fix affordable.
    "and the same value twice is still one build" in withCache { cache =>
      {
        val root = program("""print(21 * 2)""")

        Toolchain.usingEnvironment(Map("SYSL_EXTRA_CFLAGS" -> "-fno-omit-frame-pointer")) {
          ran(Config(command = "run", file = root)) shouldBe "42\n"
          ran(Config(command = "run", file = root)) shouldBe "42\n"
        }

        entries(cache) shouldBe 1
      }
    }

    // A variable that is not set contributes nothing, so adding this to the key invalidated no entry
    // anybody already had — the ordinary build's key is exactly what it was.
    "contribute nothing to the key when nothing is set" in {
      Toolchain.usingEnvironment(Map.empty)(Toolchain.buildEnvironment) shouldBe empty
    }

    "and are named with their value, so two settings cannot share an entry" in {
      Toolchain.usingEnvironment(Map("SYSL_EXTRA_CFLAGS" -> "-fno-omit-frame-pointer"))(Toolchain.buildEnvironment)
        .shouldBe(List("SYSL_EXTRA_CFLAGS=-fno-omit-frame-pointer"))
    }

    /** **`SYSL_LIB` is deliberately not in it, and that is the more correct answer rather than an
     * omission.** It names where the library source *is*, and the key already carries that library's
     * fingerprint — its contents. Including the path would make two identical trees at two paths
     * miss each other's entry for no gain.
     */
    "and 'SYSL_LIB' is not among them, because the library's contents are already in the key" in {
      Toolchain.usingEnvironment(Map("SYSL_LIB" -> "/somewhere"))(Toolchain.buildEnvironment) shouldBe empty
    }

    // The four Android variables and WASI's name a cross toolchain — a different compiler and a
    // different sysroot, so different bytes from the same source.
    "and a cross toolchain's location is in the key too, since it decides which compiler answers" in {
      Toolchain.usingEnvironment(Map("ANDROID_HOME" -> "/sdk"))(Toolchain.buildEnvironment)
        .shouldBe(List("ANDROID_HOME=/sdk"))
      Toolchain.usingEnvironment(Map("WASI_SDK_PATH" -> "/wasi"))(Toolchain.buildEnvironment)
        .shouldBe(List("WASI_SDK_PATH=/wasi"))
    }
  }

  /** `build` writes a binary somebody named and is expected to have built it; `build-c` and
   * `build-lib` write artifacts for somebody else's toolchain. None of them is something a reader
   * would want quietly skipped, so none of them consults this.
   */
  "no other command keeps anything" in withCache { cache =>
    {
      val root = program("""print(21 * 2)""")
      val out  = createTempDirectory("sysl-out-")

      ran(Config(command = "build", file = root, output = Some(s"$out/app")))
      entries(cache) shouldBe 0
    }
  }

  "the test sidecar carries a suite back exactly as it went in" - {
    // The sidecar is how a cached suite reaches the runner: the binary alone says which tests exist,
    // and nothing in it says what to call them or which hooks bracket them. A field that did not
    // survive the round trip is a cached run behaving differently from a fresh one, which is the one
    // thing a cache may never do.
    val suite = List(
      TTest("m$plain", "a test with no hooks around it", false, None, "m.sysl", 3),
      TTest("m$traps", "one that should trap", true, Some("past the end"), "m.sysl", 9,
            THooks(setup = Some(THook(HookKind.Setup, "m$up", "m.sysl", 12)),
                   teardownAll = Some(THook(HookKind.TeardownAll, "m$halt", "m.sysl", 20)))),
    )

    "every field comes back" in {
      RunCache.decode(RunCache.encode(suite)) shouldBe Some(suite)
    }

    "a test with no hooks comes back with none" in {
      RunCache.decode(RunCache.encode(suite)).get.head.hooks shouldBe THooks()
    }

    // A sidecar written before the hooks existed has fewer fields per line, and the read is what has
    // to notice: `None` is a rebuild, and a rebuild is the designed answer.
    "a line of the wrong shape is no cache at all" in {
      RunCache.decode(List("m$plain", "a test", "false", "", "0", "m.sysl", "3").mkString("\u0000")) shouldBe None
    }
  }

}
