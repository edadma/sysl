package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `sysl test` — the runner, end to end (`getting-started/cli.md § test`).
 *
 * Everything here compiles a real program, links it and starts a real process per test, because
 * every claim the runner makes is about something only a process can do: a trap ends one, a status
 * comes back from one, and output crosses a pipe. A verdict asserted against the analyzer instead
 * would be asserting what the compiler *believes* about a test rather than what running it says.
 */
class TestRunnerTests extends AnyFreeSpec with CodegenSupport with TestFrameworkSupport {


  "the hooks a module writes around its tests" - {
    // The claim `@setup` exists for: it runs in the test's **own** process, so what it leaves in
    // module storage is what the test finds. Counting rather than flagging, so that a setup running
    // twice in one process fails as loudly as one that did not run at all.
    "'@setup' runs once in each test's process, and the test sees what it left" in {
      allPass("""module m
                |
                |var ran: int = 0
                |
                |@setup
                |s() =
                |    ran = ran + 1
                |
                |@test
                |one() =
                |    assert(ran == 1, "setup ran exactly once before this test")
                |
                |@test
                |two() =
                |    assert(ran == 1, "and once before this one, in a process of its own")
                |""".stripMargin)
    }

    "'@teardown' runs in the test's process where the test returned" in {
      allPass("""module m
                |
                |var ran: int = 0
                |
                |@setup
                |s() =
                |    ran = 1
                |
                |@test
                |t() =
                |    ran = 2
                |
                |@teardown
                |d() =
                |    assert(ran == 2, "teardown sees what the test left")
                |""".stripMargin)
    }

    // The other half of the same rule, and the one a reader has to be told: a trap takes the process
    // with it, so there is nothing left to run a teardown in. It runs in a process of its own, which
    // sees module storage as the initializers left it — so what it can release there is what
    // outlives a process.
    "'@teardown' runs in a process of its own where the test did not return" in {
      allPass("""module m
                |
                |var ran: int = 0
                |
                |@setup
                |s() =
                |    ran = 1
                |
                |@test(should_trap)
                |t() =
                |    assert(ran == 1, "setup ran")
                |    assert(false, "and now the process ends")
                |
                |@teardown
                |d() =
                |    assert(ran == 0, "a fresh process sees the initializers and nothing else")
                |""".stripMargin)
    }

    "a '@setup' that does not return fails the tests it guards, and names itself" in {
      val ran = outcomes("""@setup
                           |s() =
                           |    assert(false, "the setup broke")
                           |
                           |@test
                           |t() =
                           |    print("never reached")
                           |""".stripMargin)

      ran.map(_.test.display) shouldBe List("t")
      ran.head.detail.get should include("'@setup'")
      ran.head.detail.get should include("s")
      ran.head.detail.get should include("so this test did not run")
    }

    // A setup fault and a test fault leave the same exit status, since both end the same process.
    // Telling them apart is the whole of what the dispatcher's marks buy, and this is the pair that
    // says the reading is right rather than lucky.
    "a test that fails under a '@setup' that worked is still reported as the test failing" in {
      val ran = outcomes("""module m
                           |
                           |var ran: int = 0
                           |
                           |@setup
                           |s() =
                           |    ran = 1
                           |
                           |@test
                           |t() =
                           |    assert(ran == 2, "the test's own check")
                           |""".stripMargin)

      ran.head.detail.get should startWith("did not return")
    }

    "a '@teardown' that does not return is a failure counted against the test" in {
      val ran = outcomes("""@test
                           |t() =
                           |    print("the test itself was fine")
                           |
                           |@teardown
                           |d() =
                           |    assert(false, "the teardown broke")
                           |""".stripMargin)

      ran.head.passed shouldBe false
      ran.head.detail.get should include("'@teardown'")
      ran.head.detail.get should include("after the test returned")
    }

    "a '@teardown' in its own process is a failure counted the same way" in {
      val ran = outcomes("""@test(should_trap)
                           |t() =
                           |    assert(false, "the test was meant to trap")
                           |
                           |@teardown
                           |d() =
                           |    assert(false, "and the teardown broke")
                           |""".stripMargin)

      ran.head.passed shouldBe false
      ran.head.detail.get should include("'@teardown'")
    }

    // Three tests and one row for the hook is the whole of "once per module": run per test it would
    // have been three.
    "'@teardown_all' runs once for the module, whatever the tests did" in {
      val ran = outcomes("""@test
                           |one() = 0
                           |
                           |@test
                           |two() = 0
                           |
                           |@test
                           |three() = 0
                           |
                           |@teardown_all
                           |halt() =
                           |    assert(false, "the module's teardown broke")
                           |""".stripMargin)

      ran.count(_.passed) shouldBe 3
      ran.filterNot(_.passed).map(_.test.display) shouldBe List("halt")
      ran.last.detail.get should include("'@teardown_all'")
    }

    // A `_all` hook has no test to hang a failure on, and the tests it was going to guard never
    // started — reporting them would be a third verdict for something that is not one.
    "a '@setup_all' that does not return stops the module and gets a row of its own" in {
      val ran = outcomes("""@setup_all
                           |boot() =
                           |    assert(false, "the module never came up")
                           |
                           |@test
                           |one() = 0
                           |
                           |@test
                           |two() = 0
                           |""".stripMargin)

      ran.map(_.test.display) shouldBe List("boot")
      ran.head.passed shouldBe false
      ran.head.detail.get should include("'@setup_all'")
    }

    "'@teardown_all' still runs where '@setup_all' did not come back" in {
      val ran = outcomes("""@setup_all
                           |boot() =
                           |    assert(false, "up failed")
                           |
                           |@teardown_all
                           |halt() =
                           |    assert(false, "and down is asked all the same")
                           |
                           |@test
                           |t() = 0
                           |""".stripMargin)

      ran.map(_.test.display) shouldBe List("boot", "halt")
    }

    // An `_all` hook is a process of its own, so what it leaves in module storage is gone before the
    // first test starts. The docs say so plainly, and this is the program that says it back.
    "an '_all' hook shares nothing with a test through module storage" in {
      allPass("""module m
                |
                |var ran: int = 0
                |
                |@setup_all
                |boot() =
                |    ran = 1
                |
                |@test
                |t() =
                |    assert(ran == 0, "a test's process is not the one '@setup_all' ran in")
                |""".stripMargin)
    }

    // The marks are the runner's channel, and a reader must never meet one: a failure shows what the
    // run printed, and a control character in the middle of it is the framework leaking.
    "the phase marks never reach the output a failure shows" in {
      val ran = outcomes("""@setup
                           |s() =
                           |    print("from the setup")
                           |
                           |@teardown
                           |d() =
                           |    print("from the teardown")
                           |
                           |@test
                           |t() =
                           |    print("from the test")
                           |    assert(false, "and now it fails")
                           |""".stripMargin)

      ran.head.output should include("from the setup")
      ran.head.output should include("from the test")
      ran.head.output.exists(c => c == Tests.setupMark || c == Tests.testMark) shouldBe false
    }

    // The mark is written with `write`, which the library also binds as an `extern` — so a build
    // could end up with the symbol declared twice, and LLVM refuses that outright. The declaration
    // is therefore made from the shape of the program, and a build with nothing to say does not make
    // it at all: a module with no per-test hook pays not one instruction for the mechanism.
    "the mark's 'write' is declared once, and only where there is a mark to make" in {
      val plain = testIr("""@test
                           |t() = 0
                           |""".stripMargin)

      plain should not include "@write"

      val marked = testIr("""@setup
                            |s() = 0
                            |
                            |@test
                            |t() = 0
                            |""".stripMargin)

      marked.linesIterator.count(l => l.startsWith("declare") && l.contains("@write")) shouldBe 1
    }

    // An `_all` hook is a process of its own and reports by exiting, so it needs no mark and the
    // declaration stays absent.
    "an '_all' hook alone makes no mark and needs no 'write'" in {
      testIr("""@setup_all
               |boot() = 0
               |
               |@test
               |t() = 0
               |""".stripMargin) should not include "@write"
    }

    "a module with no hooks runs exactly the processes it always ran" in {
      allPass("""@test
                |t() =
                |    assert(1 + 1 == 2, "arithmetic")
                |""".stripMargin)
    }

    "each module's tests get their own module's hooks" in {
      val ran = outcomesOf(files(
        "a.sysl" -> """module a
                      |
                      |var ran: int = 0
                      |
                      |@setup
                      |s() =
                      |    ran = 1
                      |
                      |@test
                      |mine() =
                      |    assert(ran == 1, "a's setup ran for a's test")
                      |""".stripMargin,
        "b.sysl" -> """module b
                      |
                      |var ran: int = 0
                      |
                      |@test
                      |theirs() =
                      |    assert(ran == 0, "and nothing ran for b's")
                      |""".stripMargin))

      ran.filterNot(_.passed) shouldBe empty
      ran.map(_.test.display) should contain theSameElementsAs List("mine", "theirs")
    }
  }

  "the report counts the tests that were selected, not the rows it printed" - {
    // A hook row is a row and is not a test, so reading the header off the rows would say "running 2
    // tests" over one test and one hook.
    "a hook failure adds a row without adding to the count" in {
      val ran = outcomes("""@test
                           |t() = 0
                           |
                           |@teardown_all
                           |halt() =
                           |    assert(false, "broke")
                           |""".stripMargin)

      TestRunner.rendered(ran, 0, 1) should include("running 1 test\n")
      TestRunner.rendered(ran, 0, 1) should include("1 passed, 1 failed")
    }
  }

  "a test passes by returning and fails by not" - {
    "a test that returns passes" in {
      verdicts("""@test
                 |t() =
                 |    assert(1 + 1 == 2, "arithmetic")
                 |""".stripMargin) shouldBe Map("t" -> None)
    }

    "a failed assertion fails the test, and the status says so" in {
      verdicts("""@test
                 |t() =
                 |    assert(1 + 1 == 3, "arithmetic")
                 |""".stripMargin) shouldBe Map("t" -> Some("did not return — exit status 1"))
    }

    // The property the whole design rests on: nothing in the body knows it is in a test. A contract
    // clause is checked because it is a contract clause, and the trap it raises ends the process,
    // which is the only thing the runner ever looks at.
    "a broken contract fails the test, having been told nothing about tests" in {
      val ran = outcomes("""halve(n: int) -> int
                           |    require n % 2 == 0, "even"
                           |    n / 2
                           |
                           |@test
                           |t() =
                           |    print(halve(3))
                           |""".stripMargin)

      ran.head.passed shouldBe false
      ran.head.detail.get should startWith("did not return")
    }

    "a bounds violation fails the test the same way" in {
      val ran = outcomes("""@test
                           |t() =
                           |    val a = [1, 2, 3]
                           |    var i = 5
                           |    print(a[i])
                           |""".stripMargin)

      ran.head.passed shouldBe false
    }
  }

  "'should_trap' inverts the verdict" - {
    "a test that traps passes" in {
      verdicts("""@test(should_trap)
                 |t() =
                 |    assert(false, "this must fire")
                 |""".stripMargin) shouldBe Map("t" -> None)
    }

    // The failure that would otherwise go unnoticed: a check that stopped firing turns its test
    // green unless returning is itself the failure.
    "a test that returns fails, which is what makes the form worth having" in {
      verdicts("""@test(should_trap)
                 |t() =
                 |    assert(true, "this does not fire")
                 |""".stripMargin) shouldBe Map("t" -> Some("returned, and was expected to trap"))
    }

    "with a substring, the run must have printed it" in {
      verdicts("""@test(should_trap: "past the end")
                 |t() =
                 |    assert(false, "index past the end")
                 |""".stripMargin) shouldBe Map("t" -> None)
    }

    "a trap that printed something else is not the trap that was asked for" in {
      verdicts("""@test(should_trap: "past the end")
                 |t() =
                 |    assert(false, "some other complaint")
                 |""".stripMargin) shouldBe
        Map("t" -> Some("trapped, but printed nothing holding \"past the end\""))
    }

    // A trap prints nothing at all — `llvm.trap` raises a signal and the process is gone — so a
    // substring can only be asked of a failure that had something to say on its way out. Pinned
    // because it is the one case where the two failure shapes the language has behave differently.
    "a silent trap satisfies 'should_trap' but not a substring" in {
      val src = """halve(n: int) -> int
                  |    require n % 2 == 0, "even"
                  |    n / 2
                  |
                  |@test(should_trap)
                  |silent() =
                  |    print(halve(3))
                  |
                  |@test(should_trap: "even")
                  |wanting_words() =
                  |    print(halve(3))
                  |""".stripMargin

      verdicts(src)("silent") shouldBe None
      verdicts(src)("wanting_words") shouldBe Some("trapped, but printed nothing holding \"even\"")
    }
  }

  "each test runs in a process of its own" - {
    // The reason the runner does not call them in a loop: the first trap would end the run, and
    // every test after it would have no verdict rather than a failing one.
    "a test that traps does not stop the ones after it" in {
      verdicts("""@test
                 |first() =
                 |    assert(false, "down")
                 |
                 |@test
                 |second() =
                 |    assert(true, "up")
                 |
                 |@test
                 |third() =
                 |    assert(true, "up")
                 |""".stripMargin) shouldBe
        Map(
          "first"  -> Some("did not return — exit status 1"),
          "second" -> None,
          "third"  -> None,
        )
    }

    // State does not survive between tests, and could not: each one is a fresh process, with the
    // module's storage laid down again from its initializer.
    //
    // A `val` filled by a *call* is the case worth pinning. A constant one is written into the
    // object file and would read correctly however the entry point behaved; a computed one is filled
    // by code the entry point runs, which the dispatcher had to be given as well or every test would
    // read a zero.
    "a computed module-level val is filled before a test runs" in {
      verdicts("""twice(n: int) -> int = n * 2
                 |
                 |val start: int = twice(7)
                 |
                 |@test
                 |reads_it() =
                 |    assert(start == 14, "as computed")
                 |
                 |@test
                 |reads_it_again() =
                 |    assert(start == 14, "still as computed")
                 |""".stripMargin) shouldBe Map("reads_it" -> None, "reads_it_again" -> None)
    }
  }

  "a run can be narrowed" - {
    "a filter selects by the name a report shows" in {
      val src = """@test
                  |alpha() = 0
                  |
                  |@test
                  |beta() = 0
                  |""".stripMargin

      outcomes(src, TestRunner.Options(filter = Some("alph"))).map(_.test.display) shouldBe List("alpha")
    }

    "a filter selects by module too, which is how a module's tests are named at once" in {
      val tests = List(
        TTest("geom$area", "area", false, None, "geom/a.sysl", 1),
        TTest("text$trim", "trim", false, None, "text/b.sysl", 1),
      )

      tests.filter(TestRunner.matches(_, Some("geom"))).map(_.display) shouldBe List("area")
    }

    "a display name is what the filter sees, not the function's own" in {
      val src = """@test("the sum of an empty list is zero")
                  |sum_empty() = 0
                  |""".stripMargin

      outcomes(src, TestRunner.Options(filter = Some("empty list"))).map(_.test.display) shouldBe
        List("the sum of an empty list is zero")
    }

    "fail-fast stops at the first failure and reports what ran" in {
      val ran = outcomes("""@test
                           |first() =
                           |    assert(false, "down")
                           |
                           |@test
                           |second() =
                           |    assert(true, "up")
                           |""".stripMargin, TestRunner.Options(failFast = true))

      ran.map(_.test.display) shouldBe List("first")
    }

    "fail-fast runs everything when nothing fails" in {
      val ran = outcomes("""@test
                           |first() = 0
                           |
                           |@test
                           |second() = 0
                           |""".stripMargin, TestRunner.Options(failFast = true))

      ran.map(_.test.display) shouldBe List("first", "second")
    }
  }

  /** Two outcomes, built rather than run: the report is a pure function of them, and asserting on it
   * through a real compilation would make every one of these depend on a toolchain to say nothing
   * more.
   */
  private def ran: List[TestRunner.Outcome] = List(
    TestRunner.Outcome(TTest("a", "passes", false, None, "m.sysl", 3), None, "", 1),
    TestRunner.Outcome(TTest("b", "fails", false, None, "m.sysl", 7), Some("did not return — exit status 1"),
      "panic: nope\n", 2),
  )

  "the report says what happened" - {
    "it counts what ran and what failed" in {
      val text = TestRunner.rendered(ran, 0, ran.length)

      text should include("running 2 tests")
      text should include("1 passed, 1 failed")
    }

    "a failure carries the line the attribute is on" in {
      TestRunner.rendered(ran, 0, ran.length) should include("at m.sysl:7")
    }

    // Output from a passing test is what the test was doing; output from a failing one is evidence.
    "what a failing test printed is shown, and what a passing one printed is not" in {
      val text = TestRunner.rendered(
        ran :+ TestRunner.Outcome(TTest("c", "quiet", false, None, "m.sysl", 9), None, "unread\n", 1),
        0, ran.length + 1)

      text should include("> panic: nope")
      text should not include "unread"
    }

    "a filtered run says how many it did not run" in {
      TestRunner.rendered(ran, 5, ran.length) should include("running 2 tests of 7")
    }

    "tests are shown under the file they were written in, in source order" in {
      val mixed = List(
        TestRunner.Outcome(TTest("b", "second", false, None, "m.sysl", 20), None, "", 1),
        TestRunner.Outcome(TTest("a", "first", false, None, "m.sysl", 10), None, "", 1),
      )

      val lines = TestRunner.rendered(mixed, 0, mixed.length).linesIterator.filter(_.contains("ok")).toList

      lines.head should include("first")
      lines(1) should include("second")
    }
  }

  // A hook row uses `Modules.bare(h.func)` as its display, exactly what `hookOutcome` builds —
  // reused here so a widened "outcomes with a hook row" case is exercised without a real hook
  // failing under a real process.
  private def hookRow: TestRunner.Outcome =
    TestRunner.Outcome(TTest("m.halt", "halt", false, None, "m.sysl", 30),
      Some("the module's '@teardown_all', 'm.halt', did not return — exit status 1"), "torn down badly\n", 3)

  "streaming a run" - {
    // `stream` is what `execute` calls as a run goes; `rendered` is the whole report as one string
    // once a run is over. This is the guarantee the streaming path exists to keep: watching a run
    // live shows exactly what reading the finished report would have shown, just spread across more
    // calls, with a hook's own row exercised alongside an ordinary pass and an ordinary failure.
    "streams byte-for-byte the same text 'rendered' produces for the same outcomes" in {
      val outcomes = ran :+ hookRow
      val pieces   = List.newBuilder[String]

      TestRunner.stream(outcomes, 0, outcomes.length, pieces += _)

      pieces.result().mkString shouldBe TestRunner.rendered(outcomes, 0, outcomes.length)
    }

    "emits the header before any row, and the summary after the last" in {
      val outcomes = ran :+ hookRow
      val pieces   = List.newBuilder[String]

      TestRunner.stream(outcomes, 0, outcomes.length, pieces += _)

      val emitted = pieces.result()

      emitted.head should startWith("running")
      emitted.last should include("passed")
      emitted.last should include("failed")

      // Everything between the header and the summary is a file heading or a row — neither of
      // which starts with "running" or holds the closing tally, so the header and the summary are
      // found exactly once each, at the ends.
      emitted.count(_.startsWith("running")) shouldBe 1
      emitted.count(l => l.contains(" passed, ") && l.contains(" failed")) shouldBe 1
    }

    "an empty run still gets a header and a summary, in that order" in {
      val pieces = List.newBuilder[String]

      TestRunner.stream(Nil, 0, 0, pieces += _)

      val emitted = pieces.result()

      emitted should have length 2
      emitted.head should startWith("running 0 tests")
      emitted.last should include("0 passed, 0 failed")
    }
  }

  "a test is a member of its module like any other" - {
    // The claim this settles is about *order*: tests are dropped after the whole-program checks
    // have run, so a module's capability clause reaches them (`reference/modules.md § Capabilities
    // are a module property`). A test invisible to that check would let a `no alloc` module hold an
    // allocation that its own tests exercised every day.
    "a module's 'no alloc' clause reaches its tests" in {
      errIn(("m", "m.sysl", """module m
                              |@no_alloc
                              |
                              |add(a: int, b: int) -> int = a + b
                              |
                              |@test
                              |adding() =
                              |    val s = str(add(2, 2))
                              |    assert(s == "4", "adding")
                              |""".stripMargin)) should include("declared '@no_alloc'")
    }

    "a test that allocates nothing is fine in the same module" in {
      irIn(("m", "m.sysl", """module m
                             |@no_alloc
                             |
                             |add(a: int, b: int) -> int = a + b
                             |
                             |@test
                             |adding() =
                             |    assert(add(2, 2) == 4, "adding")
                             |""".stripMargin)) should not be empty
    }
  }

  "a test has one caller, and it is not the program" - {
    // Found by probing rather than by reasoning: a call compiled fine and failed at the *link*,
    // naming a symbol nothing in the source explained — because the ordinary build had dropped the
    // definition and kept the call.
    "calling a test is refused where the call is written" in {
      err("""@test
            |t() =
            |    assert(true, "up")
            |
            |t()
            |""".stripMargin) should include("'sysl test' calls and nothing else does")
    }

    "a test calling another test is the same refusal" in {
      err("""@test
            |helper() =
            |    assert(true, "up")
            |
            |@test
            |t() =
            |    helper()
            |""".stripMargin) should include("'sysl test' calls and nothing else does")
    }

    "work two tests share goes in an ordinary function, which both may call" in {
      allPass("""shared() -> int = 21
                |
                |@test
                |first() =
                |    assert(shared() == 21, "shared")
                |
                |@test
                |second() =
                |    assert(shared() * 2 == 42, "shared again")
                |""".stripMargin)
    }
  }

  "a test build is not the program" - {
    "the program's own statements do not run" in {
      // The statement would print if the entry point were the ordinary one. What runs instead is the
      // dispatcher, which calls one test and nothing else.
      outcomes("""print("the program ran")
                 |
                 |@test
                 |t() = 0
                 |""".stripMargin).head.output shouldBe ""
    }

    "a declared main does not run either" in {
      outcomes("""main()
                 |    print("main ran")
                 |
                 |@test
                 |t() = 0
                 |""".stripMargin).head.output shouldBe ""
    }

    "the dispatcher compares the name it was given against each test" in {
      val out = testIr("""@test
                         |t() = 0
                         |""".stripMargin)

      out should include("declare i32 @strcmp(ptr, ptr)")
      out should include regex "call i32 @strcmp"
      out should include("define i32 @main(i32 %argc, ptr %argv)")
    }

    "a name the binary has no test for is neither a pass nor a failure" in {
      // Status 2 rather than 0 or a trap: a runner and a binary that disagree is not a test result,
      // and reading it as one would turn a stale build into a green run.
      val out = testIr("""@test
                         |t() = 0
                         |""".stripMargin)

      mainOf(out) should include("ret i32 2")
    }
  }

  "an ordinary build has no tests in it" - {
    "a test function is not emitted" in {
      val out = ir("""double(n: int) -> int = n * 2
                     |
                     |@test
                     |doubling() =
                     |    assert(double(2) == 4, "doubling")
                     |
                     |print(double(3))
                     |""".stripMargin)

      out should not include "doubling"
    }

    "nor is a helper only a test calls" in {
      val out = ir("""only_a_test_calls_this() -> int = 42
                     |
                     |@test
                     |t() =
                     |    assert(only_a_test_calls_this() == 42, "helper")
                     |
                     |print("hello")
                     |""".stripMargin)

      out should not include "only_a_test_calls_this"
    }

    // The other half of the same rule, and the one that would be missed: a helper the *program* also
    // calls is not a test's to remove.
    "a helper the program also calls stays" in {
      val out = ir("""shared() -> int = 42
                     |
                     |@test
                     |t() =
                     |    assert(shared() == 42, "helper")
                     |
                     |print(shared())
                     |""".stripMargin)

      out should include("shared")
    }

    // A library is lowered without pruning — it has no `main` to prune from, and every public
    // declaration is a potential entry — so the rule that keeps a program's tests out of its output
    // does not reach it. Dropping them is a separate act, and this is what says it happens.
    "a library's tests are not in the library" in {
      val lib = SyslParser.parse(Source("lib.sysl", """module demo
                                                      |
                                                      |double(n: int) -> int = n * 2
                                                      |
                                                      |@test
                                                      |doubling() =
                                                      |    assert(double(2) == 4, "doubling")
                                                      |""".stripMargin, List("demo"))) match {
        case Right(p) => p
        case Left(e)  => fail(e)
      }

      Compiler.compileLibrary(List(lib)) match {
        case Right((out, symbols)) =>
          out should include("double")
          out should not include "doubling"
          symbols.filter(_.contains("doubling")) shouldBe empty
        case Left(e) => fail(e)
      }
    }

    "the program still runs, and runs what it always did" in {
      val src = """double(n: int) -> int = n * 2
                  |
                  |@test
                  |doubling() =
                  |    assert(double(2) == 4, "doubling")
                  |
                  |print(double(21))
                  |""".stripMargin

      Toolchain.compileAndRun(src) match {
        case Right((0, out)) => out shouldBe "42\n"
        case Right((c, out)) => fail(s"exited with $c: $out")
        case Left(e)         => fail(e)
      }
    }
  }
}
