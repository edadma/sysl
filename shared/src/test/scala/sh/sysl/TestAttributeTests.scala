package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `@test` as the grammar and the analyzer see it (`reference/attributes.md § @test — a function with a caller nothing else has`).
 *
 * The attribute is the language's first, so half of what is pinned here is about the *mechanism*
 * rather than about tests: that `#` opens one, that the word after it is checked rather than
 * swallowed, that the attribute and its declaration are one statement across the newline between
 * them. The other half is what a `@test` function may be, which is everything needed for the runner
 * to call it with nothing and read the answer off whether it returned.
 */
class TestAttributeTests extends AnyFreeSpec with CodegenSupport with RunSupport with TestFrameworkSupport {

  private def parsed(src: String): List[Stmt] =
    SyslParser.parse(Source("<input>", src)) match {
      case Right(p) => p.body
      case Left(e)  => fail(e)
    }

  private def attrOf(src: String): TestAttr =
    parsed(src).collectFirst { case f: FuncDecl if f.test.isDefined => f.test.get } match {
      case Some(a) => a
      case None    => fail(s"no '@test' function was parsed from:\n$src")
    }

  "the attribute parses in each form it offers" - {
    "a bare '@test' says only that the function is one" in {
      attrOf("""@test
               |t() = 0
               |""".stripMargin) shouldBe TestAttr(None, false, None)
    }

    "a string is the name a report shows" in {
      attrOf("""@test("a sentence about what holds")
               |t() = 0
               |""".stripMargin) shouldBe TestAttr(Some("a sentence about what holds"), false, None)
    }

    "'should_trap' inverts the verdict" in {
      attrOf("""@test(should_trap)
               |t() = 0
               |""".stripMargin) shouldBe TestAttr(None, true, None)
    }

    "'should_trap' may name what the run must have printed" in {
      attrOf("""@test(should_trap: "past the end")
               |t() = 0
               |""".stripMargin) shouldBe TestAttr(None, true, Some("past the end"))
    }

    // The two are independent — a test may want a sentence for its name *and* be about a check that
    // fires — so they compose rather than being alternatives, which is the one combination a grammar
    // written as a flat choice would have left out.
    "a name and an expectation may be written together" in {
      attrOf("""@test("an index past the end is refused", should_trap: "past the end")
               |t() = 0
               |""".stripMargin) shouldBe
        TestAttr(Some("an index past the end is refused"), true, Some("past the end"))
    }

    "a test may be private, which is what tests of a module's own internals need" in {
      val decls = parsed("""@test
                           |private t() = 0
                           |""".stripMargin)

      decls.collectFirst { case f: FuncDecl => (f.vis, f.test.isDefined) } shouldBe Some((Visibility.File, true))
    }
  }

  "the attribute is one statement with the declaration below it" - {
    // The separator between statements is a newline, so without the rule joining them the attribute
    // is a statement of its own — and the failure is about whatever the *next* line is, which says
    // nothing about the line that was wrong.
    "an ordinary declaration after a test is not swept into it" in {
      val decls = parsed("""@test
                           |t() = 0
                           |
                           |plain() = 1
                           |""".stripMargin)

      decls.collect { case f: FuncDecl => (f.name, f.test.isDefined) } shouldBe
        List(("t", true), ("plain", false))
    }

    "two tests in a row each keep their own attribute" in {
      val decls = parsed("""@test("first")
                           |a() = 0
                           |@test("second")
                           |b() = 0
                           |""".stripMargin)

      decls.collect { case f: FuncDecl => f.test.flatMap(_.display) } shouldBe
        List(Some("first"), Some("second"))
    }

    "blank lines between the attribute and the function do not separate them" in {
      attrOf("""@test("still attached")
               |
               |
               |t() = 0
               |""".stripMargin).display shouldBe Some("still attached")
    }
  }

  "an attribute the language does not have is refused by name" - {
    // The point of naming them: a mechanism with a closed set of members should say which ones it
    // has, rather than reporting the grammar's own confusion about a token it could not place. The
    // list grows as the set does — `@pure` and `@ghost` joined it with `17` — so what is asserted is
    // that every member is named, one at a time, rather than one sentence that goes stale each time
    // an annotation is added.
    // `@packed` stood here until it became one of the annotations the language has, which is the
    // hazard of writing this test against a word somebody might one day implement. `@inline` is
    // chosen for being a thing other languages have and this one deliberately does not.
    "an unknown word after '@' says what the annotations are" in {
      val message = err("""@inline
                          |t() = 0
                          |""".stripMargin)

      message should include("'inline' is not an annotation a declaration takes")

      for known <- List("@test", "@tailrec", "@pure", "@ghost", "@packed", "@align") do
        message should include(known)

      // The hooks joined the set with the runner's own scaffolding, and are named for the reason
      // every other member is: a reader who wrote `@before` has the right idea and the wrong word.
      for known <- List("@setup", "@teardown", "@setup_all", "@teardown_all") do
        message should include(known)

      // The header's three are named too, and for the reason the four above are: a reader who wrote
      // `@link` over a function has the right annotation in the wrong place, and the message that
      // only listed what a *declaration* takes would leave them looking for a name that is not
      // missing. Asserted one at a time for the same reason — the sentence may change, the set is
      // what matters.
      for known <- List("@no_", "@requires", "@link") do message should include(known)
    }

    /** `#` where `@` was meant — the sigil someone arriving from Rust or C reaches for, and the one
     * spelling this rename made wrong in every program that had it. It is answered with the
     * difference between the two rather than with the annotation list, because the reader has the
     * right word and the wrong mark.
     */
    "the other sigil is answered with what each of the two is for" in {
      val out = err("""#test
                      |t() = 0
                      |""".stripMargin)

      out should include("an annotation is written '@test'")
      out should include("'#' opens a directive, which gates lines before the lexer sees them")
    }

    // …and the directive it names is untouched, which is the half of the split that had to keep
    // working: `#if` never reaches the grammar at all, so nothing above can be about one.
    "while a directive at the margin is read as a directive" in {
      run("""#if posix
            |t() -> int = 1
            |#else
            |t() -> int = 2
            |#endif
            |print(t())
            |""".stripMargin) shouldBe "1\n"
    }

    "a declaration that is not a function cannot be a test" in {
      err("""@test
            |struct Point
            |    x: int
            |""".stripMargin) should include("only a function")
    }

    "an extern has no body to run" in {
      err("""@test
            |extern side_effect()
            |""".stripMargin) should include("only a function")
    }
  }

  "what a '@test' function may be is what the runner can call" - {
    "a parameter has nowhere to come from" in {
      err("""@test
            |t(n: int) =
            |    print(n)
            |""".stripMargin) should include("takes no parameters")
    }

    // A variadic needs a named parameter in front of the tail (`reference/ffi.md § Variadic
    // functions`), so it is refused by the rule above and needs no case of its own — and a tail
    // with *no* named parameter never reaches this rule at all, being refused where every other
    // signature like it is.
    "a variadic tail is refused for the parameter it has to have" in {
      err("""@test
            |t(n: int, ...) =
            |    print(n)
            |""".stripMargin) should include("takes no parameters")
    }

    "a tail with nothing in front of it is refused where any other signature would be" in {
      err("""@test
            |t(...) = 0
            |""".stripMargin) should include("at least one named parameter")
    }

    "a generic has no compiled form until a caller fixes its arguments" in {
      err("""@test
            |t[T]() = 0
            |""".stripMargin) should include("no type parameters")
    }

    "a result is a value nothing is going to read" in {
      err("""@test
            |t() -> int = 3
            |""".stripMargin) should include("returns nothing")
    }

    // `-> unit` and no result at all are the same signature written two ways (`reference/declarations.md § Functions`), so a test
    // that says it out loud is as good as one that does not — and a rule checking the *syntax*
    // rather than the resolved type would have refused this one.
    "an explicit '-> unit' is the same as writing none" in {
      discovered("""@test
                   |t() -> unit = 0
                   |""".stripMargin) shouldBe List("t")
    }
  }

  "a test is reported under the name it was given" - {
    "with no string, that is the function's own bare name" in {
      discovered("""@test
                   |adds_two() = 0
                   |""".stripMargin) shouldBe List("adds_two")
    }

    "a module does not decorate it — the report groups by file already" in {
      Compiler.compileTests(List(Source("m.sysl", """module m
                                                    |
                                                    |@test
                                                    |adds_two() = 0
                                                    |""".stripMargin, List("m"))), Nil) match {
        case Right((_, tests)) =>
          tests.map(_.display) shouldBe List("adds_two")
          tests.map(_.func) shouldBe List("m$adds_two")
        case Left(e) => fail(e)
      }
    }

    "the position a report points at is the attribute's, not the function's" in {
      Compiler.compileTests(List(Source("m.sysl", """double(n: int) -> int = n * 2
                                                    |
                                                    |@test
                                                    |t() = 0
                                                    |""".stripMargin)), Nil) match {
        case Right((_, tests)) => tests.map(t => (t.file, t.line)) shouldBe List(("m.sysl", 3))
        case Left(e)           => fail(e)
      }
    }

    "tests are listed in the order the source declared them" in {
      discovered("""@test
                   |third() = 0
                   |
                   |@test
                   |first() = 0
                   |
                   |@test
                   |second() = 0
                   |""".stripMargin) shouldBe List("third", "first", "second")
    }
  }

  "the shapes the grammar has to say no to" - {
    // A member is not a declaration statement — it parses under the rule that reads a struct's,
    // an enum's, a trait's and an `impl`'s alike — so `@test` has no place there. The refusal comes
    // from the member grammar rather than from the attribute rule, which is why it is pinned: the
    // message a reader gets is the one that rule gives, and this says which rule that is.
    "a method cannot be a test" in {
      err("""struct Point
            |    x: int
            |
            |    @test
            |    t(self) = 0
            |""".stripMargin) should not be empty
    }

    // Every one of the four below used to reach the same sentence — that an annotation marks a
    // function and only a function — which is about the *declaration below* and is true of it. So
    // the reader was sent to read a function that was never the problem, with a caret on a
    // parenthesis the message did not mention. Having read the `(` there is nothing else the author
    // could have been writing, so the argument list answers for itself from there on.
    "an attribute with empty parentheses says nothing, and is not a form" in {
      err("""@test()
            |t() = 0
            |""".stripMargin) should include(
        "'@test' takes a description or nothing at all, and '()' is neither — drop the parentheses",
      )
    }

    "a description is a string, and something else in its place says so" in {
      err("""@test(3)
            |t() = 0
            |""".stripMargin) should include("takes the description a report shows it under, written as a string")
    }

    "an argument list left open is answered by the annotation rather than by the line below it" in {
      err("""@test("what holds"
            |t() = 0
            |""".stripMargin) should include("there is no ')' here to end them")
    }

    "'should_trap' left open is the same case" in {
      err("""@test(should_trap
            |t() = 0
            |""".stripMargin) should include("there is no ')' here to end them")
    }

    "one attribute to a declaration" in {
      err("""@test
            |@test
            |t() = 0
            |""".stripMargin) should not be empty
    }

    // Refused, but by the grammar rather than by the attribute rule, so the message is the
    // grammar's. The attribute rule's own sentence needs a token after the newline to be reported
    // against and at the end of a file there is none — every case where something *is* written
    // below gets the better message, which is the one a reader will meet.
    "an attribute at the end of a file has nothing to attach to" in {
      err("@test\n") should not be empty
    }
  }


  "the hooks a module writes around its tests" - {
    def hookOf(src: String): HookAttr =
      parsed(src).collectFirst { case f: FuncDecl if f.hook.isDefined => f.hook.get } match {
        case Some(a) => a
        case None    => fail(s"no hook function was parsed from:\n$src")
      }

    "each of the four is its own moment" in {
      hookOf("@setup\ns() = 0\n").kind shouldBe HookKind.Setup
      hookOf("@teardown\nd() = 0\n").kind shouldBe HookKind.Teardown
      hookOf("@setup_all\ns() = 0\n").kind shouldBe HookKind.SetupAll
      hookOf("@teardown_all\nd() = 0\n").kind shouldBe HookKind.TeardownAll
    }

    // `setup_all` is a whole identifier, so nothing has to order the alternatives to keep it from
    // being read as `setup` with a stray word after it — asserted because the opposite reading is
    // what a longest-match grammar would have needed a rule for.
    "'setup_all' is not 'setup' with something after it" in {
      hookOf("@setup_all\ns() = 0\n").kind should not be HookKind.Setup
    }

    "a hook takes no arguments" in {
      err("""@setup("once")
            |s() = 0
            |""".stripMargin) should include("a hook takes no arguments")
    }

    "the position a report points at is the attribute's" in {
      Compiler.compileTests(List(Source("m.sysl", """double(n: int) -> int = n * 2
                                                    |
                                                    |@setup
                                                    |s() = 0
                                                    |
                                                    |@test
                                                    |t() = 0
                                                    |""".stripMargin)), Nil) match {
        case Right((_, tests)) =>
          tests.head.hooks.setup.map(h => (h.func, h.file, h.line)) shouldBe Some(("s", "m.sysl", 3))
        case Left(e) => fail(e)
      }
    }

    "a module's tests all carry that module's hooks" in {
      Compiler.compileTests(List(Source("m.sysl", """module m
                                                    |
                                                    |@setup
                                                    |s() = 0
                                                    |
                                                    |@test
                                                    |one() = 0
                                                    |
                                                    |@test
                                                    |two() = 0
                                                    |""".stripMargin, List("m"))), Nil) match {
        case Right((_, tests)) =>
          tests.map(_.hooks.setup.map(_.func)) shouldBe List(Some("m$s"), Some("m$s"))
        case Left(e) => fail(e)
      }
    }

    // The scope is the module and nothing smaller, so two files of one module share one hook and two
    // modules keep their own. The second half is what makes the refusal below a rule about a module
    // rather than about a file.
    "a hook reaches only the module that declared it" in {
      Compiler.compileTests(files(
        "a.sysl" -> """module a
                      |
                      |@setup
                      |s() = 0
                      |
                      |@test
                      |mine() = 0
                      |""".stripMargin,
        "b.sysl" -> """module b
                      |
                      |@test
                      |theirs() = 0
                      |""".stripMargin), Nil) match {
        case Right((_, tests)) =>
          tests.map(t => t.display -> t.hooks.setup.map(_.func)).toMap shouldBe
            Map("mine" -> Some("a$s"), "theirs" -> None)
        case Left(e) => fail(e)
      }
    }

    "a second hook of one kind in one module is refused, and the message names both" in {
      val message = errOf(
        "a.sysl" -> """module a
                      |
                      |@setup
                      |first() = 0
                      |
                      |@setup
                      |second() = 0
                      |
                      |@test
                      |t() = 0
                      |""".stripMargin)

      message should include("at most one '@setup'")
      message should include("first")
      message should include("second")
      message should include("a.sysl:3")
    }

    "the same kind in two modules is two hooks and no complaint" in {
      Compiler.compileTests(files(
        "a.sysl" -> """module a
                      |
                      |@setup
                      |s() = 0
                      |
                      |@test
                      |mine() = 0
                      |""".stripMargin,
        "b.sysl" -> """module b
                      |
                      |@setup
                      |s() = 0
                      |
                      |@test
                      |theirs() = 0
                      |""".stripMargin), Nil) match {
        case Right((_, tests)) => tests.map(_.hooks.setup.map(_.func)) shouldBe List(Some("a$s"), Some("b$s"))
        case Left(e)           => fail(e)
      }
    }

    "the four kinds are four scopes, so all four may stand in one module" in {
      Compiler.compileTests(List(Source("m.sysl", """module m
                                                    |
                                                    |@setup
                                                    |up() = 0
                                                    |
                                                    |@teardown
                                                    |down() = 0
                                                    |
                                                    |@setup_all
                                                    |boot() = 0
                                                    |
                                                    |@teardown_all
                                                    |halt() = 0
                                                    |
                                                    |@test
                                                    |t() = 0
                                                    |""".stripMargin, List("m"))), Nil) match {
        case Right((_, tests)) =>
          tests.head.hooks.all.map(_.func) should contain theSameElementsAs
            List("m$up", "m$down", "m$boot", "m$halt")
        case Left(e) => fail(e)
      }
    }

    "two hooks above one declaration name two moments" in {
      val message = err("""@setup
                          |@teardown
                          |s() = 0
                          |""".stripMargin)

      message should include("different moments")
      message should include("'@setup'")
      message should include("'@teardown'")
    }

    "'@test' beside a hook is the same mistake" in {
      err("""@test
            |@setup
            |s() = 0
            |""".stripMargin) should include("different moments")
    }

    "one hook written twice above one declaration is the ordinary repeat" in {
      err("""@setup
            |@setup
            |s() = 0
            |""".stripMargin) should include("written twice above one declaration")
    }
  }

  "what a hook function may be is what the runner can call" - {
    "a parameter has nowhere to come from, and the message names the hook" in {
      val message = err("""@setup
                          |s(n: int) =
                          |    print(n)
                          |""".stripMargin)

      message should include("a '@setup' function takes no parameters")
    }

    "a generic has no compiled form until a caller fixes its arguments" in {
      err("""@teardown_all
            |d[T]() = 0
            |""".stripMargin) should include("a '@teardown_all' function has no type parameters")
    }

    "a result is a value nothing is going to read" in {
      err("""@setup_all
            |s() -> int = 3
            |""".stripMargin) should include("a '@setup_all' function returns nothing")
    }
  }

  "a hook is scaffolding, and a build that runs nothing drops it" - {
    // The same bargain a test gets: analyzed in every build, emitted in one. A hook that stopped
    // being checked outside `sysl test` would rot exactly as an unchecked test would.
    "a broken hook is an error in an ordinary build" in {
      err("""@setup
            |s() =
            |    print(undefined_name)
            |""".stripMargin) should include("undefined name 'undefined_name'")
    }

    "an ordinary build emits neither the hook nor what only it calls" in {
      val out = ir("""only_the_hook_calls_me() = print("scaffolding")
                     |
                     |@setup
                     |s() =
                     |    only_the_hook_calls_me()
                     |
                     |print("program")
                     |""".stripMargin)

      out should not include "only_the_hook_calls_me"
    }
  }

  "a test is analyzed whether or not the build would run it" - {
    // The property that lets a test sit beside what it tests: dropping them is an *emission*
    // decision, taken after every check has run. A test that stopped being checked the moment it
    // stopped being emitted would rot in place, and nobody would find out until they ran it.
    "a broken test is an error in an ordinary build" in {
      err("""@test
            |t() =
            |    print(undefined_name)
            |""".stripMargin) should include("undefined name 'undefined_name'")
    }

    "a broken test is an error in a test build too" in {
      Compiler.compileTests(List(Source("<input>", """@test
                                                     |t() =
                                                     |    print(undefined_name)
                                                     |""".stripMargin)), Nil) match {
        case Left(e)  => e should include("undefined name 'undefined_name'")
        case Right(_) => fail("expected the broken test to be reported")
      }
    }
  }
}
