package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** `@tests` — a file of a module's test scaffolding, kept by `sysl test` and dropped by every other
 * build (`reference/attributes.md § @tests — a file of scaffolding`).
 *
 * The header says two things and both are pinned here, because either alone is unsound. It **drops**
 * the file, which is what keeps a helper out of a program and out of a library's artifact; and it
 * **restricts** who may name what the file declared, which is what makes dropping it safe rather
 * than a link error waiting for whoever writes the reference.
 *
 * The restriction is stated over the referring *declaration* and not over the file it sits in, and
 * the case that forces it is the one `reference/attributes.md § @tests — a file of scaffolding` asks for: a `@test` written beside what it tests,
 * in an ordinary file, calling scaffolding from the test file. A rule about files would refuse that
 * and drive every test out of the module's real files.
 */
class TestFileTests extends AnyFreeSpec with CodegenSupport with RunSupport {

  /** Every test a project declares, run, in the order the sources declared them. */
  private def ranIn(fs: (String, String, String)*): List[TestRunner.Outcome] = {
    assume(Toolchain.clangAvailable, "clang not available")

    val (built, tests) = Compiler.compileTests(project(fs*), Nil) match {
      case Right(result) => result
      case Left(e)       => fail(e)
    }

    val exe = io.github.edadma.cross_platform.createTempFile("sysl-test-", "")

    try
      Toolchain.build(built.ir, exe, links = built.links) match {
        case Left(e)  => fail(e)
        case Right(_) => TestRunner.execute(exe, tests, TestRunner.Options())
      }
    finally
      try io.github.edadma.cross_platform.deleteFile(exe)
      catch case _: Exception => ()
  }

  private def parsed(src: String): Program =
    SyslParser.parse(Source("<input>", src)) match {
      case Right(p) => p
      case Left(e)  => fail(e)
    }

  "the header attribute parses where the others do" - {

    "under a module header, with the capability clauses" in {
      parsed("module m\n@tests\n\nf() -> int = 1\n").testOnly shouldBe true
    }

    "beside one, in either order" in {
      parsed("module m\n@no_alloc\n@tests\n\nf() -> int = 1\n").testOnly shouldBe true
      parsed("module m\n@tests\n@no_alloc\n\nf() -> int = 1\n").testOnly shouldBe true
    }

    "and it says nothing about the capabilities beside it" in {
      val p = parsed("module m\n@tests\n@no_alloc\n\nf() -> int = 1\n")

      p.capabilities.map(_.name) shouldBe List("alloc")
    }

    "opening a file that declares no module, where the root module's header would go" in {
      parsed("@tests\n\nf() -> int = 1\n").testOnly shouldBe true
    }

    "and a file that does not write it is not one" in {
      parsed("module m\n\nf() -> int = 1\n").testOnly shouldBe false
    }
  }

  "written anywhere but the header, it says where it belongs" - {

    "below the statements it would otherwise seem to cover" in {
      err("f() -> int = 1\n@tests\n") should include("belongs in the file's header")
    }

    "and between two declarations, where an annotation would go" in {
      err("module m\n\nf() -> int = 1\n\n@tests\ng() -> int = 2\n") should
        include("belongs in the file's header")
    }

    // What ends the header is the first statement, not the first blank line — so a file that puts
    // air between 'module' and its attributes is written the way most of them are.
    "though a blank line between it and 'module' leaves it in the header" in {
      parsed("module m\n\n\n@tests\n\nf() -> int = 1\n").testOnly shouldBe true
    }
  }

  "what such a file declares is dropped from every build but a test build" - {

    "a helper is not defined in the program that was built beside it" in {
      val out = irIn(
        ("", "main.sysl", "import m.*\nprint(double(21))"),
        ("m", "m.sysl", "module m\n\ndouble(n: int) -> int = n * 2"),
        ("m", "tests.sysl", "module m\n@tests\n\nquadruple(n: int) -> int = double(double(n))"),
      )

      out should include("@m$double")
      out should not include "quadruple"
    }

    "module storage goes with it" in {
      val out = irIn(
        ("", "main.sysl", "import m.*\nprint(double(21))"),
        ("m", "m.sysl", "module m\n\ndouble(n: int) -> int = n * 2"),
        ("m", "tests.sysl", "module m\n@tests\n\nval fixture: int = 7"),
      )

      out should not include "fixture"
    }

    "and the program still runs" in {
      runIn(
        ("", "main.sysl", "import m.*\nprint(double(21))"),
        ("m", "m.sysl", "module m\n\ndouble(n: int) -> int = n * 2"),
        ("m", "tests.sysl", "module m\n@tests\n\nquadruple(n: int) -> int = double(double(n))"),
      ) shouldBe "42\n"
    }
  }

  "and kept by 'sysl test', which is the whole point of writing one" - {

    "a test in such a file runs, and the helper beside it is there to be called" in {
      val ran = ranIn(
        ("", "main.sysl", "import m.*\nprint(double(21))"),
        ("m", "m.sysl", "module m\n\ndouble(n: int) -> int = n * 2"),
        ("m", "tests.sysl",
         "module m\n@tests\n\nquadruple(n: int) -> int = double(double(n))\n\n" +
           "@test\ndoubling_twice_is_quadrupling() =\n    assert(quadruple(3) == 12)\n"),
      )

      ran.map(o => o.test.display -> o.passed) shouldBe List("doubling_twice_is_quadrupling" -> true)
    }

    "a '@test' in an ordinary file may name the scaffolding, since the two are dropped together" in {
      val ran = ranIn(
        ("", "main.sysl", "import m.*\nprint(double(21))"),
        ("m", "m.sysl",
         "module m\n\ndouble(n: int) -> int = n * 2\n\n" +
           "@test\nbeside_what_it_tests() =\n    assert(quadruple(5) == 20)\n"),
        ("m", "tests.sysl", "module m\n@tests\n\nquadruple(n: int) -> int = double(double(n))"),
      )

      ran.map(o => o.test.display -> o.passed) shouldBe List("beside_what_it_tests" -> true)
    }

    "and one test file may name another's" in {
      val ran = ranIn(
        ("", "main.sysl", "import m.*\nprint(double(21))"),
        ("m", "m.sysl", "module m\n\ndouble(n: int) -> int = n * 2"),
        ("m", "fixtures.sysl", "module m\n@tests\n\nquadruple(n: int) -> int = double(double(n))"),
        ("m", "tests.sysl",
         "module m\n@tests\n\n@test\nacross_two_test_files() =\n    assert(quadruple(4) == 16)\n"),
      )

      ran.map(_.passed) shouldBe List(true)
    }
  }

  /** A closure is lowered to a function of its own under a name no reader wrote, so the rule has to
   * follow the body it was written in rather than the name it was filed under. Both directions are
   * pinned here, because a fix that only widened the exemption would pass the first three and lose
   * the fourth.
   */
  "a closure is judged by the body it was written in" - {

    val consumer =
      "module m\n\ndouble(n: int) -> int = n * 2\n\n" +
        "apply(f: &Fn(int) -> int, n: int) -> int = f(n)"

    val scaffolding = "module m\n@tests\n\nquadruple(n: int) -> int = double(double(n))"

    "a lambda in a test may call what the test file declared" in {
      val ran = ranIn(
        ("", "main.sysl", "import m.*\nprint(double(21))"),
        ("m", "m.sysl", consumer),
        ("m", "tests.sysl",
         scaffolding + "\n\n@test\na_lambda_names_its_own_file() =\n" +
           "    assert(apply(v -> quadruple(v), 3) == 12)\n"),
      )

      ran.map(o => o.test.display -> o.passed) shouldBe List("a_lambda_names_its_own_file" -> true)
    }

    "and a bare name, which is the capture-free closure and so the same question" in {
      val ran = ranIn(
        ("", "main.sysl", "import m.*\nprint(double(21))"),
        ("m", "m.sysl", consumer),
        ("m", "tests.sysl",
         scaffolding + "\n\n@test\na_bare_name_from_a_test_file() =\n" +
           "    assert(apply(quadruple, 3) == 12)\n"),
      )

      ran.map(o => o.test.display -> o.passed) shouldBe List("a_bare_name_from_a_test_file" -> true)
    }

    "and storage it declared, which is the third way a name is reached at all" in {
      val ran = ranIn(
        ("", "main.sysl", "import m.*\nprint(double(21))"),
        ("m", "m.sysl", consumer),
        ("m", "tests.sysl",
         "module m\n@tests\n\nval fixture: int = 7\n\n" +
           "@test\na_lambda_reads_test_storage() =\n    assert(apply(v -> v + fixture, 3) == 10)\n"),
      )

      ran.map(o => o.test.display -> o.passed) shouldBe List("a_lambda_reads_test_storage" -> true)
    }

    /** A generic taking a callable is instantiated at the closure's own type, and the call inside
     * that instantiation is a direct call on the closure's body — so an ordinary library function
     * ends up naming it. Reporting that would name `$closure0.call` at a line in the library, which
     * is neither a name the program contains nor a place the reader can act on.
     */
    "and a generic instantiated at a test's closure is not a mistake to report" in {
      val ran = ranIn(
        ("", "main.sysl", "import m.*\nprint(double(21))"),
        ("m", "m.sysl",
         "module m\n\ndouble(n: int) -> int = n * 2\n\n" +
           "both[T](f: (T, T) -> T, a: T, b: T) -> T = f(a, b)"),
        ("m", "tests.sysl",
         scaffolding + "\n\n@test\na_generic_at_a_test_closure() =\n" +
           "    assert(both((x, y) -> quadruple(x) + y, 2, 3) == 11)\n"),
      )

      ran.map(o => o.test.display -> o.passed) shouldBe List("a_generic_at_a_test_closure" -> true)
    }

    "and a nested function, which is a closure with a name" in {
      val ran = ranIn(
        ("", "main.sysl", "import m.*\nprint(double(21))"),
        ("m", "m.sysl", consumer),
        ("m", "tests.sysl",
         scaffolding + "\n\n@test\na_nested_function_in_a_test() =\n" +
           "    eight_times(n: int) -> int = quadruple(n) * 2\n\n" +
           "    assert(eight_times(2) == 16)\n"),
      )

      ran.map(o => o.test.display -> o.passed) shouldBe List("a_nested_function_in_a_test" -> true)
    }

    "and a closure inside a closure, which is still inside the test that wrote it" in {
      val ran = ranIn(
        ("", "main.sysl", "import m.*\nprint(double(21))"),
        ("m", "m.sysl", consumer),
        ("m", "tests.sysl",
         scaffolding + "\n\n@test\na_closure_inside_a_closure() =\n" +
           "    assert(apply(v -> apply(w -> quadruple(w), v), 3) == 12)\n"),
      )

      ran.map(o => o.test.display -> o.passed) shouldBe List("a_closure_inside_a_closure" -> true)
    }

    /** A default is written at the declaration and analyzed outside every body, so which
     * declaration it belongs to is something that pass has to say for itself. Getting it from
     * whatever was analyzed last would file an ordinary function's default closure as a test's and
     * drop it from the build that ships — a missing symbol at the link rather than a diagnostic.
     */
    "a parameter default's closure belongs to the declaration that wrote it" in {
      runIn(
        ("", "main.sysl", "import m.*\nprint(with_default(4))"),
        ("m", "m.sysl",
         "module m\n\napply(f: &Fn(int) -> int, n: int) -> int = f(n)\n\n" +
           "with_default(n: int, f: &Fn(int) -> int = (v: int) -> v * 3) -> int = apply(f, n)"),
        ("m", "tests.sysl", "module m\n@tests\n\nval fixture: int = 7"),
      ) shouldBe "12\n"
    }

    "an ordinary function's closure may NOT, exactly as the function itself may not" in {
      val e = errIn(
        ("", "main.sysl", "import m.*\nprint(twice_over(21))"),
        ("m", "m.sysl", consumer + "\n\ntwice_over(n: int) -> int = apply(v -> quadruple(v), n)"),
        ("m", "tests.sysl", scaffolding),
      )

      e should include("'m.quadruple' is declared in a file that said '@tests'")
    }

    /** The exemption is sound only because the drop reaches the lowered body too. Left behind, it
     * would call a helper that is no longer there — and the method table registering it as an `Fn`
     * is one of pruning's *roots*, so nothing downstream would remove it either.
     *
     * The closure's **type** is still defined, exactly as the types a test file declares are: a
     * definition nothing reads costs the output no code. What must not be there is a body.
     */
    "and what a test's closure lowered to is dropped with the test" in {
      val out = irIn(
        ("", "main.sysl", "import m.*\nprint(double(21))"),
        ("m", "m.sysl", consumer),
        ("m", "tests.sysl",
         scaffolding + "\n\n@test\na_lambda_names_its_own_file() =\n" +
           "    assert(apply(v -> quadruple(v), 3) == 12)\n"),
      )

      out should not include "quadruple"
      out.linesIterator.filter(_.startsWith("define")).mkString("\n") should not include "closure"
    }
  }

  "anything else that names one is told so where it wrote the name" - {

    "a function of an ordinary file" in {
      val e = errIn(
        ("", "main.sysl", "import m.*\nprint(twice_over(21))"),
        ("m", "m.sysl",
         "module m\n\ndouble(n: int) -> int = n * 2\n\ntwice_over(n: int) -> int = quadruple(n)"),
        ("m", "tests.sysl", "module m\n@tests\n\nquadruple(n: int) -> int = double(double(n))"),
      )

      e should include("'m.quadruple' is declared in a file that said '@tests'")
      e should include("only another such file, or a '@test' function, may name it")
    }

    "the program's own statements, which no build drops at all" in {
      errIn(
        ("", "main.sysl", "import m.*\nprint(quadruple(21))"),
        ("m", "m.sysl", "module m\n\ndouble(n: int) -> int = n * 2"),
        ("m", "tests.sysl", "module m\n@tests\n\nquadruple(n: int) -> int = double(double(n))"),
      ) should include("is declared in a file that said '@tests'")
    }

    "a read of storage it declared" in {
      errIn(
        ("", "main.sysl", "import m.*\nprint(fixture)"),
        ("m", "m.sysl", "module m\n\ndouble(n: int) -> int = n * 2"),
        ("m", "tests.sysl", "module m\n@tests\n\nval fixture: int = 7"),
      ) should include("is declared in a file that said '@tests'")
    }

    "and taking a helper's address, which is a use of it exactly as calling it is" in {
      errIn(
        ("", "main.sysl", "import m.*\nval f: *extern(int) -> int = &quadruple\nprint(f(21))"),
        ("m", "m.sysl", "module m\n\ndouble(n: int) -> int = n * 2"),
        ("m", "tests.sysl", "module m\n@tests\n\nquadruple(n: int) -> int = double(double(n))"),
      ) should include("is declared in a file that said '@tests'")
    }
  }

  /** `private` in sysl is private to the **file** (`reference/modules.md § A file-private name is
   * scoped to its file`), so two files of one module may each declare their own `pick` — and only
   * one of them can hold the plain module-qualified key, the other taking a numbered one. The same
   * is true of an overload: the first declaration keeps the plain key and the rest are numbered.
   *
   * So which declaration a `@tests` header is about is a question about the **key**, and asking it
   * about the plain spelling answered for whichever file was hoisted first. Both directions were
   * wrong, and the pair is why the marking follows the file rather than the name: an ordinary file
   * first had its own helper reported at its own call site as reaching into the test file, and the
   * test file first left the ordinary file's declaration marked as scaffolding under a numbered key
   * — which put an internal spelling into a diagnostic and would have dropped a shipping
   * declaration.
   */
  "a name two files of one module contend for is marked by the file that wrote it" - {

    val ordinary = "module m\n\nprivate pick(n: int) -> int = n\n\nuse(n: int) -> int = pick(n)"

    val tests =
      "module m\n@tests\n\nprivate pick(s: string) -> string = s\n\n" +
        "@test\nthe_tests_file_picks_a_string() =\n    assert(pick(\"x\") == \"x\")\n"

    "the ordinary file's call answers its own file-private helper" in {
      runIn(
        ("", "main.sysl", "import m.*\nprint(use(3))"),
        ("m", "a.sysl", ordinary),
        ("m", "b.sysl", tests),
      ) shouldBe "3\n"
    }

    "and the test file's own is still its own, so the test runs" in {
      val ran = ranIn(
        ("", "main.sysl", "import m.*\nprint(use(3))"),
        ("m", "a.sysl", ordinary),
        ("m", "b.sysl", tests),
      )

      ran.map(o => o.test.display -> o.passed) shouldBe List("the_tests_file_picks_a_string" -> true)
    }

    "whichever of the two the walk reaches first" in {
      runIn(
        ("", "main.sysl", "import m.*\nprint(use(3))"),
        ("m", "a_tests.sysl", tests),
        ("m", "z.sysl", ordinary),
      ) shouldBe "3\n"

      val ran = ranIn(
        ("", "main.sysl", "import m.*\nprint(use(3))"),
        ("m", "a_tests.sysl", tests),
        ("m", "z.sysl", ordinary),
      )

      ran.map(o => o.test.display -> o.passed) shouldBe List("the_tests_file_picks_a_string" -> true)
    }

    /** The control the pair above is read against: nothing here says `@tests` at all, and two files
     * each keeping a `pick` to themselves is the ordinary arrangement the modifier is for.
     */
    "two ordinary files each with one is the control, and compiles" in {
      runIn(
        ("", "main.sysl", "import m.*\nprint(use(3))\nprint(use_s(\"x\"))"),
        ("m", "a.sysl", ordinary),
        ("m", "b.sysl",
         "module m\n\nprivate pick(s: string) -> string = s\n\nuse_s(s: string) -> string = pick(s)"),
      ) shouldBe "3\nx\n"
    }

    "and an overload split across the two is the same question about the same keys" in {
      val ran = ranIn(
        ("", "main.sysl", "import m.*\nprint(use(3))"),
        ("m", "a.sysl", "module m\n\npick(n: int) -> int = n\n\nuse(n: int) -> int = pick(n)"),
        ("m", "b.sysl",
         "module m\n@tests\n\npick(s: string) -> string = s\n\n" +
           "@test\nan_overload_in_the_tests_file() =\n" +
           "    assert(pick(\"x\") == \"x\")\n    assert(pick(2) == 2)\n"),
      )

      ran.map(o => o.test.display -> o.passed) shouldBe List("an_overload_in_the_tests_file" -> true)
    }

    /** What the marking must not lose: the restriction is still the file's, so a test file naming an
     * ordinary file's file-private helper is refused — by the privacy rule rather than by the
     * `@tests` one, because a name this file may not reach is not a candidate for it at all.
     */
    "while a test file naming an ordinary file's private helper is still refused" in {
      errIn(
        ("", "main.sysl", "import m.*\nprint(use(3))"),
        ("m", "a.sysl", "module m\n\nprivate helper(n: int) -> int = n\n\nuse(n: int) -> int = helper(n)"),
        ("m", "b.sysl",
         "module m\n@tests\n\n@test\nreaches_across() =\n    assert(helper(1) == 1)\n"),
      ) should include("'m.helper' is private to 'a.sysl', the file that declares it")
    }

    "and a public declaration in a test file shadows nothing an ordinary file kept private" in {
      runIn(
        ("", "main.sysl", "import m.*\nprint(use(3))"),
        ("m", "a.sysl", ordinary),
        ("m", "b.sysl", "module m\n@tests\n\npick(s: string) -> string = s"),
      ) shouldBe "3\n"
    }
  }

  "an 'impl' block may not sit in one" - {

    "because a test build would keep it and every other build would drop it" in {
      val e = errIn(
        ("", "main.sysl", "import m.*\nprint(1)"),
        ("m", "m.sysl", "module m\n\ntrait Scale\n    scale(self, k: int) -> int\n\nstruct P\n    n: int"),
        ("m", "tests.sysl",
         "module m\n@tests\n\nimpl Scale for P\n    scale(self, k: int) -> int = self.n * k\n"),
      )

      e should include("an 'impl' block may not sit in a file that said '@tests'")
      e should include("It belongs beside the type")
    }
  }

  "a library ships none of it" - {

    "the artifact advertises what the module declared and not what its tests did" in {
      val sources = List(
        Source("demo/lib.sysl", "module demo\n\ndouble(n: int) -> int = n * 2\n", List("demo")),
        Source("demo/tests.sysl",
               "module demo\n@tests\n\nquadruple(n: int) -> int = double(double(n))\n", List("demo")),
      )

      LibraryArtifact.build(sources) match {
        case Left(e) => fail(s"the library did not build: $e")
        case Right((ir, meta)) =>
          // Neither half carries it: the compiled half because `Tests.strip` took the definition out,
          // the tree half because the file was left out of the metadata. A consumer reads the second
          // to instantiate a generic, so a helper left there would be nameable by everything that
          // links the library — which is the whole hazard `@tests` exists to close.
          ir should not include "quadruple"

          LibraryArtifact.read("demo.syslib", meta, Target.default) match {
            case Left(e)              => fail(e)
            case Right((units, _, _)) =>
              Library.names(units.flatMap(_.body)).toList.sorted shouldBe List("double")
          }
      }
    }

    /** A library's tests are **parsed but never analyzed**, and these three pin exactly where that
      * line falls — which is not where "`build-lib` no longer checks a library's tests" would put it.
      *
      * What is given up is everything *after* the parse: name resolution, types, visibility,
      * capabilities, `@test` well-formedness, generic instantiation. It was given up because
      * analysis is not a passive reading — a test naming `Buf[int]` *creates* the whole of `Buf` at
      * `int`, and an instantiation is an ordinary library function afterwards, with nothing in it
      * recording which declaration demanded it. Analyze the tests and the artifact ships what they
      * caused, however carefully the declarations are filtered out afterwards.
      *
      * Where a library test's real errors *are* reported is `sysl test` — `--std` for the standard
      * library, which `StdSelfTests` runs as part of this suite, and plain `sysl test` for anyone
      * else's.
      */
    "an undefined name in one is no longer a build error, because it is never analyzed" in {
      val sources = List(
        Source("demo/lib.sysl", "module demo\n\ndouble(n: int) -> int = n * 2\n", List("demo")),
        Source("demo/tests.sysl", "module demo\n@tests\n\nbad() -> int = nosuchthing()\n", List("demo")),
      )

      LibraryArtifact.build(sources) match {
        case Right(_) => succeed
        case Left(e)  => fail(s"a library's own tests should not be compiled by build-lib: $e")
      }
    }

    /** **The other side of the line, and the one most likely to be got wrong when reading the rule
      * back.** `LibraryArtifact.build` parses every source and returns on the first `Left` before
      * `compileLibrary` is reached, so a `@tests` file that is not well-formed *text* still stops
      * the build — the strip never sees it, because there is no tree to strip from.
      *
      * Worth a test of its own because the natural summary of this change — "a library's tests are
      * not checked" — implies this file builds clean, and it does not.
      */
    "but a syntax error in one still is, because the strip happens after the parse" in {
      val sources = List(
        Source("demo/lib.sysl", "module demo\n\ndouble(n: int) -> int = n * 2\n", List("demo")),
        Source("demo/tests.sysl", "module demo\n@tests\n\n@test\nbad( = )\n", List("demo")),
      )

      LibraryArtifact.build(sources) match {
        case Right(_) => fail("a library whose test file does not parse built without a word")
        case Left(e)  => e should include("demo/tests.sysl")
      }
    }

    // Discriminating against the first of these, and the reason they are a set: a `build-lib` that
    // had stopped analyzing the library *altogether* would pass it for entirely the wrong reason.
    // The same undefined name, in a declaration that is not a test, is still a build error.
    "while an ordinary declaration with the same undefined name still is" in {
      val sources = List(
        Source("demo/lib.sysl", "module demo\n\nordinary() -> int = nosuchthing()\n", List("demo")),
      )

      LibraryArtifact.build(sources) match {
        case Right(_) => fail("a library with a broken ordinary declaration built without a word")
        case Left(e)  => e should include("nosuchthing")
      }
    }

    /** **The other end of the trade, and the assertion that justifies making it.**
      *
      * Everything the three above give up, a test build has to still catch, or "the net moved rather
      * than went" is a sentence with nothing behind it. The demanding case is a helper in a `@tests`
      * file that **no test calls**: nothing reaches it, so a checker that worked outwards from the
      * tests would never look at it, and it is precisely the declaration `build-lib` used to be the
      * only thing looking at.
      *
      * Both are asserted in one compilation because the analyzer reports every mistake it finds, so
      * a pass that had stopped at the first would show up as the second going missing.
      */
    "but a test build still analyzes all of it, including a helper nothing calls" in {
      val sources = List(
        Source("demo/lib.sysl", "module demo\n\ndouble(n: int) -> int = n * 2\n", List("demo")),
        Source("demo/tests.sysl",
               """module demo
                 |@tests
                 |
                 |unreached() -> int = nosuchthing()
                 |
                 |@test
                 |bad() = assert_eq(double(nowhere()), 4)
                 |""".stripMargin,
               List("demo")),
      )

      Compiler.compileTests(sources, Nil) match {
        case Right(_) => fail("a test build compiled a tree whose test file names two undefined functions")
        case Left(e) =>
          e should include("nosuchthing")
          e should include("nowhere")
      }
    }

    // The same again for a `@test` written in an ordinary file rather than a `@tests` one, since
    // `stripSource` removes the two by different rules — a file whole, and a declaration out of a
    // file that stays.
    "and a broken '@test' in an ordinary file is not a build error either" in {
      val sources = List(
        Source("demo/lib.sysl",
               "module demo\n\ndouble(n: int) -> int = n * 2\n\n@test\nbad() = nosuchthing()\n",
               List("demo")),
      )

      LibraryArtifact.build(sources) match {
        case Right((ir, _)) => ir should not include "bad"
        case Left(e)        => fail(s"a '@test' in a library should not be compiled by build-lib: $e")
      }
    }
  }

  /** **Two `@tests` files of one module, each with a `private` helper of the same name.**
   *
   * `private` in sysl is file-private, so this is ordinary — the two are different declarations and
   * `declKey` gives the second a numbered key of its own so they can coexist. What that key does not
   * inherit for free is being scaffolding: `testOnlyDecls` is filled *before* hoisting, which is what
   * lets it remember which file wrote a declaration and is also why it can only hold the plain
   * spelling. So the second file's body was held to the rule for shipped code and reported for
   * calling the very file it is in.
   *
   * Found 2026-08-27 by `sysl.fs` growing a third test file, each with a `private scratch`.
   */
  "a file-private helper declared in two '@tests' files of one module is scaffolding in both" - {

    "the second file's helper may name what its own file declares" in {
      // **The shape matters and a symmetrical fixture misses the branch entirely.** What is scanned
      // is a function whose key is *not* in `testOnlyDecls`, and what is reported is a name that
      // *is* — so the numbered `scratch` has to call something whose key is plain. Two files each
      // declaring `helper` and `scratch` gives the second file a numbered key for both, and then
      // nothing it calls is reportable: the fixture passes with the fix removed. `only_here` is
      // declared once, so it keeps the plain key and is the name the walk would object to.
      val ran = ranIn(
        ("", "main.sysl", "import m.*\nprint(double(21))"),
        ("m", "m.sysl", "module m\n\ndouble(n: int) -> int = n * 2"),
        ("m", "a_tests.sysl",
         "module m\n@tests\n\nprivate scratch(n: int) -> int = n + 1\n\n" +
           "@test\nfirst_file() =\n    assert(scratch(1) == 2)\n"),
        ("m", "b_tests.sysl",
         "module m\n@tests\n\nonly_here(n: int) -> int = n + 10\n\n" +
           "private scratch(n: int) -> int = only_here(n)\n\n" +
           "@test\nsecond_file() =\n    assert(scratch(1) == 11)\n"),
      )

      ran.map(o => o.test.display -> o.passed) shouldBe
        List("first_file" -> true, "second_file" -> true)
    }

    // The drop is the other half of the header's promise, and a numbered key that was never marked
    // scaffolding would be asked about under a name `Tests.strip` does not know. Neither helper may
    // reach a program built beside them.
    "and neither file's helper is defined in the program built beside them" in {
      val out = irIn(
        ("", "main.sysl", "import m.*\nprint(double(21))"),
        ("m", "m.sysl", "module m\n\ndouble(n: int) -> int = n * 2"),
        ("m", "a_tests.sysl", "module m\n@tests\n\nprivate scratch(n: int) -> int = n + 1"),
        ("m", "b_tests.sysl", "module m\n@tests\n\nprivate scratch(n: int) -> int = n + 10"),
      )

      out should include("@m$double")
      out should not include "scratch"
    }
  }

  "the flag survives the artifact codec" in {
    // It is never written to a real artifact, since the file is filtered out before the encode. It
    // travels anyway because a codec that quietly defaulted a field is the failure `Version` exists
    // to catch, and one nobody would be told about.
    val p = parsed("module m\n@tests\n\nf() -> int = 1\n")

    AstCodec.decode(AstCodec.encode(List(p)), Map("<input>" -> p.source)) match {
      case Left(e)         => fail(s"decode failed: $e")
      case Right(List(back)) => back.testOnly shouldBe true
      case Right(other)    => fail(s"expected one program, got ${other.length}")
    }
  }
}
