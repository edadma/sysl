package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Two files of one module may each declare the same **file-private** name
 * (`reference/modules.md § Visibility`).
 *
 * `private` in sysl is private to the file, and until 2026-08-27 it restricted the *reach* of a name
 * without restricting the *namespace*: a second file declaring its own `Limit` was refused as a
 * duplicate of a name it could not have named anyway. That defeated the thing file-privacy is for —
 * the reason to keep a helper to its file is that `Limit`, `helper`, `check` are local matters — so
 * a module of any size grew `MaxCallDepth` beside `MaxHashDepth` for two bounds that were each one
 * file's business. Rust, C and Go all scope the name as well as the reach.
 *
 * **What stays refused is a private name against a PUBLIC one of the same spelling**, which is a
 * genuine ambiguity for the sibling file's own references, and a second declaration of one spelling
 * inside a single file, which is the ordinary duplicate this never meant to allow.
 */
class FilePrivateNameTests extends AnyFreeSpec with CodegenSupport with RunSupport {

  "two files may each keep a name to themselves" - {

    "a constant" in {
      runIn(
        ("", "main.sysl", "print(m.from_one(), m.from_two())"),
        ("m", "one.sysl",
         """module m
           |private const Limit: int = 1
           |from_one() -> int = Limit
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private const Limit: int = 2
           |from_two() -> int = Limit
           |""".stripMargin),
      ) shouldBe "1 2\n"
    }

    // The card's own case: two files of a module each bounding how deep something walks, each
    // wanting to call the bound what it is.
    "a function, which is the case the card was filed from" in {
      runIn(
        ("", "main.sysl", "print(m.from_one(), m.from_two())"),
        ("m", "one.sysl",
         """module m
           |private helper() -> int = 1
           |from_one() -> int = helper()
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private helper() -> int = 2
           |from_two() -> int = helper()
           |""".stripMargin),
      ) shouldBe "1 2\n"
    }

    "a module 'val'" in {
      runIn(
        ("", "main.sysl", "print(m.from_one(), m.from_two())"),
        ("m", "one.sysl",
         """module m
           |private val base: int = 10
           |from_one() -> int = base
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private val base: int = 20
           |from_two() -> int = base
           |""".stripMargin),
      ) shouldBe "10 20\n"
    }

    "a struct" in {
      runIn(
        ("", "main.sysl", "print(m.from_one(), m.from_two())"),
        ("m", "one.sysl",
         """module m
           |private struct Cell
           |    n: int
           |from_one() -> int = Cell(1).n
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private struct Cell
           |    s: int
           |    t: int
           |from_two() -> int = Cell(2, 3).t
           |""".stripMargin),
      ) shouldBe "1 3\n"
    }

    // Each file's own declaration is what its bodies see — the assertion the two above would still
    // pass if both files somehow shared one declaration, so it is made on its own.
    "and each file names its own, not the other's" in {
      runIn(
        ("", "main.sysl", "print(m.one_says(), m.two_says())"),
        ("m", "one.sysl",
         """module m
           |private const Which: int = 111
           |one_says() -> int = Which
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private const Which: int = 222
           |two_says() -> int = Which
           |""".stripMargin),
      ) shouldBe "111 222\n"
    }
  }

  "what is still refused" - {

    // The sibling file's own references would have two answers with nothing to tell them apart, so
    // this pairing is a real ambiguity rather than the separable case.
    "a private name against a public one of the same spelling" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        ("m", "one.sysl",
         """module m
           |private const Limit: int = 1
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |const Limit: int = 2
           |""".stripMargin),
      ) should include("already declared")
    }

    "and one file declaring the same private name twice, which is the ordinary duplicate" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        ("m", "one.sysl",
         """module m
           |private const Limit: int = 1
           |private const Limit: int = 2
           |""".stripMargin),
      ) should include("already declared")
    }

    // Reach is untouched by any of this: a name scoped to its file is still unreachable from
    // outside it, which is what `private` was always buying.
    "and a sibling file still cannot name the other's private declaration" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        ("m", "one.sysl",
         """module m
           |private const Limit: int = 1
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |borrow() -> int = Limit
           |""".stripMargin),
      ) should include("Limit")
    }
  }

  /** A function name stands for every declaration of it, so reach has to be asked of the whole
   * overload set (`reference/modules.md`: *"A name a file may not reach is not a candidate for
   * it"*).
   *
   * A file-private declaration and a public one of the same spelling in two files are told apart by
   * their arguments the way any overload pair is — from the file that wrote the private one, which
   * sees both. From every other file the private declaration is not a candidate at all: it may not
   * take a call, may not make one ambiguous, and may not stand in the way of the public declaration
   * being found.
   */
  "a private declaration is no candidate outside its own file" - {

    "the public one takes the call its own file makes" in {
      runIn(
        ("", "main.sysl", "print(m.use())"),
        ("m", "one.sysl",
         """module m
           |skip_line(n: int) -> int = n + 1
           |use() -> int = skip_line(1)
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private skip_line(s: string) -> string = s
           |""".stripMargin),
      ) shouldBe "2\n"
    }

    // The defect this suite's section was written from: the sibling's private declaration took a
    // call that no declaration the file can name would have taken, and compiled.
    "and a call the public one does not take is refused rather than reaching the private one" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        ("m", "one.sysl",
         """module m
           |skip_line(n: int) -> int = n
           |use() -> string = skip_line("x")
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private skip_line(s: string) -> string = s
           |""".stripMargin),
      ) should include("is int, but string was given")
    }

    // The other direction, and the one that refused a program outright: the private declaration was
    // written first, so it held the plain key, and every other file was told the *public* name was
    // private to a file it had never heard of.
    "and a public declaration is found from a third file though a private one holds the name" in {
      runIn(
        ("", "main.sysl", "print(m.use_c())"),
        ("m", "one.sysl",
         """module m
           |private skip_line(n: int) -> int = n
           |use_a() -> int = skip_line(1)
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |skip_line(s: string) -> string = s
           |""".stripMargin),
        ("m", "three.sysl",
         """module m
           |use_c() -> string = skip_line("y")
           |""".stripMargin),
      ) shouldBe "y\n"
    }

    "and the file that wrote the private one sees both" in {
      runIn(
        ("", "main.sysl", "print(m.use_b(), m.use_b_int())"),
        ("m", "one.sysl",
         """module m
           |skip_line(n: int) -> int = n + 1
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private skip_line(s: string) -> string = s
           |use_b() -> string = skip_line("x")
           |use_b_int() -> int = skip_line(1)
           |""".stripMargin),
      ) shouldBe "x 2\n"
    }

    "and a public declaration of the same signature is still a duplicate" in {
      errIn(
        ("", "main.sysl", "print(1)"),
        ("m", "one.sysl",
         """module m
           |skip_line(n: int) -> int = n
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private skip_line(n: int) -> int = n
           |""".stripMargin),
      ) should include("already declared")
    }

    // A private slot holds every declaration one file made of the contended spelling, so it carries
    // an overload set of its own. Filing them all under the slot itself dropped every one but the
    // last, and the call the dropped declaration took was then reported against the survivor.
    "and one file may overload the name it keeps to itself" in {
      runIn(
        ("", "main.sysl", "print(m.use_b(), m.use_b2())"),
        ("m", "one.sysl",
         """module m
           |private skip_line(b: bool) -> bool = b
           |use_a() -> bool = skip_line(true)
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private skip_line(n: int) -> int = n
           |private skip_line(s: string) -> string = s
           |use_b() -> string = skip_line("x")
           |use_b2() -> int = skip_line(3)
           |""".stripMargin),
      ) shouldBe "x 3\n"
    }

    "and two of them a call could not tell apart are refused, under the name as written" in {
      val message = errIn(
        ("", "main.sysl", "print(1)"),
        ("m", "one.sysl",
         """module m
           |private skip_line(b: bool) -> bool = b
           |""".stripMargin),
        ("m", "two.sysl",
         """module m
           |private skip_line(n: int) -> int = n
           |private skip_line(n: int) -> string = "x"
           |""".stripMargin),
      )

      message should include("'skip_line' is already declared")
      // The slot is the compiler's answer to a contended spelling, and no reader wrote it.
      message should not include "private1"
    }
  }
}
