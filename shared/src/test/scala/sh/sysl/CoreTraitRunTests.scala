package sh.sysl

import org.scalatest.freespec.AnyFreeSpec

/** Tier-2 runtime behavior of the core trait catalog (`reference/traits.md § Declaring a trait`,
 * `§2`, `§5`): `Self`, the operator traits the library declares, and the memberships the compiler
 * provides for the built-in types.
 *
 * The point of a membership is that it is invisible at run time — a scalar's `add` is the machine's
 * `add`, not a call — so what these check is that the *type system* now agrees a scalar satisfies a
 * bound, and that everything reached through that agreement computes what the operator computes.
 */
class CoreTraitRunTests extends AnyFreeSpec with RunSupport with CodegenSupport {

  /** A user type with the three memberships an operand-sharing form needs, plus a way of building
   * one that says when it is evaluated.
   */
  private val tagged =
    """struct M
      |    v: int
      |impl Ord for M
      |    lt(self, rhs: Self) -> bool = self.v < rhs.v
      |impl Eq for M
      |    eq(self, rhs: Self) -> bool = self.v == rhs.v
      |impl Add for M
      |    add(self, rhs: Self) -> Self = M(self.v + rhs.v)
      |tag(v: int) -> M
      |    print(v)
      |    M(v)
      |""".stripMargin

  "Self" - {
    "a trait signature written with 'Self' is the implementing type" in {
      val src =
        """trait Doubler
          |    twice(self) -> Self
          |struct Money
          |    cents: int
          |impl Doubler for Money
          |    twice(self) -> Self = Money(self.cents * 2)
          |var m = Money(21)
          |print(m.twice().cents)""".stripMargin

      run(src) shouldBe "42\n"
    }

    // The trait says `Self` and the impl says `Money`. Conformance compares resolved types, so the
    // two spellings have to be one signature — if they were not, this would fail to conform.
    "'Self' and the concrete name are interchangeable in an impl" in {
      val src =
        """trait Combine
          |    join(self, other: Self) -> Self
          |struct Money
          |    cents: int
          |impl Combine for Money
          |    join(self, other: Money) -> Money = Money(self.cents + other.cents)
          |var m = Money(30).join(Money(12))
          |print(m.cents)""".stripMargin

      run(src) shouldBe "42\n"
    }

    "'Self' works in a type's own members, signature and body alike" in {
      val src =
        """struct Money
          |    cents: int
          |    grow(self, by: Self) -> Self
          |        var sum: Self = Money(self.cents + by.cents)
          |        sum
          |var m = Money(40).grow(Money(2))
          |print(m.cents)""".stripMargin

      run(src) shouldBe "42\n"
    }
  }

  "a scalar satisfies the bound the compiler gave it" - {
    // The whole point of `§5`: no `impl Add for int` was written, yet `int` and `real` both pass a
    // `[T: Add]`. Two instantiations, so a single accidental specialization could not serve both.
    "a numeric type instantiates a '[T: Add]' generic" in {
      val src =
        """sum[T: Add](a: T, b: T) -> T = a.add(b)
          |print(sum(19, 23))
          |print(sum(1.5, 2.25))""".stripMargin

      run(src) shouldBe "42\n3.75\n"
    }

    // The payoff of the whole chapter: the operator, not the method name, and the bound is what
    // licenses it. Two instantiations, so no single specialization could serve both.
    "an operator on a bounded parameter resolves through its bound" in {
      val src =
        """sum[T: Add](a: T, b: T) -> T = a + b
          |print(sum(19, 23))
          |print(sum(1.5, 2.25))""".stripMargin

      run(src) shouldBe "42\n3.75\n"
    }

    "an ordering operator on a bounded parameter yields a bool" in {
      val src =
        """smaller[T: Ord](a: T, b: T) -> T = if a < b then a else b
          |larger[T: Ord](a: T, b: T) -> T = if a > b then a else b
          |print(smaller(7, 3), larger(7, 3))
          |print(smaller("beta", "alpha"), larger("beta", "alpha"))""".stripMargin

      run(src) shouldBe "3 7\nalpha beta\n"
    }

    // `§4`'s invariant: a body the definition accepted cannot fail at an instantiation. The same
    // `a + b` is a native instruction for `int` and a call to `Point.add` for a user type.
    "one operator body serves a scalar and a user type" in {
      val src =
        """struct Point
          |    x: int
          |    y: int
          |impl Add for Point
          |    add(self, rhs: Self) -> Self = Point(self.x + rhs.x, self.y + rhs.y)
          |sum[T: Add](a: T, b: T) -> T = a + b
          |print(sum(40, 2))
          |var p = sum(Point(1, 2), Point(30, 9))
          |print(p.x, p.y)""".stripMargin

      run(src) shouldBe "42\n31 11\n"
    }

    "a string satisfies 'Add', which is concatenation" in {
      val src =
        """sum[T: Add](a: T, b: T) -> T = a.add(b)
          |print(sum("four", "two"))""".stripMargin

      run(src) shouldBe "fourtwo\n"
    }

    "'Ord' yields a bool, and the bound licenses the comparison" in {
      val src =
        """smaller[T: Ord](a: T, b: T) -> T = if a.lt(b) then a else b
          |print(smaller(7, 3))
          |print(smaller('z', 'a'))
          |print(smaller("beta", "alpha"))""".stripMargin

      run(src) shouldBe "3\na\nalpha\n"
    }

    "'Eq' reaches further than 'Ord' — a bool is equatable and not ordered" in {
      val src =
        """same[T: Eq](a: T, b: T) -> bool = a.eq(b)
          |print(same(true, true))
          |print(same(true, false))""".stripMargin

      run(src) shouldBe "true\nfalse\n"
    }

    "two bounds on one parameter are both available" in {
      val src =
        """clamp_add[T: Add + Ord](a: T, b: T, cap: T) -> T
          |    var s = a.add(b)
          |    if cap.lt(s) then cap else s
          |print(clamp_add(20, 22, 100))
          |print(clamp_add(20, 22, 30))""".stripMargin

      run(src) shouldBe "42\n30\n"
    }
  }

  "a core-trait method on a scalar is the operator" - {
    // Each arithmetic and bitwise trait, called by name on a literal. A wrong operator in the
    // table would show up here as a wrong number, and the operands are chosen so that no two
    // operators agree on them.
    "each arithmetic and bitwise method computes its own operator" in {
      val src =
        """print(20.add(3))
          |print(20.sub(3))
          |print(20.mul(3))
          |print(20.div(3))
          |print(20.rem(3))
          |print(20.bitand(3))
          |print(20.bitor(3))
          |print(20.bitxor(3))
          |print(20.shl(3))
          |print(20.shr(3))""".stripMargin

      run(src) shouldBe "23\n17\n60\n6\n2\n0\n23\n23\n160\n2\n"
    }

    "the prefix methods negate and complement" in {
      val src =
        """print(20.neg())
          |print(20.not())
          |print(2.5.neg())""".stripMargin

      run(src) shouldBe "-20\n-21\n-2.5\n"
    }

    "the comparison methods are the one each trait requires" in {
      val src =
        """print(3.eq(3), 3.eq(4))
          |print(3.lt(4), 4.lt(3), 3.lt(3))""".stripMargin

      run(src) shouldBe "true false\ntrue false false\n"
    }

    // A membership changes no codegen (`§5`): `a.add(b)` and `a + b` are the same instruction, so
    // the two must agree on a value where a call through anything else could not.
    "a method call and the operator agree" in {
      val src =
        """var a = 2147483647
          |print(a.add(1) == a + 1)
          |print(a.mul(3) == a * 3)""".stripMargin

      run(src) shouldBe "true\ntrue\n"
    }

    "a member of a struct wins over a core-trait method of the same name" in {
      val src =
        """struct Tally
          |    n: int
          |    add(self, k: int) -> int = self.n + k * 100
          |print(Tally(2).add(4))""".stripMargin

      run(src) shouldBe "402\n"
    }
  }

  "a user type carries its own impl" - {
    // `Point` is not a built-in, so its `Add` comes from the `impl` — and the same generic body
    // serves it and a scalar, which is the one dispatch rule of `§3` doing its job.
    "one generic body serves a user type and a scalar alike" in {
      val src =
        """struct Point
          |    x: int
          |    y: int
          |impl Add for Point
          |    add(self, rhs: Self) -> Self = Point(self.x + rhs.x, self.y + rhs.y)
          |sum[T: Add](a: T, b: T) -> T = a.add(b)
          |var p = sum(Point(1, 2), Point(30, 9))
          |print(p.x, p.y)
          |print(sum(40, 2))""".stripMargin

      run(src) shouldBe "31 11\n42\n"
    }

    // `Eq` is a catalog trait, but `Point` is not a built-in, so its membership comes from the
    // `impl` — the two sources of conformance meet at the same bound.
    "a user type's own 'Eq' impl satisfies the bound" in {
      val src =
        """struct Point
          |    x: int
          |    y: int
          |impl Eq for Point
          |    eq(self, rhs: Self) -> bool = self.x == rhs.x && self.y == rhs.y
          |same[T: Eq](a: T, b: T) -> bool = a.eq(b)
          |print(same(Point(1, 2), Point(1, 2)))
          |print(same(Point(1, 2), Point(1, 3)))""".stripMargin

      run(src) shouldBe "true\nfalse\n"
    }

    "an operator on a user type is its trait's method" in {
      val src =
        """struct Point
          |    x: int
          |    y: int
          |impl Add for Point
          |    add(self, rhs: Self) -> Self = Point(self.x + rhs.x, self.y + rhs.y)
          |var p = Point(1, 2) + Point(30, 9)
          |print(p.x, p.y)""".stripMargin

      run(src) shouldBe "31 11\n"
    }

    // Four operators from one method. `>` and `<=` swap the operands, `!=`, `<=` and `>=` negate,
    // so a wrong entry in the derivation table shows up as a flipped answer here.
    "all four ordering operators derive from one 'lt'" in {
      val src =
        """struct Money
          |    cents: int
          |impl Ord for Money
          |    lt(self, rhs: Self) -> bool = self.cents < rhs.cents
          |var a = Money(1)
          |var b = Money(2)
          |print(a < b, b < a, a < a)
          |print(a > b, b > a, a > a)
          |print(a <= b, b <= a, a <= a)
          |print(a >= b, b >= a, a >= a)""".stripMargin

      run(src) shouldBe
        "true false false\nfalse true false\ntrue false true\nfalse true true\n"
    }

    // The other half of the same claim, and the half that has to be checked rather than observed:
    // `library/core.md § What is in it` says the catalog stays flat and being `Ord` does not imply `Eq`. So a type ordered by
    // one `lt` gets all four comparisons above and no `==` at all — which is what lets an ordering
    // key be written for a value whose equality would have meant something else entirely.
    "and being ordered does not make a type equatable" in {
      val src =
        """struct Money
          |    cents: int
          |impl Ord for Money
          |    lt(self, rhs: Self) -> bool = self.cents < rhs.cents
          |print(Money(1) == Money(2))""".stripMargin

      err(src) should include("'==' is not defined for Money")
    }

    // **An `Option` is equatable exactly when its payload is**, which is one `impl` in the library
    // rather than a membership the compiler hands out — the block below is the shape of it, and the
    // library writes the same one for its own two enums. `Some` is never equal to `None`, and both
    // directions are asked, since a comparison reading the left operand's variant alone answers one
    // of them correctly by accident.
    "and an Option is equatable when what it holds is" in {
      run("""var a: Option[u8] = Some(3u8)
            |var b: Option[u8] = Some(3u8)
            |var c: Option[u8] = Some(4u8)
            |var n: Option[u8] = None
            |print(a == b, a == c, a == n, n == a, n == n)
            |""".stripMargin) shouldBe "true false false false true\n"
    }

    /** **The literal spelling, which is the one a reader reaches for first.** `Some(166)` written
     * beside an `Option[usize]` reads as an `Option[int]` on its own, and one does not become the
     * other by any conversion — so a comparison written the natural way was refused for a difference
     * nobody put there, while the same comparison between two *named* options worked.
     *
     * An operand is settled by its neighbour here exactly as an argument is settled by the solution
     * at a call (`reference/expressions.md § Closures`): whichever side has a type of its own
     * supplies it, and the other is read again against it. `None` takes the same road one step
     * earlier — it has no reading of its own at all, so it waits for the neighbour rather than
     * being asked first.
     */
    "and the literal spelling of either side reads at the other's type" in {
      run("""var a: Option[usize] = Some(166)
            |var n: Option[usize] = None
            |print(a == Some(166), a == Some(167), a == n, n == None, a != n)
            |""".stripMargin) shouldBe "true false false true true\n"
    }

    "a result reads the same way, at both of its payloads" in {
      run("""var ok: Result[usize, usize] = Ok(3)
            |var bad: Result[usize, usize] = Err(3)
            |print(ok == Ok(3), bad == Err(3), ok == bad, ok == Ok(4))
            |""".stripMargin) shouldBe "true true false false\n"
    }

    // Reading the neighbour is not agreeing with it. An operand that cannot be read at the other
    // side's type is refused in the terms it always was.
    "an operand that cannot be read at the other's type is still refused" in {
      err("""var a: Option[usize] = Some(166)
            |print(a == "no")
            |""".stripMargin) should include("needs matching types")
    }

    // And the bound is on the **payload**, so an option of something incomparable is refused — at
    // the comparison, naming the half that is missing rather than the option.
    "but not when it holds something that is not" in {
      err("""struct Opaque
            |    n: int
            |var a: Option[Opaque] = Some(Opaque(1))
            |var b: Option[Opaque] = Some(Opaque(2))
            |print(a == b)
            |""".stripMargin) should include("asks 'sysl.Eq' of Opaque, which does not implement it")
    }

    // The same row written for an enum of the program's own, which is what the library's block looks
    // like from the outside — and the one a program has to write for itself, since `Eq` and `Option`
    // are both the library's and an `impl` for a generic type covers every instantiation at once, so
    // there is no `Option[MyType]` to give a block of one's own a home.
    "and the row itself is writable for an enum of its own" in {
      run("""enum Maybe[T]
            |    Just(v: T)
            |    Nothing
            |impl[T: Eq] Eq for Maybe[T]
            |    eq(self, rhs: Maybe[T]) -> bool = self match
            |        Just(a) -> rhs match
            |            Just(b) -> a == b
            |            Nothing -> false
            |        Nothing -> rhs match
            |            Just(b) -> false
            |            Nothing -> true
            |var a: Maybe[u8] = Just(3u8)
            |var b: Maybe[u8] = Just(4u8)
            |var c: Maybe[u8] = Nothing
            |print(a == a, a == b, a == c, c == c)
            |""".stripMargin) shouldBe "true false false true\n"
    }

    /** A **simple** enum — every variant dataless — is `Eq` by the rule the open `iN` family is, and
     * for the same reason: the value *is* its discriminant, so there is one thing equality could
     * mean and no finite list an `impl` could be written over.
     *
     * The workarounds it replaces were all short, which is the tell rather than the reassurance:
     * a hand-written `eq` over a conversion, an `int(a) == int(b)`, or a two-armed `match` answering
     * one question. Every enum-shaped API wrote one of them.
     */
    "a simple enum is equatable, and a data enum is not" - {

      "two of them compare by their discriminant" in {
        run("""enum Colorspace
              |    Srgb
              |    Linear
              |var a = Srgb
              |var b = Linear
              |print(a == a, a == b, a != b, b != b)""".stripMargin) shouldBe
          "true false true false\n"
      }

      // The comparison happens at the storage type the annotation named, not at `int` — a width the
      // compare was emitted at wrongly would answer for a discriminant the enum does not have.
      "at the width the ': iN' annotation gave it" in {
        run("""enum Small: u8
              |    A = 1
              |    B = 255
              |var a = A
              |var b = B
              |print(a == b, b == b)""".stripMargin) shouldBe "false true\n"
      }

      // The membership has to satisfy the `Eq` **bound**, not merely make the token typecheck —
      // which is the difference between this and a special case in the operator path. `satisfies`
      // consults the same predicate, so a bounded generic takes one.
      "and it satisfies an 'Eq' bound, so a bounded generic takes one" in {
        run("""enum Colorspace
              |    Srgb
              |    Linear
              |same[T: Eq](a: T, b: T) -> bool = a == b
              |print(same(Srgb, Srgb), same(Srgb, Linear))""".stripMargin) shouldBe "true false\n"
      }

      // Structural equality over payloads needs every payload type to be `Eq` itself, which is a
      // real feature and a different one. `simple` is the line, and it is the same line the lowering
      // already draws.
      "while an enum that carries data is refused, as it was" in {
        err("""enum Shape
              |    Dot
              |    Line(n: int)
              |print(Dot == Dot)""".stripMargin) should include("'==' is not defined for Shape")
      }

      // `reference/expressions.md § Operator dispatch`'s rule about a built-in's memberships,
      // reaching the first type a *program* declares. Refused rather than silently ignored — the
      // operator lowers to the compare whatever the block says, so a block left standing would be
      // dead code that reads as the thing being called.
      "and writing the 'impl' by hand is refused, saying why the type is already a member" in {
        val e = err("""enum Colorspace
                      |    Srgb
                      |    Linear
                      |impl Eq for Colorspace
                      |    eq(self, rhs: Colorspace) -> bool = int(self) == int(rhs)
                      |print(Srgb == Srgb)""".stripMargin)

        e should include("already implements")
        e should include("its value is its discriminant")
      }

      // The same block over an enum that carries something is untouched, which is what keeps the
      // refusal about the membership rather than about enums.
      "while a data enum may still be given one by hand" in {
        run("""enum Shape
              |    Dot
              |    Line(n: int)
              |impl Eq for Shape
              |    eq(self, rhs: Shape) -> bool = self match
              |        Dot -> rhs match
              |            Dot -> true
              |            Line(_) -> false
              |        Line(a) -> rhs match
              |            Dot -> false
              |            Line(b) -> a == b
              |print(Dot == Dot, Line(1) == Line(1), Line(1) == Line(2))""".stripMargin) shouldBe
          "true true false\n"
      }
    }

    "both equality operators derive from one 'eq'" in {
      val src =
        """struct Money
          |    cents: int
          |impl Eq for Money
          |    eq(self, rhs: Self) -> bool = self.cents == rhs.cents
          |var a = Money(1)
          |print(a == Money(1), a == Money(2))
          |print(a != Money(1), a != Money(2))""".stripMargin

      run(src) shouldBe "true false\nfalse true\n"
    }

    "a prefix operator on a user type is its trait's method" in {
      val src =
        """struct Point
          |    x: int
          |    y: int
          |impl Neg for Point
          |    neg(self) -> Self = Point(-self.x, -self.y)
          |var p = -Point(3, 4)
          |print(p.x, p.y)""".stripMargin

      run(src) shouldBe "-3 -4\n"
    }

    "a catalog method reaches its receiver through a pointer" in {
      val src =
        """var n = 40
          |var p = &n
          |print(p.add(2))""".stripMargin

      run(src) shouldBe "42\n"
    }
  }

  /** A comparison chain and a compound assignment each use one operand **twice from a single
   * evaluation**, so what they need from a trait-supplied operator is a method to apply to a value
   * already in hand rather than a call built over the operand's expression. These pin that: an
   * operand that announces itself must announce itself exactly once.
   */
  "an operand-sharing form evaluates each operand once" - {

    "a chain of user types compares each neighbouring pair" in {
      val src =
        """struct M
          |    v: int
          |impl Ord for M
          |    lt(self, rhs: Self) -> bool = self.v < rhs.v
          |var a = M(1)
          |var b = M(2)
          |var c = M(3)
          |print(a < b < c, a < c < b, c < b < a)""".stripMargin

      run(src) shouldBe "true false false\n"
    }

    // The middle operand is compared against both its neighbours, and announces itself once.
    "the middle operand of a dispatched chain is evaluated once" in {
      val out = run(tagged + "print(tag(1) < tag(2) < tag(3))")

      out shouldBe "1\n2\n3\ntrue\n"
    }

    // Left to right, and no further: the chain stops at the comparison that failed, so the third
    // operand is never reached — the same short-circuit `01` specifies for a chain of scalars.
    "a dispatched chain short-circuits at the first failure" in {
      val out = run(tagged + "print(tag(2) < tag(1) < tag(3))")

      out shouldBe "2\n1\nfalse\n"
    }

    // `>` derives as `lt(b, a)`, which swaps the *values* at the call and not the expressions that
    // produced them — so the left operand is still evaluated first, as it is for a scalar.
    "a derived operator does not reorder evaluation" in {
      val out = run(tagged + "print(tag(1) > tag(2))")

      out shouldBe "1\n2\nfalse\n"
    }

    /** `§2` says the four derived comparisons are sound for a total order and unsound for a partial
      * one, that the scalars are routed around the derivation so a float keeps IEEE's answers, and
      * that the discrepancy is therefore **reachable only by a user type whose own `lt` is partial**
      * — something to construct deliberately rather than to stumble into. This constructs it, and it
      * is worth pinning precisely because it is the documented consequence of a decision: a change
      * that quietly derived the scalars the same way would show up here first.
      *
      * `n < one` and `n > one` are both false, as IEEE says. `n <= one` is `!lt(one, n)` and `n >= one`
      * is `!lt(n, one)`, so both come back **true**, where a float would say false.
      */
    "a user type whose 'lt' is partial reaches the discrepancy the derivation has" in {
      val src =
        """struct Maybe
          |    v: f64
          |impl Ord for Maybe
          |    lt(self, rhs: Self) -> bool = self.v < rhs.v
          |var n = Maybe(0.0 / 0.0)
          |var one = Maybe(1.0)
          |print(n < one, n > one, n <= one, n >= one)""".stripMargin

      run(src) shouldBe "false false true true\n"
    }

    // The other half of the same claim, and the reason the scalars are routed around: the float's own
    // comparisons answer false at every one of the four, which negating `lt` could not have produced.
    "while the float the type wraps answers false to all four" in {
      run("var n = 0.0 / 0.0\nprint(n < 1.0, n > 1.0, n <= 1.0, n >= 1.0)") shouldBe
        "false false false false\n"
    }

    // Two middle operands, each shared by the pair of comparisons around it, and the links are not
    // all the same operator — `<=` is derived and `<` is not, so a chain has to carry the two
    // derivations independently.
    "a longer chain shares every middle operand" in {
      val out = run(tagged + "print(tag(1) < tag(2) <= tag(2) < tag(3))")

      out shouldBe "1\n2\n2\n3\ntrue\n"
    }

    "compound assignment on a user type updates through its 'Add'" in {
      val src =
        """struct M
          |    v: int
          |impl Add for M
          |    add(self, rhs: Self) -> Self = M(self.v + rhs.v)
          |var a = M(1)
          |a += M(2)
          |a += M(39)
          |print(a.v)""".stripMargin

      run(src) shouldBe "42\n"
    }

    // The place is read once and written once, whatever the expression naming it costs to evaluate.
    "compound assignment reads its place once" in {
      val src =
        """struct M
          |    v: int
          |impl Add for M
          |    add(self, rhs: Self) -> Self = M(self.v + rhs.v)
          |var a = [M(1), M(10)]
          |at(i: int) -> int
          |    print(i)
          |    i
          |a[at(1)] += M(32)
          |print(a[0].v, a[1].v)""".stripMargin

      run(src) shouldBe "1\n1 42\n"
    }

    // The whole point of definition-checked bounds: one body, and the chain in it works for every
    // type the bound admits — a scalar's instruction and a user type's method alike.
    "a chain inside a bounded generic serves both a scalar and a user type" in {
      val src =
        """between[T: Ord](lo: T, x: T, hi: T) -> bool = lo < x < hi
          |struct M
          |    v: int
          |impl Ord for M
          |    lt(self, rhs: Self) -> bool = self.v < rhs.v
          |print(between(1, 5, 9), between(5, 1, 9))
          |print(between(M(1), M(5), M(9)), between(M(5), M(1), M(9)))""".stripMargin

      run(src) shouldBe "true false\ntrue false\n"
    }

    "a compound assignment inside a bounded generic serves both" in {
      val src =
        """total[T: Add](a: T, b: T, c: T) -> T
          |    var acc = a
          |    acc += b
          |    acc += c
          |    acc
          |struct M
          |    v: int
          |impl Add for M
          |    add(self, rhs: Self) -> Self = M(self.v + rhs.v)
          |print(total(1, 2, 3))
          |print(total(M(10), M(20), M(12)).v)""".stripMargin

      run(src) shouldBe "6\n42\n"
    }

    // A chain whose operands own heap storage: each is released once, by the region the codegen
    // ladder opened for it, however early the chain exits.
    "a dispatched chain over reference-carrying operands neither leaks nor frees twice" in {
      val src =
        """struct Tag
          |    name: string
          |impl Ord for Tag
          |    lt(self, rhs: Self) -> bool = self.name < rhs.name
          |var n = 0
          |while n < 1000
          |    var a = Tag(str(n))
          |    if Tag("a") < a < Tag("z") then n += 1 else n += 1
          |print(n)""".stripMargin

      run(src) shouldBe "1000\n"
    }
  }

  /** An operand-sharing form reaches the trait's method with the value already in a register, which
   * is a **second route to the same call** — and a route says nothing about the convention the
   * callee was defined under. Past `Layout.DirectBytes` a parameter is an address, so the value has
   * to be staged into storage and the address handed over, exactly as an ordinary call does for an
   * argument it cannot take the address of.
   *
   * Nothing diagnoses getting that wrong. A `call` naming the aggregate where the definition names
   * a pointer assembles, links, and reads the first word of the value as the address it was
   * promised — so the shape of the failure is a fault inside a method whose body touches nothing.
   * A string is three words here, which puts six of them one field past the boundary and five of
   * them exactly on it.
   */
  "an operator whose operand is past the by-value boundary hands over an address" - {

    /** The reduction: an enum whose widest variant carries six counted fields, compared at a
     * variant that carries none. `eq` is written to answer without reading either operand, so
     * anything but `true` is the handover rather than the comparison.
     */
    val wide =
      """enum Wide
        |    Empty
        |    Many(a: string, b: string, c: string, d: string, e: string, f: string)
        |impl Eq for Wide
        |    eq(self, rhs: Wide) -> bool = true
        |""".stripMargin

    "the same method answers whether it is reached by name or by operator" in {
      run(wide + "var w: Wide = Empty\nprint(w.eq(w))\nprint(w == w)") shouldBe "true\ntrue\n"
    }

    "and it reads the operands it was handed, not the words underneath them" in {
      val src =
        """enum Wide
          |    Empty
          |    Many(a: string, b: string, c: string, d: string, e: string, f: string)
          |impl Eq for Wide
          |    eq(self, rhs: Wide) -> bool
          |        tail(self) == tail(rhs)
          |tail(w: Wide) -> string
          |    w match
          |        Empty -> ""
          |        Many(a, b, c, d, e, f) -> f
          |var x: Wide = Many("a", "b", "c", "d", "e", "f")
          |var y: Wide = Many("a", "b", "c", "d", "e", "z")
          |print(x == x, x == y, x != y)""".stripMargin

      run(src) shouldBe "true false true\n"
    }

    "one field further past it is no different" in {
      val src =
        """struct Seven
          |    a: string
          |    b: string
          |    c: string
          |    d: string
          |    e: string
          |    f: string
          |    g: string
          |impl Eq for Seven
          |    eq(self, rhs: Self) -> bool = self.g == rhs.g
          |var s = Seven("1", "2", "3", "4", "5", "6", "7")
          |var t = Seven("1", "2", "3", "4", "5", "6", "x")
          |print(s == s, s == t, s != t)""".stripMargin

      run(src) shouldBe "true false true\n"
    }

    // A payload of mixed widths rather than six of one, so the boundary is crossed by a shape no
    // single field count would have predicted.
    "a mixed payload past the boundary behaves the same" in {
      val src =
        """struct Mixed
          |    name: string
          |    cells: [12]i64
          |    tag: int
          |    flag: bool
          |    ratio: f64
          |    note: string
          |impl Eq for Mixed
          |    eq(self, rhs: Self) -> bool = self.tag == rhs.tag && self.note == rhs.note
          |var m = Mixed("m", [0; 12], 7, true, 1.5, "n")
          |var n = Mixed("m", [0; 12], 8, true, 1.5, "n")
          |print(m == m, m == n, m != n)""".stripMargin

      run(src) shouldBe "true false true\n"
    }

    // The operand reaches the operator through a type parameter, so nothing at the call site names
    // the width the boundary is about — the instantiation is where it has to be got right.
    "a generic function over an 'Eq' bound compares one the same way" in {
      val src =
        """struct Wide6
          |    a: string
          |    b: string
          |    c: string
          |    d: string
          |    e: string
          |    f: string
          |impl Eq for Wide6
          |    eq(self, rhs: Self) -> bool = self.f == rhs.f
          |same[T: Eq](x: T, y: T) -> bool = x == y
          |differ[T: Eq](x: T, y: T) -> bool = x != y
          |var p = Wide6("1", "2", "3", "4", "5", "6")
          |var q = Wide6("1", "2", "3", "4", "5", "z")
          |print(same(p, p), same(p, q), differ(p, q))""".stripMargin

      run(src) shouldBe "true false true\n"
    }

    // `Ord` reaches `dispatchValue` by the same route `Eq` does, and a chain reaches it twice over
    // one shared evaluation — which is the form the whole path exists for.
    "'<' and a chain of them cross it too" in {
      val src =
        """struct Wide6
          |    a: string
          |    b: string
          |    c: string
          |    d: string
          |    e: string
          |    f: string
          |impl Ord for Wide6
          |    lt(self, rhs: Self) -> bool = self.f < rhs.f
          |var p = Wide6("1", "2", "3", "4", "5", "1")
          |var q = Wide6("1", "2", "3", "4", "5", "5")
          |var r = Wide6("1", "2", "3", "4", "5", "9")
          |print(p < q, r < q, p >= q, p < q < r, r < q < p)""".stripMargin

      run(src) shouldBe "true false false true false\n"
    }

    // The other half of the same boundary: a compound assignment's method **returns** a large value,
    // which comes back through an out-pointer rather than as a result.
    "a compound assignment whose method returns a large value still assigns it" in {
      val src =
        """struct Wide6
          |    a: string
          |    b: string
          |    c: string
          |    d: string
          |    e: string
          |    f: string
          |impl Add for Wide6
          |    add(self, rhs: Self) -> Self = Wide6(self.a + rhs.a, "b", "c", "d", "e", self.f + rhs.f)
          |var p = Wide6("1", "2", "3", "4", "5", "6")
          |var q = Wide6("x", "2", "3", "4", "5", "y")
          |p += q
          |print(p.a, p.f)""".stripMargin

      run(src) shouldBe "1x 6y\n"
    }

    // The boundary has two sides, and one that could be anywhere is not a boundary. Five strings
    // are exactly `Layout.DirectBytes`, so this pair still crosses the call as a value.
    "while an operand that stops short of it is still passed as a value" in {
      val src =
        """struct Narrow5
          |    a: string
          |    b: string
          |    c: string
          |    d: string
          |    e: string
          |impl Eq for Narrow5
          |    eq(self, rhs: Self) -> bool = self.e == rhs.e
          |var p = Narrow5("1", "2", "3", "4", "5")
          |var q = Narrow5("1", "2", "3", "4", "z")
          |print(p == p, p == q, p != q)""".stripMargin

      run(src) shouldBe "true false true\n"
    }

    // A large operand that carries counted storage, driven hard enough that a count taken once too
    // often or once too seldom shows up as a leak or a double free rather than a wrong answer.
    "and one carrying references is counted exactly once either way" in {
      val src =
        """struct Wide6
          |    a: string
          |    b: string
          |    c: string
          |    d: string
          |    e: string
          |    f: string
          |impl Eq for Wide6
          |    eq(self, rhs: Self) -> bool = self.f == rhs.f
          |var n = 0
          |while n < 1000
          |    var p = Wide6(str(n), "b", "c", "d", "e", str(n))
          |    var q = Wide6(str(n), "b", "c", "d", "e", "z")
          |    if p == q then n += 2 else n += 1
          |print(n)""".stripMargin

      run(src) shouldBe "1000\n"
    }
  }
}
