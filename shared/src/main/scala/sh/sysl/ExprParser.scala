package sh.sysl

/** The expression grammar: the precedence ladder, the postfix tail, and every literal form.
 *
 * The ladder is the whole of `01`'s precedence table written out as one rule per level, loosest
 * first — assignment, `||`, `&&`, chained comparison, range, `|`, `^`, `&`, additive,
 * multiplicative-and-shift, prefix, postfix. Each level is `chainl1` over the next, which iterates
 * rather than left-recurses, so associativity falls out of the combinator and the grammar stays
 * linear under packrat.
 *
 * Two levels are deliberately not C's. Comparison **chains** rather than associates, so `a < b < c`
 * is one node and not a comparison against a `bool`. Shift binds like multiplication rather than
 * below the comparisons, which is the correction that makes `a & 1 << n` mean what it looks like.
 */
trait ExprParser extends SyslParserBase {

  // --- expressions ---------------------------------------------------------------------

  /** `if` and `match` are expressions (they yield the taken branch's value), so they sit at
   * the top of the grammar — an ordinary operand everywhere an expression is expected.
   *
   * `match` is **postfix**, as Scala's is, so it comes last: it reads an ordinary expression and
   * then looks for the keyword, which is what makes `x match` and `a + b match` both name the
   * value they are written after. An operand with no `match` behind it is that operand, so this
   * alternative is also the ordinary fall-through to `assignment`.
   */
  lazy val expression: PackratParser[Expr] =
    describe("expression")(
      at(lambda | quantifier | ifExpr | whileExpr | doWhileExpr | loopExpr | forExpr | matchExpr),
    )

  /** `x -> x + 1` — a closure literal (`reference/expressions.md § Closures`).
   *
   * It sits at the top of the expression grammar because its body extends as far to the right as an
   * expression can: `x -> x + 1` is a closure over the sum, not a closure over `x` added to `1`.
   *
   * Nothing else in the grammar begins with a name or a parenthesized list and then an arrow, so the
   * commitment is the `->` and the alternatives below are reached by an ordinary backtrack. A match
   * arm is not a competitor even though it is written with the same token: an arm's left side is a
   * *pattern*, parsed by its own production, and only the arm's body is an expression.
   */
  protected lazy val lambda: PackratParser[Expr] =
    lambdaParams ~ (op("->") ~> lambdaBody) ^^ { case ps ~ b => Lambda(ps, b) }

  /** One parameter with no parentheses, or a parenthesized list of them — including the empty list,
   * which is the one arity that has nowhere else to be written (`reference/expressions.md §
   * Closures`).
   */
  protected lazy val lambdaParams: Parser[List[LambdaParam]] =
    op("(") ~> commaList(lambdaParam) <~ op(")") |
      at(ident ^^ (n => LambdaParam(n, None))) ^^ (List(_))

  /** A closure's parameter: a name, and a type only where there is nothing to infer one from. The
   * annotation is written inside the parentheses and nowhere else, so `x: int -> …` is not a second
   * spelling of `(x: int) -> …` waiting to disagree with it.
   */
  protected lazy val lambdaParam: Parser[LambdaParam] =
    at(
      ident ~ (op(":") ~> typeRef) <~ noDefault ^^ { case n ~ t => LambdaParam(n, Some(t)) } |
        ident ^^ (n => LambdaParam(n, None)),
    )

  /** A closure's parameter declares no default (`reference/declarations.md § Default parameters and
   * named arguments`). It is matched through the `Fn` trait, which carries types and no names, so a
   * call through one has nothing to read a default out of — the same absence that stops a closure
   * being called by name. Said here rather than left to the "')' expected" a grammar with no place
   * for one would give.
   *
   * Reached only where the parameter wrote a **type**, and that is what makes saying it safe. A
   * parenthesized list is read speculatively — `(x = 1)` is an argument that is a store, and `(a, b)`
   * is a tuple — so a refusal here that could fire on one of those would refuse it before the rule
   * that was going to accept it ever ran. `x: int` is an expression in no reading, so having got
   * that far the list is a closure's parameters and nothing else.
   */
  protected lazy val noDefault: Parser[Unit] =
    op("=") ~> err("a closure's parameter declares no default — a call reaches a closure through " +
      "'Fn', which carries the types and not the names, so there would be nothing at the call to " +
      "fill one from. Give the value a name of its own and capture it") |
      success(())

  /** A closure's body, which is a function's body without the `=`: an expression, or an indented
   * block whose trailing expression is the value.
   *
   * The expression form is a statement-level position and closes a placeholder there
   * (`reference/expressions.md § _ — a parameter with the name left out`), so `x -> _ + 1` is a
   * closure yielding a closure rather than one arrow with two parameters. That is also what lets
   * [[Placeholders.free]] stop at a [[Lambda]]: one can hold no placeholder that is still looking
   * for its parameter list.
   *
   * The block is reached through `blockAhead` rather than tried outright, which is what stops a
   * closure whose body was forgotten — `val g = x ->` and then the next statement — being answered
   * with `indent expected` against a line the writer never meant as a body.
   */
  protected lazy val lambdaBody: PackratParser[List[Stmt]] =
    blockAhead ~> suite | expression ^^ (e => List(ExprStmt(Placeholders.lift(e)).setPos(e.pos)))

  protected def binOp(sym: String): Parser[(Expr, Expr) => Expr] =
    op(sym) ^^^ ((l: Expr, r: Expr) => Binary(sym, l, r))

  /** Assignment is right-associative and lowest precedence, so it recurses on its right. The right
   * side is a full `expression`, so a control-flow expression may sit there — `x = if …`,
   * `total += if …` — in the same tail position `var x = …` already allows one; this does not put
   * those forms into a binary operand, so `1 + if …` still does not parse.
   *
   * `match` needs no such allowance, being postfix: `x = y match` is an assignment whose right side
   * is a match, and `a + b match` matches on the sum, exactly as the same lines read in Scala.
   */
  lazy val assignment: PackratParser[Expr] =
    at(
      logicalOr ~ assignOp ~ expression ^^ { case l ~ o ~ r => Assign(o, l, r) } |
        logicalOr,
    )

  protected def assignOp: Parser[String] =
    op("=") | op("+=") | op("-=") | op("*=") | op("/=") | op("%=") |
      op("&=") | op("|=") | op("^=") | op("<<=") | op(">>=")

  lazy val logicalOr: PackratParser[Expr]  = at(chainl1(logicalAnd, binOp("||")))
  lazy val logicalAnd: PackratParser[Expr] = at(chainl1(isTest, binOp("&&")))

  /** `x is Pat`, `x is not Pat` — a pattern where a condition is wanted (`reference/expressions.md
   * § is — a pattern where a condition is wanted`).
   *
   * It sits **between `&&` and the comparisons**, which is what makes `a is P && b > 0` a chain of
   * two terms rather than an `is` against a conjunction: the subject and the pattern are each held
   * tighter than the `&&` that joins one term to the next. Nothing below it can hold an `is`, so
   * `a is P || q` does not parse as a disjunction of tests — the analyzer is what says why, since a
   * grammar that refused it would have to say "expression expected".
   *
   * The right side is an arm's left side: `|`-alternatives and all. There is nothing for the `|` to
   * be ambiguous with, a pattern having no bitwise operator, so the same spelling means the same
   * thing in both places and neither has to be learned twice.
   *
   * `is` and `not` are soft words: neither is reserved, so both stay usable as names, and this rule
   * is the only place either is read as a keyword.
   */
  protected lazy val isTest: PackratParser[Expr] =
    at(
      comparison ~ opt(isWord ~> opt(notWord) ~ rep1sep(pattern, op("|"))) ^^ {
        case e ~ None           => e
        case e ~ Some(n ~ pats) => IsPattern(e, pats, n.isDefined)
      },
    )

  protected lazy val isWord: Parser[Unit]  = softWord("is")
  protected lazy val notWord: Parser[Unit] = softWord("not")

  /** Comparison chains rather than associates: `a < b < c` becomes one `Compare`. */
  lazy val comparison: PackratParser[Expr] =
    at(
      rangeExpr ~ rep(compareOp ~ rangeExpr) ^^ {
        case first ~ Nil  => first
        case first ~ rest => Compare(first :: rest.map { case _ ~ e => e }, rest.map { case o ~ _ => o })
      },
    )

  protected def compareOp: Parser[String] =
    op("==") | op("!=") | op("<=") | op(">=") | op("<") | op(">")

  /** `..=` is refused **by name**, wherever a range may be written.
   *
   * Rust spells an inclusive range `a..=b`, so a reader arriving from it writes one here once. Left
   * to the ordinary grammar what they get is a message about the open-ended range `a..` that was
   * parsed — in a `for` header, *"a range is only allowed in a 'for' loop, …"*, which is a true
   * sentence about something else and asserts the very thing the reader can see is already so. The
   * `=` that caused it appears nowhere in it.
   *
   * The refusal is an `Error` rather than a `Failure` so it survives the alternations above it: a
   * `Failure` here would be outranked by whichever candidate got furthest, which is how the message
   * about the open-ended range won in the first place. `guard` is what points it at the `..` rather
   * than past the `=`.
   */
  protected def noInclusiveEq: Parser[Nothing] =
    guard(op("..") ~ op("=")) ~> err("'..=' is not a range — inclusive is 'a..b' and exclusive is 'a..<b'")

  protected def rangeOp: Parser[Boolean] = op("..<") ^^^ false | noInclusiveEq | op("..") ^^^ true

  /** Ranges are non-associative and sit below arithmetic, so each end is a `bitOr`. Either
   * end may be omitted (`a..`, `..b`).
   */
  lazy val rangeExpr: PackratParser[Expr] =
    at(
      bitOr ~ opt(rangeOp ~ opt(bitOr)) ^^ {
        case lo ~ None              => lo
        case lo ~ Some(inc ~ hiOpt) => RangeExpr(Some(lo), hiOpt, inc)
      } |
        rangeOp ~ bitOr ^^ { case inc ~ hi => RangeExpr(None, Some(hi), inc) } |
        rangeOp ^^ (inc => RangeExpr(None, None, inc)),
    )

  lazy val bitOr: PackratParser[Expr]  = at(chainl1(bitXor, binOp("|")))
  lazy val bitXor: PackratParser[Expr] = at(chainl1(bitAnd, binOp("^")))
  lazy val bitAnd: PackratParser[Expr] = at(chainl1(additive, binOp("&")))
  lazy val additive: PackratParser[Expr] =
    at(chainl1(multiplicative, binOp("+") | binOp("-")))

  /** Shift binds like multiplication (the deliberate correction to C), so `<<`/`>>` live at
   * this level alongside `* / %`.
   */
  lazy val multiplicative: PackratParser[Expr] =
    at(chainl1(unary, binOp("*") | binOp("/") | binOp("%") | binOp("<<") | binOp(">>")))

  /** The operand every binary level is written over, and so the rule that answers for a token
   * appearing where a value was wanted: `1 +` runs out here rather than at [[expression]], which was
   * entered back at the `1`. Both carry the same name for the same reason.
   */
  lazy val unary: PackratParser[Expr] =
    describe("expression")(
      at(
        (op("-") | op("!") | op("~") | op("*") | op("&")) ~ unary ^^ { case o ~ e => Unary(o, e) } |
          (op("++") | op("--")) ~ unary ^^ { case o ~ e => PreIncDec(o, e) } |
          postfix,
      ),
    )

  lazy val postfix: PackratParser[Expr] =
    at(primary ~ rep(postfixTail) ^^ { case p ~ tails => tails.foldLeft(p)((acc, f) => f(acc)) })

  /** A postfix tail points at *itself* rather than at the operand it extends: a missing field is
   * a complaint about the `.name`, and a bad call is a complaint about the argument list, so the
   * caret belongs on the tail, not back at the start of the receiver.
   */
  protected lazy val postfixTail: PackratParser[Expr => Expr] =
      // One thing in the brackets is a subscript; more than one never was — `xs[a, b]` was a parse
      // error until this read it, so nothing that compiled before changes meaning here. What the
      // comma buys is the *multi-argument* type list `&f[A, B]` (`reference/ffi.md § A function's
      // address`), which the analyzer is the only thing entitled to accept, and it refuses this
      // node everywhere else.
      here ~ (op("[") ~> expression ~ rep(op(",") ~> expression) <~ op("]")) ^^ {
        case p ~ (idx ~ Nil)  => (e: Expr) => Index(e, idx).setPos(p)
        case p ~ (idx ~ more) => (e: Expr) => TypeArgs(e, idx :: more).setPos(p)
      } |
      // **The position is taken after the dot, so a diagnostic points at the member rather than at
      // the punctuation.** Every message about a member — a missing one, a wrong arity, a receiver
      // that may not be written through — names the member, and a caret under the `.` sends the
      // reader looking one column to the left of what was said.
      (op(".") ~> here ~ ident) ^^ { case p ~ n => (e: Expr) => Field(e, n).setPos(p) } |
      (op(".") ~> here ~ (tupleIndex | nestedTupleIndex)) ^^ { case p ~ n => (e: Expr) => Field(e, n).setPos(p) } |
      here ~ (op("::") ~> ident) ^^ { case p ~ n => (e: Expr) => TypeAttr(e, n).setPos(p) } |
      // A call is the exception: what is wrong with `foo(…)` is nearly always `foo` — it does not
      // exist, or it does not take these arguments — so the callee's own position wins, and the `(`
      // is only the fallback for a callee that somehow has none. The call is what absorbs a bare
      // `_` argument, which `argument` deliberately left alone, so the lift happens on the whole
      // call rather than on its parts (`reference/expressions.md § _ — a parameter with the name
      // left out`). A call whose arguments were each big enough to lift where they stood has
      // nothing free left in it, and this hands it straight back.
      here ~ (op("(") ~> commaList(argument) <~ op(")")) ~ opt(trailingBlock) ^^ { case p ~ args ~ blk =>
        (e: Expr) => Placeholders.lift(Call(e, args ::: blk.toList).setPos(e.pos).setPos(p))
      } |
      // `column:` — the block with no argument list in front of it, which is the whole of what a
      // container reads like. It is a tail of its own rather than an `opt` on the one above,
      // because there are no parentheses for that one to have matched.
      here ~ trailingBlock ^^ { case p ~ blk =>
        (e: Expr) => Placeholders.lift(Call(e, List(blk)).setPos(e.pos).setPos(p))
      } |
      // `base with { bg = ACCENT }` — a tail rather than a binary operator, so it binds as tightly
      // as a field selection does and `a + b with { … }` changes `b` rather than the sum. The
      // position is the `with`, which is what the rule about the base being a struct complains at.
      here ~ (withWord ~> withFields) ^^ { case p ~ fs => (e: Expr) => WithExpr(e, fs).setPos(p) } |
      here <~ op("?") ^^ (p => (e: Expr) => TryExpr(e).setPos(p)) |
      here <~ op("++") ^^ (p => (e: Expr) => PostIncDec("++", e).setPos(p)) |
      here <~ op("--") ^^ (p => (e: Expr) => PostIncDec("--", e).setPos(p))

  /** `with` is a soft word: it stays a legal identifier everywhere else, and this rule is the only
   * place it is read as a keyword. Nothing else may follow a value on one line, so consuming it
   * **commits** — the alternatives below report what was expected instead of backtracking and
   * leaving the statement to end at a word the reader can see is part of it.
   */
  protected lazy val withWord: Parser[Unit] = softWord("with")

  /** The braced field list. Braces rather than parentheses because a parenthesised list of
   * `name = value` is already a call's argument list, and this is not a call.
   *
   * A brace suspends the off-side rule until it closes (`reference/lexical.md § Brackets suspend
   * the rule`), so the fields may be written one per line with commas, exactly as an array
   * literal's elements are, and the trailing comma is allowed for the same reason.
   */
  protected lazy val withFields: Parser[List[WithField]] =
    op("{") ~> (commaList1(withField) | emptyWith) <~ op("}") |
      err("'with' takes the fields to change in braces, as 'base with { bg = ACCENT }'")

  /** `base with { }`. The refusal is written **inside** the braces rather than as an alternative to
   * the whole form, and the position is why: an alternative is outranked by whichever candidate got
   * furthest along the line, and a candidate that has consumed the `{` is always further than one
   * that has not. Written here it stands at the same token the field list failed at, which is what
   * lets it win — and what leaves `{ bg }` to the sharper `'=' expected` a token later.
   */
  private def emptyWith: Parser[Nothing] =
    err("'with' changes at least one field — with none, 'base with { }' is the value 'base' already is")

  protected lazy val withField: Parser[WithField] =
    at(ident ~ (op("=") ~> expression) ^^ { case n ~ v => WithField(n, v) })

  /** `f:` and then an indented block — an argument written as layout rather than inside the
   * parentheses (`reference/expressions.md § A trailing block`).
   *
   * **What the block becomes is not decided here**, because the parser does not know the callee's
   * parameter types: a collection parameter reads it as an array of its lines and a callable
   * parameter reads it as a closure over them, and argument binding is the first place a parameter
   * and its argument are known to be a pair. So this builds a [[BlockArg]] and says nothing more.
   *
   * **The colon commits to nothing until the indent is there**, which is what keeps this off ground
   * the language already uses. A contract clause writes its message after one — `requires n > 0:
   * "…"` — and that is an ordinary expression followed by a colon, so a rule that consumed the colon
   * and then insisted would refuse it. The lookahead is [[asOneToken]] for the reason [[blockAhead]]
   * is: a search that crossed a token and found nothing must not leave an expectation behind at the
   * token it crossed, where it would outrank the real mistake by position.
   *
   * A block written on the same line — `f: x` — is therefore not this form and not any other. That
   * is deliberate rather than an omission: the colon is already spoken for on a line, and the whole
   * of what the form buys is the layout.
   */
  protected lazy val trailingBlock: PackratParser[Expr] =
    at(asOneToken(op(":") ~ blockAhead) ~> suite ^^ BlockArg.apply)

  /** `t.0` — a tuple's part, selected by position. It is a `Field` because it *is* one: a tuple's
   * fields are named for their positions, so nothing downstream needs a second form of selection.
   * A suffix (`t.0u8`) is not an index, since what follows the dot names a part rather than
   * denoting a number.
   */
  protected lazy val tupleIndex: Parser[String] =
    accept("tuple index", { case t: lexical.IntLit if t.suffix.isEmpty => t.value.toString })

  /** `t.0.1` — the nested selection that the lexer reads as one number, since `0.1` is a float
   * before it is two indices (`reference/types.md § Tuples`). Fatal rather than a backtrack:
   * nothing else can be meant by a float immediately after a `.`, and the fix is worth naming where
   * it happened.
   */
  protected lazy val nestedTupleIndex: Parser[Nothing] = Parser { in =>
    in.first match
      case t: lexical.FloatLit if t.suffix.isEmpty && t.text.matches("""\d+\.\d+""") =>
        val Array(outer, inner) = t.text.split('.')

        Error(s"'.${t.text}' reads as a number rather than as two tuple indices — " +
          s"write '(x.$outer).$inner' to select part $inner of part $outer", in)
      case _ => Failure("tuple index expected", in)
  }

  lazy val primary: PackratParser[Expr] =
    at(
      floatLit | intLit | charLit | interpLit | cStrLit | strLit | boolLit | nullLit | layoutOf | offsetOf |
        selfExpr |
        placeholderExpr |
        implicitMember |
        identExpr |
        arrayLit |
        op("(") ~> parenTail,
    )

  /** `.red`, `.Circle(3)`, `.make(2)` — a member of the type the context expects, written with
   * that type's name left off (`reference/expressions.md § A leading dot`).
   *
   * **The leading dot is unambiguous wherever an expression may start**, which is what makes this a
   * `primary` rather than a form needing lookahead. A range's `..` and `..<` are single tokens by
   * longest match; a number must begin with a digit, so `.5` was never a literal; and a tuple index
   * is read in the postfix tail, where there is a value to the dot's left. A name follows, for the
   * reason a field's does — what comes after a dot names something rather than denoting it.
   *
   * A **statement** beginning with one parses here and is refused by the analyzer, since a statement
   * expects no type. That is what leaves the leading-dot continuation style for a call chain
   * available: the two forms cannot both be meant on one line, because one of them is never legal.
   */
  protected lazy val implicitMember: PackratParser[Expr] =
    op(".") ~> (ident ^^ ImplicitMember.apply | noLeadingPoint)

  /** `.5` — a fraction written the way C allows and sysl does not, refused by name where it would
   * otherwise be read as this form with the name missing.
   *
   * A number begins with a digit here (`reference/lexical.md § Literals`), so `.5` was always two
   * tokens and always a mistake; what changes is that the dot now belongs to a form, and
   * "identifier expected" points at the `5` while saying nothing about the fraction the reader
   * wrote. Nothing else in the grammar puts a number straight after a dot at the start of an
   * expression — a tuple index is read in the postfix tail, where a value is to its left — so
   * consuming one commits.
   */
  private lazy val noLeadingPoint: Parser[Nothing] =
    guard(floatLit | intLit) ~> err("a number is written with a digit before the point — '0.5' " +
      "rather than '.5'")

  /** `sizeof(T)` and `alignof(T)` — the two forms whose operand is a type (`reference/memory.md §
   * Reinterpreting storage`).
   *
   * They are read here rather than left to look like calls because a call's argument list holds
   * expressions, and `sizeof(*Node)` would parse as a dereference of a name. Both words are reserved,
   * so nothing else can be meant by one and the parentheses can be insisted on.
   */
  protected lazy val layoutOf: PackratParser[Expr] =
    (op("sizeof") ^^^ "sizeof" | op("alignof") ^^^ "alignof") >> { what =>
      (op("(") ~> typeRef <~ op(")")) ^^ (t => LayoutOf(what, t)) |
        err(s"'$what' takes a type in parentheses, as '$what(int)' — there is no form that takes a " +
          "value, since a value's type is what would be measured anyway")
    }

  /** `offsetof(T, field)` — the third form whose first operand is a type.
   *
   * The field is a **name** rather than an expression for the same reason the type is not one: there
   * is no value here to select from, so `p.x` would have nothing to be the `p` of. That also makes
   * the comma unambiguous, since a type is read by `typeRef` and stops at it.
   *
   * The refusal is written **twice**, once at each place a token may be missing, rather than once
   * after the whole form. A message raised after a form that got further along the line is never
   * reported — the failure the form left behind outranks it by position — so a single one at the end
   * would have covered only `offsetof` followed by nothing at all, and `offsetof(Header)` would have
   * been answered with `',' expected`.
   */
  protected lazy val offsetOf: PackratParser[Expr] =
    op("offsetof") ~> {
      (op("(") ~> typeRef ~ (op(",") ~> ident | offsetOfForm) <~ op(")")) ^^ {
        case t ~ f => OffsetOf(t, f)
      } | offsetOfForm
    }

  private def offsetOfForm: Parser[Nothing] =
    err("'offsetof' takes a struct type and a field name in parentheses, as 'offsetof(Header, length)'")

  /** A comma-separated list that may end in a comma.
   *
   * Every such list in sysl is bracketed, and a bracket suspends the off-side rule until it closes
   * (`reference/lexical.md § Brackets suspend the rule`), so a list is free to span lines. That is
   * what makes the trailing comma worth having rather than a curiosity: with one element per line,
   * the last line stops being different from the others, so an element can be added, removed or
   * reordered without touching its neighbour, and a diff shows the line that changed and no other.
   *
   * The comma is optional only *after* an element, never instead of one — `[,]` and `f(,)` stay
   * errors. That is why the empty case is a separate alternative rather than `repsep` with an
   * optional comma hung off it, which would have accepted both.
   */
  /** One argument at a call: an ordinary expression, or `name = value` standing at the parameter it
   * names rather than at the one its position would give it (`reference/declarations.md § Default
   * parameters and named arguments`).
   *
   * `name = value` is also a legal expression — assignment yields the value stored — so the two
   * readings collide and the named argument is the one taken. It is tried first, and only where the
   * name is a bare identifier with an `=` after it: `p.x = 1` and `b[i] = v` are stores as they
   * always were, since neither is a name a parameter list could have written. A store to a plain
   * variable is still reachable in an argument by parenthesizing it, and a name matching no
   * parameter is refused by the analyzer rather than quietly falling back to the store.
   */
  protected lazy val argument: PackratParser[Expr] =
    (at(ident ~ (op("=") ~> expression) ^^ { case n ~ v => NamedArg(n, v) }) |
      // `xs...` — this argument is already the slice a `...T` parameter collects. It is read here
      // rather than as a postfix operator because that is what it is: a statement about how one
      // argument is bound, which is the same category `name = value` above belongs to.
      at(expression <~ op("...") ^^ Spread.apply) |
      expression) ^^ Placeholders.liftArg

  protected def commaList[T](p: Parser[T]): Parser[List[T]] = commaList1(p) | success(Nil)

  protected def commaList1[T](p: Parser[T]): Parser[List[T]] = rep1sep(p, op(",")) <~ opt(op(","))

  /** `self` is reserved, so it never lexes as an identifier; inside a method body it reads as an
   * ordinary name that the analyzer resolves to the receiver binding, and is undefined elsewhere.
   *
   * **`^^` rather than `^^^`, and every constant-yielding rule below is written the same way for
   * the same reason.** `^^^` evaluates its argument once and hands the *same* object back on every
   * application, so one `Ident("self")` would serve every `self` in the file — and since `setPos`
   * keeps the first position it is given, every one of them would report where the first was
   * written. A function makes a node per parse, which is what a position belongs to.
   */
  protected lazy val selfExpr: Parser[Expr] = op("self") ^^ (_ => Ident("self"))

  /** `[a, b, c]` — an array literal, or `[v; n]` — an array of `n` copies of one value. A leading
   * `[` is unambiguous in operand position, since a subscript is a postfix tail on something
   * already parsed, and the two forms separate on the token after the first expression.
   */
  protected lazy val arrayLit: PackratParser[Expr] =
    op("[") ~> expression ~ (op(";") ~> expression) <~ op("]") ^^ { case v ~ n => ArrayFill(v, n) } |
      op("[") ~> commaList(expression) <~ op("]") ^^ ArrayLit.apply

  /** After `(`: `)` is unit, one expression is a grouping, more are a tuple.
   *
   * The group is one of the three boundaries a placeholder closes at (`reference/expressions.md § _
   * — a parameter with the name left out`), and it is the one a program reaches for when the other
   * two fall in the wrong place: `(_ + 1) * 2` multiplies the closure rather than closing over the
   * product. A tuple is lifted whole, since the parentheses that delimit it are the same ones.
   */
  protected lazy val parenTail: PackratParser[Expr] =
    op(")") ^^ (_ => UnitLit()) |
      expression ~ rep(op(",") ~> expression) <~ op(")") ^^ {
        case e ~ Nil  => Placeholders.lift(e)
        case e ~ more => Placeholders.lift(Tuple(e :: more).setPos(e.pos))
      }

  protected lazy val intLit: Parser[Expr] =
    accept("integer literal", { case t: lexical.IntLit => IntLit(t.value, t.suffix) })

  protected lazy val floatLit: Parser[Expr] =
    accept("float literal", { case t: lexical.FloatLit => FloatLit(t.text, t.suffix) })

  protected lazy val charLit: Parser[Expr] =
    accept("character literal", { case t: lexical.CharLit => CharLit(t.codepoint) })

  protected lazy val strLit: Parser[Expr] =
    accept("string literal", { case t: lexical.StrLit => StrLit(t.value) })

  protected lazy val cStrLit: Parser[Expr] =
    accept("C string literal", { case t: lexical.CStrLit => CStrLit(t.value) })

  /** An interpolated string desugars to the concatenation of its literal segments with each
   * embedded expression rendered by `str`: `s"a${e}b"` becomes `"a" + str(e) + "b"`. The embedded
   * source is parsed here as an ordinary expression — so it may itself interpolate — and an empty
   * literal segment is dropped, since it is the identity under `+`.
   */
  protected lazy val interpLit: Parser[Expr] = Parser { in =>
    in.first match
      case t: lexical.StrInterp =>
        desugarInterp(t, spanOf(in, in.rest)) match
          case Right(e) => Success(e, in.rest)
          // A malformed embedded expression has no other reading, so the failure is fatal rather
          // than a cue to backtrack — that keeps the real message instead of a generic one from
          // whatever alternative the enclosing rule tries next.
          case Left(msg) => Error(msg, in)
      case _ => Failure("string interpolation expected", in)
  }

  /** The concatenation an interpolated string stands for, with every node in it positioned.
   *
   * **Nothing here is written in the source, so nothing here would have a position unless one is
   * given.** Only the outermost node of a rule's result is stamped, by `at` around `primary`, so a
   * chain built out of a dozen `StrLit`s, `Ident`s, `Call`s and `Binary`s would carry exactly one
   * span and the rest would report nowhere — which is how an analyzer complaint about a hole's type
   * ends up with no caret at all. The span they are all given is `whole`, the literal itself: it is
   * what the reader wrote and what they would edit, and the tree holds no finer position for a part
   * of a token. Nodes the *sub-parse* built keep their own positions, which point into the hole.
   */
  protected def desugarInterp(t: lexical.StrInterp, whole: Pos): Either[String, Expr] = {
    def here[T <: Positioned](node: T): T = node.setPos(whole)

    val parsed =
      t.exprs.foldRight(Right(Nil): Either[String, List[Expr]]) { (src, acc) =>
        for
          rest <- acc
          e    <- parseEmbedded(src)
        yield e :: rest
      }

    // A plain hole renders through `str`; a hole with a specifier renders through `format`, which
    // carries the specifier as a literal for the analyzer to check against the value's type.
    def render(e: Expr, spec: Option[String]): Expr = spec match
      case None      => here(Call(here(Ident("str")), List(e)))
      case Some(fmt) => here(Call(here(Ident("format")), List(e, here(StrLit(fmt)))))

    parsed.map { exprs =>
      val terms =
        t.parts.head match
          case "" => List.empty[Expr]
          case p  => List(here(StrLit(p)): Expr)

      val holes = exprs.lazyZip(t.parts.tail).lazyZip(t.specs)

      val rendered = holes.foldLeft(terms) { case (acc, (e, part, spec)) =>
        val withExpr = acc :+ render(e, spec)
        if part.isEmpty then withExpr else withExpr :+ here(StrLit(part))
      }

      rendered match
        case Nil     => here(StrLit(""))
        case x :: xs => xs.foldLeft(x)((l, r) => here(Binary("+", l, r)))
    }
  }

  /** Lexes and parses the source of a `${ … }` interpolation as a single expression.
   *
   * The embedded text is its own little source, so a position inside a hole points into the hole
   * rather than into an unrelated column of the line the string sits on.
   *
   * **A hole is therefore a placeholder boundary** (`reference/expressions.md § _ — a parameter
   * with the name left out`), and it has to be: the sub-parser numbers placeholders from zero, so a
   * `_` in a hole and a `_` outside the string would both be `$ph1` and the lift would build a
   * parameter list naming one thing twice. Closing the hole makes that unreachable rather than
   * unlikely, since a lifted closure's names never join the outer tree's free set. What it costs is
   * that a placeholder cannot reach out of a string to close over the whole of one — the arrow form
   * outside the string is how that is written.
   */
  protected def parseEmbedded(src: String): Either[String, Expr] = {
    val sub = new SyslParser(Source(s"${source.name} (interpolation)", src))

    sub.parseExpression match
      case sub.Success(e, _) => Right(Placeholders.lift(e))
      case ns: sub.NoSuccess => Left(s"in interpolation '$src': ${ns.msg}")
  }

  protected lazy val boolLit: Parser[Expr] =
    op("true") ^^ (_ => BoolLit(true)) | op("false") ^^ (_ => BoolLit(false))

  protected lazy val nullLit: Parser[Expr] = op("null") ^^ (_ => NullLit())

  protected lazy val identExpr: Parser[Expr] = ident ^^ Ident.apply

  /** `_` in operand position — a closure's parameter with the name left out
   * (`reference/expressions.md § _ — a parameter with the name left out`).
   *
   * It is tried before [[identExpr]] because `_` lexes as an identifier, and it is matched here
   * rather than given a token of its own so that the pattern grammar's `_` is untouched: the two
   * productions never read the same position, so neither has to know about the other.
   *
   * What it yields is an ordinary name that the source could not have written, which is what makes
   * a lifted closure indistinguishable from a written one everywhere downstream. The counter runs
   * per parser, so the names are unique within the file; a value burned by a production that
   * backtracked only leaves a gap in the numbering, which nothing reads.
   */
  protected lazy val placeholderExpr: Parser[Expr] = wildcard ^^ { _ =>
    placeholders += 1
    Ident(Placeholders.fresh(placeholders))
  }

  private var placeholders = 0
}
