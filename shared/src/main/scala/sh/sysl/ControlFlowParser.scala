package sh.sysl

/** Control flow, which in sysl is written like a statement and read like an expression.
 *
 * Split out of `SyslParser`. `ExprParser` admits all six of these directly as operands, which is
 * what makes `var x = if c then a else b` an ordinary binding rather than a special case — the
 * crossing is `SyslParserBase`'s abstract declarations, so nothing here has to be reached from
 * above by name.
 *
 * `softEnd` and `endMarker` are here rather than beside the declarations that also carry an `end`:
 * every form with a marker of its own is in this file, and `DeclParser` reaches `softEnd` through
 * the base's declaration exactly as it reaches everything else across an area boundary.
 */
trait ControlFlowParser extends StmtParser {

  /** `if cond then a else b` — an expression. Its branches are statement lists whose trailing
   * expression is the branch value; `elif` nests into the else branch, and the `else` is
   * optional (a missing one gives an open branch that only the analyzer's unit rule allows).
   *
   * **The nested `if` an `elif` becomes is positioned at its own keyword.** Only the outermost node
   * is stamped by the rule around this one, so a chain built here would leave every `elif` but the
   * first pointing nowhere — and a complaint about a branch would arrive with no caret. Each clause
   * carries the span of the `elif` that introduced it, which is where the reader would look.
   */
  protected lazy val ifExpr: PackratParser[Expr] =
    op("if") ~> expression ~ body("then") ~ rep(elifClause) ~ opt(elseClause) ~ opt(endMarker("if")) ^^ {
      case c ~ t ~ elifs ~ e ~ _ =>
        val elseChain = elifs.foldRight(e) { case ((kw, ec, eb), acc) =>
          Some(List(ExprStmt(IfExpr(ec, eb, acc).setPos(kw)).setPos(kw)))
        }

        IfExpr(c, t, elseChain)
    }

  /** `end` is a soft keyword — an ordinary identifier everywhere except immediately before a
   * construct keyword, where `end if` / `end while` / `end for` close the preceding block
   * Scala-style. It is optional; matching it here (rather than reserving `end`) keeps `end`
   * usable as a name.
   */
  protected lazy val softEnd: Parser[Unit] =
    accept("'end'", { case t: lexical.Identifier if t.chars == "end" => () })

  protected def endMarker(construct: String): Parser[Unit] =
    onNextLine(softEnd) ~> op(construct) ^^^ (())

  /** `elif cond then …` is sugar for `else if cond then …` — each one nests into the else
   * branch of the previous, so no distinct AST node is needed.
   *
   * The keyword's own span comes out with the parts, because the node it stands for is built by
   * [[ifExpr]] after every later clause has been read.
   */
  protected lazy val elifClause: Parser[(Pos, Expr, List[Stmt])] =
    onNextLine(withSpan(op("elif"))) ~ expression ~ body("then") ^^ {
      case (kw, _) ~ c ~ b => (kw, c, b)
    }

  /** `else` sits on a fresh line after a block body, or on the same line after an inline
   * one — so any intervening `Newline` is optional.
   */
  protected lazy val elseClause: Parser[List[Stmt]] =
    onNextLine(op("else")) ~> (suite | inlineBody)

  /** `while cond body [else …]` — an expression. The optional `else` reuses the same clause as
   * `if`, sitting after the body and before any `end while`.
   */
  protected lazy val whileExpr: PackratParser[Expr] =
    loopLabel ~ (op("while") ~> expression) ~ body("do") ~ opt(elseClause) ~ opt(endMarker("while")) ^^ {
      case lbl ~ c ~ b ~ e ~ _ => While(lbl, c, b, e)
    }

  /** `do body while cond [else …]` — the post-test loop (`reference/statements.md`).
   *
   * **`do` is unambiguous by position, which is what lets the same word do both jobs.** Everywhere
   * else it is a body *introducer*, and it only ever appears there after a loop header on the same
   * line — `while c do …`, `loop do …`, `for x in xs do …` — so it is never the first token of a
   * statement. Here it is the head, so a `do` that starts a line can only open this form.
   *
   * The tail needs no rule of its own for the same reason: this loop is *incomplete* without its
   * `while`, and there is no bare `do` block in the language for that `while` to have belonged to
   * instead. So a `while` found where the body ends is this loop's test and nothing else can want
   * it. Both bodies are written: `do total += 1 while more()` on one line, or an indented block with
   * the `while` on the line that closes it.
   *
   * There is no `end do`: the tail already names where the body stopped, and a marker after it would
   * be closing a block that has just been closed.
   */
  protected lazy val doWhileExpr: PackratParser[Expr] =
    loopLabel ~ (op("do") ~> (suite | inlineBody)) ~ (skipNewlines ~> op("while") ~> expression) ~
      opt(elseClause) ^^ {
        case lbl ~ b ~ c ~ e => DoWhile(lbl, b, c, e)
      }

  /** `loop body` — a `while` with the condition left out, ended by a `break` rather than by a test.
   *
   * It takes no `else`: an `else` runs when a loop finishes on its own, and this one never does.
   * The inline `loop do …` form is there because `while c do …` has it and a one-line body should
   * not have to change shape when its condition goes away.
   */
  protected lazy val loopExpr: PackratParser[Expr] =
    loopLabel ~ (op("loop") ~> body("do")) ~ opt(endMarker("loop")) ^^ { case lbl ~ b ~ _ => Loop(lbl, b) }

  /** `for all i in 0..<n do a[i] > 0`, `for some k in 0..n do a[k] == t` — a quantifier over an
   * integer range (`reference/verification.md § for all and for some`).
   *
   * **`all` and `some` stay ordinary identifiers**, matched here as soft words, so nothing a program
   * already names is spent on this. Telling the form from a counted loop takes one token: a
   * quantifier is `for` `all`/`some` `name` `in`, and a loop is `for` `name` `in`, so `for all in
   * 0..<n do …` is still a loop over a variable called `all`. This rule is tried before `forExpr` in
   * `expression` and backtracks into it when the name is missing.
   *
   * The separator is `do` — the word every loop header already uses to introduce what it does with
   * each element — rather than Ada's `=>`, which is not in the operator set and would have been
   * added for this one form. The predicate is a full `expression`, so the body extends as far to the
   * right as one can: `for all i in r do P(i) && Q(i)` quantifies over the conjunction, which is the
   * reading a specification wants and the one a reader of the line already expects from `->`.
   */
  protected lazy val quantifier: PackratParser[Expr] =
    (op("for") ~> quantifierKind) ~ ident ~ (op("in") ~> expression) ~ (op("do") ~> expression) ^^ {
      case univ ~ n ~ it ~ p => Quantifier(univ, n, it, p)
    }

  private lazy val quantifierKind: Parser[Boolean] =
    softWord("all") ^^^ true | softWord("some") ^^^ false

  protected lazy val forExpr: PackratParser[Expr] = constForExpr | cForExpr | forCommaNames | forInExpr

  /** `for const i in 0..<A.len` — the loop the compiler unrolls (`reference/generics.md § A
   * parameter may stand for a list of types`).
   *
   * Tried first, and unambiguous against the other two: neither of them may have `const` after the
   * `for`. There is no label and no `else` clause, because there is no loop at run time for a
   * `break` to leave or for an `else` to follow — the copies are what the analyzer produces.
   */
  protected lazy val constForExpr: PackratParser[Expr] =
    (op("for") ~> op("const") ~> ident) ~ (op("in") ~> expression) ~ body("do") ~ opt(endMarker("for")) ^^ {
      case n ~ it ~ b ~ _ => ConstFor(n, it, b)
    }

  /** `for x in xs`, and `for (k, v) in pairs` — the walk, whose variable is a **name or a pattern**.
   *
   * The pattern is [[destructuring]], which is what a `val` or a `var` binding takes, so there is
   * one rule rather than two: a `for` binds what a binding binds — a tuple, a struct, either of
   * those named with `n @`, nested as deep as the value goes, and a variant pattern parsed only so
   * that it can be refused with a reason. Nothing about the loop had to learn any of that.
   *
   * **A pattern is desugared here, into the binding the reader would otherwise have written.** The
   * loop names a temporary no program can write, and the body opens with the pattern bound to it:
   *
   * ```
   * for (k, v) in walk()            for $elem in walk()
   *     print(k)             -->        var (k, v) = $elem
   *                                     print(k)
   * ```
   *
   * That leaves `For` binding a name, which is what every later pass already reads, and it reaches
   * both kinds of walk at once — a range and an `Iterate` lower differently, and this sits above the
   * difference.
   *
   * **The synthesized binding is stamped with the pattern's own position**, which is what keeps the
   * desugaring from showing through: `for (a, b) in 0..<n` is answered *"one int is not something to
   * take apart"* under the `(` in the header, and not against a line the reader did not write.
   *
   * The parts are `var` rather than `val` because the loop variable itself is one — a `for` may
   * assign to what it named, and taking the value apart should not quietly take that away.
   *
   * **The two forms cannot be confused for one another**: every alternative of `destructuring`
   * needs a `(` or a `{` where a plain loop has its `in`, so `for x in xs` never reaches the
   * pattern path and parses exactly as it always did.
   */
  protected lazy val forInExpr: PackratParser[Expr] =
    loopLabel ~ (op("for") ~> here) ~ forBinding ~ (op("in") ~> expression) ~ body("do") ~ opt(elseClause) ~ opt(
      endMarker("for"),
    ) ^^ {
      case lbl ~ _ ~ Right(n) ~ it ~ b ~ e ~ _ => For(lbl, n, it, b, e)
      case lbl ~ p0 ~ Left(p) ~ it ~ b ~ e ~ _ =>
        val elem = s"${Modules.sep}elem"
        val bind = PatternDecl(p, mutable = true, Ident(elem).setPos(p0)).setPos(p0)

        For(lbl, elem, it, bind :: b, e)
    }

  /** What a `for` binds: a pattern, or the plain name that has always stood there.
   *
   * The pattern is tried first and costs nothing when there is not one — it commits on a token a
   * name cannot be.
   */
  private lazy val forBinding: Parser[Either[Pattern, String]] =
    destructuring ^^ (Left(_)) | ident ^^ (Right(_))

  /** `for k, v in pairs` — the comma spelling, parsed so that it can be refused with the form that
   * works.
   *
   * It is not legal, and leaving it out of the grammar does not make it an error a reader can act
   * on. The three-clause loop's init clause may be a multi-assignment, so `for k, v …` is a
   * half-written `for i, j = 0, n; i < j; …` until the `in` several tokens later — and what the
   * reader got was `'>>=' expected`, naming a compound assignment nobody had written and sending
   * them looking for one.
   *
   * The comma form is refused rather than accepted because that ambiguity is the whole of what it
   * would buy: one header already has three shapes fighting over its first token, and a second
   * spelling of the pattern form is not worth a fourth.
   *
   * **It has to reach the `in` before complaining, and that is a positioning rule rather than a
   * reading one.** Two candidates that fail at one token are ranked by position and the later one
   * wins, and the three-clause loop gets as far as the `in` before giving up — so a refusal raised
   * back at the comma, which is where the mistake actually is, loses the race to the very message it
   * exists to replace. Written that way it never fired. The `in` is a `guard` so that the complaint
   * stops level with its rival rather than one token past it.
   */
  private lazy val forCommaNames: PackratParser[Expr] =
    loopLabel ~> op("for") ~> ident ~ (op(",") ~> rep1sep(ident, op(","))) <~ guard(op("in")) >> {
      case first ~ rest =>
        err(s"a 'for' names one thing, and a pattern is what takes it apart — write " +
          s"'for (${(first :: rest).mkString(", ")}) in …'")
    }

  /** `for init; cond; step` — the three-clause loop (`00` §10).
   *
   * Tried before `for x in …`, and unambiguous against it: this one needs two `;` in its header and
   * that one has none, so whichever the reader wrote is the one that parses. No parentheses, as in
   * Go — every other header in the language is without them, and a parenthesized one here would be
   * the only place they were structure rather than grouping.
   *
   * Each clause may be empty. An absent condition is `true`, which makes `for ; ;` a `loop` spelled
   * the long way; `loop` says it better and this does not forbid it.
   */
  protected lazy val cForExpr: PackratParser[Expr] =
    loopLabel ~ (op("for") ~> opt(forClause) <~ op(";")) ~ (opt(expression) <~ op(";")) ~ opt(forClause) ~
      body("do") ~ opt(elseClause) ~ opt(endMarker("for")) ^^ {
        case lbl ~ init ~ cond ~ step ~ b ~ e ~ _ => CFor(lbl, init, cond, step, b, e)
      }

  /** One clause of a three-clause `for` header: a `var` declaration or a bare expression, which for
   * the step is the assignment or increment that advances the loop.
   */
  protected lazy val forClause: PackratParser[Stmt] = at(varDecl | multiAssign | exprStmt)

}
