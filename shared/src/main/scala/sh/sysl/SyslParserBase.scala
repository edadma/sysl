package sh.sysl

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.{Position, Reader}

/** What every part of the grammar stands on: the token reader, position stamping, the terminal
 * matchers, and the handful of shapes that recur at every level — a comma list, a soft word, an
 * indented suite, an `end` marker.
 *
 * It also declares, abstractly, the rules that are reached **across** the grammar's areas. A
 * language's grammar is not a tree of independent parts: an expression may be an `if` whose branches
 * are statements, a statement may be a declaration whose body is statements again, and a type appears
 * inside all three. Splitting the grammar across files therefore has to name those crossings rather
 * than pretend they do not exist, and naming them here is what lets each area be read on its own —
 * the list below *is* the coupling, in full, and nothing outside it reaches sideways.
 *
 * The traits are mixed in one order — `ExprParser`, then `DeclParser` and the areas beside it, then
 * `StmtParser`, `ControlFlowParser`, and `SyslParser` itself — so a rule needs a declaration here
 * only when it is used by an area that comes **before** the one that defines it. That is why the
 * list is shorter than the number of crossings.
 *
 * **The order is part of the specification, so a new area goes in at the position whose rules it
 * took.** `StmtParser` and `ControlFlowParser` were both carved out of `SyslParser`'s own body and
 * are mixed in exactly where that body sat, which is what makes the split a move rather than a
 * change: the linearization every rule resolves against is the one it resolved against before.
 */
trait SyslParserBase extends PackratParsers {

  val source: Source


  val lexical: SyslLexical = new SyslLexical
  type Elem = lexical.Token

  /** A reader over the pre-scanned, positioned token list. Immutable, so packrat may revisit
   * positions safely — unlike feeding the stateful scanner directly.
   *
   * `past` is what it reports once the tokens are exhausted. It is still **no position at all** —
   * line zero, exactly as `NoPosition` was — and it carries where the last token stopped so that a
   * rule running to the end of the file has an end. `TokenPos.after` says why both halves matter.
   */
  protected class TokenReader(tokens: List[(lexical.Token, Position)], past: Position)
      extends Reader[lexical.Token] {
    def first: lexical.Token = if (tokens.isEmpty) lexical.EOF else tokens.head._1
    def pos: Position        = if (tokens.isEmpty) past else tokens.head._2
    def atEnd: Boolean       = tokens.isEmpty

    /** One reader per token, built the first time the position is reached and reused after.
     *
     * A `def` here would answer a *new* reader every time a rule consumed this token, and a rule
     * consumes each token many times over a backtracking parse. [[PositionReader]] says what that
     * costs.
     */
    lazy val rest: Reader[lexical.Token] =
      if (tokens.isEmpty) this else new TokenReader(tokens.tail, past)
  }

  /** A [[PackratReader]] that answers **one reader per token position** rather than a fresh one per
   * step, which is what keeps a parse's memory proportional to the file rather than to the number
   * of times the grammar walked over it.
   *
   * `PackratReader.rest` is a `def` that builds a new reader — and, with it, a new memo table and a
   * new recursion-head table — on every call. Every token a rule consumes calls it, every
   * alternative that backtracks calls it again, and every memoized result *keeps* the reader it
   * ended at, so the tables that reader allocated and never used are retained for the whole parse.
   * That is the dominant cost of parsing a file: measured over a 4,000-line program before this,
   * 1.4 GB allocated and 129 MB still live at the end of the parse, against a 5 MB tree.
   *
   * Answering the same reader for the same position is sound because the memo is only ever
   * consulted at the reader's *own* position — `getFromCache`, `updateCacheAndGet` and the
   * recursion heads all key on `pos`, and `pos` is fixed for a given reader. A table per position
   * is therefore the same table, indexed one level earlier: the same entry is found by the same
   * lookup, and the giant `(parser, position)` map that used to hold the whole file at once —
   * whose `growTable` is where a large build ran out of heap — is replaced by one small map per
   * token, holding only the rules that were tried there.
   */
  protected class PositionReader(under: Reader[lexical.Token]) extends PackratReader[lexical.Token](under) {
    readersBuilt += 1

    override lazy val rest: Reader[lexical.Token] =
      if (under.atEnd) this else new PositionReader(under.rest)
  }

  /** How many [[PositionReader]]s this parser has built — which is what says the memo is laid out
   * per position rather than per step, and is the only observable that distinguishes the two.
   *
   * A parse builds one reader per token it reaches and never a second, so this is bounded by the
   * file's token count however long the grammar spends backtracking over it. It is a plain count
   * rather than anything about time or memory on purpose: a test can assert on it, and it cannot
   * pass or fail because of the machine.
   */
  private[sysl] var readersBuilt: Int = 0

  /** The token list this parser reads, each token's position widened into the span it occupies.
   *
   * `src` is this parser's own `source.text` at both call sites — the end offsets the lexer
   * reports are offsets into whatever was scanned, and they are turned back into lines and columns
   * against `source`, so handing it anything else would place the ends in another file.
   */
  protected def reader(src: String): Reader[lexical.Token] = {
    val tokens = spanned(lexical.scanPositioned(src))
    val past   = tokens.lastOption match {
      case Some((_, p)) => TokenPos.after(source, p.endLine, p.endColumn)
      case None         => TokenPos.after(source, 0, 0)
    }

    new PositionReader(new TokenReader(tokens, past))
  }

  /** The scanned tokens, each with the offset just past it resolved into a line and a column, and
   * each told where the token **before** it stopped.
   *
   * That second part is how a rule learns its own extent, and it travels this way because there is
   * nowhere else for it to travel. A parser is handed the reader positioned at the token *after*
   * everything it consumed, and `PackratReader` forwards nothing of the reader beneath it but that
   * token's position — so the end of the last token consumed has to arrive in the position of the
   * one following. `rest.pos` will not do instead: it is the next token's *start*, which is past
   * whatever whitespace, comment or line break sits between, and would put a node's end wherever
   * the writer happened to press return.
   *
   * The end is clamped forward to the start, for the two tokens the scanner synthesizes at end of
   * input: they occupy no characters and are reported over the reader in front of them.
   */
  private def spanned(scanned: List[(lexical.Token, Position, Int)]): List[(lexical.Token, TokenPos)] = {
    val buf        = ListBuffer.empty[(lexical.Token, TokenPos)]
    var prevLine   = 1
    var prevColumn = 1

    for ((token, start, past) <- scanned) {
      val (line, column)       = source.placeOf(past)
      val backwards            = line < start.line || (line == start.line && column < start.column)
      val (endLine, endColumn) = if backwards then (start.line, start.column) else (line, column)

      buf += ((token, TokenPos(source, start.line, start.column, endLine, endColumn, prevLine, prevColumn)))
      prevLine = endLine
      prevColumn = endColumn
    }

    buf.toList
  }

  // --- positions -----------------------------------------------------------------------

  /** The span of the next token, in this parser's source. */
  protected def posOf(in: Input): Pos = in.pos match {
    case p: TokenPos => p.toPos
    case p           => Pos(source, p.line, p.column)
  }

  /** The extent a rule covered: from the start of the token it began at, to the end of the last
   * token it consumed.
   *
   * The end arrives in the position of the token *after* the rule — `TokenPos` carries where its
   * predecessor stopped, for the reason `spanned` gives. A rule that consumed nothing, and a reader
   * carrying no span at all, both leave the first token's own extent standing.
   */
  protected def spanOf(from: Input, to: Input): Pos = {
    val start = posOf(from)

    to.pos match {
      case p: TokenPos => start.endingAt(p.prevEndLine, p.prevEndColumn)
      case _           => start
    }
  }

  /** Stamps whatever `p` builds with the extent of everything `p` consumed — as the node's `extent`
   * always, and as the position a diagnostic points at where the rule did not choose one itself.
   *
   * **The two come apart on purpose and a single stamp cannot serve both.** A rule may anchor its
   * node somewhere inside what it read — `postfixTail` puts a field selection on the member name
   * rather than on the dot, and a call on its callee — because that is where a reader's attention
   * belongs when the node is wrong. The extent answers a different question, asked by an editor
   * rather than by a reader: which construct is the cursor inside. `xs.foo(1)` points at `foo` and
   * covers all of `xs.foo(1)`.
   *
   * Because `setPos` keeps the first position it is given (`Positioned`), wrapping an outer rule
   * never overwrites what an inner one already recorded — so a rule that merely passes its
   * operand through costs nothing, and only the rule that actually built the node decides where
   * it points. `p` is by-name so a rule may wrap a `lazy val` declared later in the file.
   *
   * **That is also what keeps an extent honest rather than merely large.** An inner rule finishes
   * before the rule around it, so it stamps first, and a precedence ladder passing one operand up
   * through a dozen levels leaves the operand spanning itself rather than spanning the ladder. A
   * node's span therefore grows to the whole construct exactly when the construct is what was
   * built — a call, a binding, a binary operation — and no further.
   *
   * **The node is stamped in place and `p`'s own result is handed back**, rather than a fresh
   * `Success` around it. A `Success` carries the furthest failure the parse reached on its way here
   * (`lastFailure`), which is what a parser that backtracks and then succeeds knows about the file
   * and nothing else does — and it is what `phrase` reports when the whole parse stops short. That
   * field has no public constructor, so rebuilding the result silently emptied it: every rule in the
   * grammar is wrapped in this one, so what survived to be reported was whatever failed *outside*
   * the outermost `at`, which is a position near the top of the file rather than near the mistake.
   *
   * **A failure the rule did not get past is dropped, and only one further on is kept.** A rule that
   * ends in an optional tail — `logicalOr ~ assignOp ~ expression | logicalOr`, or any `opt` — leaves
   * behind a failure at exactly the token it stopped before, saying which of the tail's spellings it
   * looked for last. That is a road not taken rather than a reason, and it is at the position the
   * enclosing rule is about to complain about itself, with a message about what it actually wanted.
   * `val x = 1 2` reported `'>>=' expected` from an assignment nobody was writing; the token the
   * statement wanted there is a newline, and dropping the road not taken is what lets it say so.
   * Rebuilding the result is how the field is emptied, since it cannot be written.
   */
  protected def at[T <: Positioned](p: => Parser[T]): Parser[T] = {
    lazy val q = p

    Parser { in =>
      q(in) match {
        case s @ Success(t, rest) =>
          val span = spanOf(in, rest)

          t.setPos(span)
          t.setExtent(span)

          if s.lastFailure.exists(f => !(rest.pos < f.next.pos)) then Success(t, rest) else s
        case other => other
      }
    }
  }

  /** Renames the failure `p` reports when it fails **without consuming anything**, so the reader is
   * told what was wanted rather than which candidate the grammar happened to try last.
   *
   * The precedence ladder is a stack of alternations, and `Failure.append` keeps the *last* of the
   * candidates that failed at one position. A token that can begin no expression at all fails every
   * level of the ladder at the same place, so what was reported was whatever sits at the bottom of
   * the last alternative tried — which is why four unrelated mistakes all used to say `'..' expected`
   * and mention a range nobody had written.
   *
   * The rename fires only at the rule's own start. A failure further along is the grammar having got
   * somewhere and then found something specific missing, and that message is the better one.
   */
  protected def describe[T](what: String)(p: => Parser[T]): Parser[T] = {
    lazy val q = p
    val wanted = s"$what expected"

    Parser { in =>
      q(in) match {
        case f: Failure if !(in.pos < f.next.pos) => Failure(wanted, in)
        case other                                => other
      }
    }
  }

  /** [[scala.util.parsing.combinator.Parsers.accept]], with the refusal's words built **once**
   * rather than on every token the terminal declines.
   *
   * `acceptMatch` concatenates `expected + " expected"` inside the parser, so a terminal offered a
   * token it does not want builds a string to say so — and a backtracking grammar offers every
   * terminal to very nearly every token, almost always to be refused. Building those strings was
   * the largest single allocation site in a parse, and none of them is ever read: only the furthest
   * failure is reported.
   *
   * The message and the behaviour are exactly the library's, including `end of input`; what changes
   * is when the text is assembled.
   */
  override def accept[U](expected: String, f: PartialFunction[Elem, U]): Parser[U] = {
    val wanted = expected + " expected"

    Parser { in =>
      if (in.atEnd) Failure("end of input", in)
      else if (f.isDefinedAt(in.first)) Success(f(in.first), in.rest)
      else Failure(wanted, in)
    }
  }

  /** `rep`, with the rule it repeats built **once** rather than once per element.
   *
   * `Parsers.rep1` takes its operand by name and evaluates it *inside* the parser it returns, so an
   * operand written as an expression — which is how nearly every repetition in this grammar is
   * written — is rebuilt at every element of every list the file contains, alternation, terminals
   * and refusal messages included. Forcing it into a `lazy val` first leaves the library's own loop
   * unchanged and hands it the same parser every time.
   *
   * A rule does not depend on where it is applied, so building it once is the same grammar. The
   * `lazy val` is what keeps an operand naming a rule declared later in the file reachable, exactly
   * as [[at]] and [[describe]] do.
   */
  override def rep[T](p: => Parser[T]): Parser[List[T]] = {
    lazy val q = p

    super.rep(q)
  }

  override def rep1[T](p: => Parser[T]): Parser[List[T]] = {
    lazy val q = p

    super.rep1(q)
  }

  override def rep1[T](first: => Parser[T], p: => Parser[T]): Parser[List[T]] = {
    lazy val f = first
    lazy val q = p

    super.rep1(f, q)
  }

  /** The current position, consuming nothing — for a rule that builds its node from a tail it
   * has already passed, where the tail's own start is the better place to point.
   */
  protected def here: Parser[Pos] = Parser(in => Success(posOf(in), in))

  // --- terminals -----------------------------------------------------------------------

  /** Matches a keyword or delimiter token by its exact spelling. Reserved words and
   * operators both lex as `Keyword`, so this covers `if`, `+`, `..<` alike.
   */
  protected def op(sym: String): Parser[String] =
    accept(s"'$sym'", { case t: lexical.Keyword if t.chars == sym => sym })

  /** A name, however it was written — bare, or backtick-quoted.
   *
   * The two forms are one token here because every position that names something takes either: a
   * quoted name is a name, and nothing about a parameter, a field or a call cares how it was
   * spelled. The places that *do* care are the ones matching a specific `Identifier` by its text —
   * the soft keywords `end`, `sync`, `volatile` and `Fn` — and they get the distinction for free by
   * naming the bare token, which is why `` `end` `` is a name rather than a block terminator.
   */
  protected lazy val ident: Parser[String] =
    accept("identifier", {
      case t: lexical.Identifier   => t.chars
      case t: lexical.QuotedIdent  => t.name
    })

  /** A reserved word, as a word — `val`, `match`, `struct`, and not `+` or `..<`.
   *
   * Both lex as `Keyword`, so what tells them apart is the spelling: a reserved word begins with a
   * letter and an operator does not. That test is the whole of it, and it stays right as the set
   * grows, which counting the list here would not.
   *
   * It exists for the refusals below rather than for the grammar — nothing is *read* through it, and
   * a rule that consumed one would be claiming a word the statement grammar needs.
   */
  protected lazy val reservedWord: Parser[String] =
    accept("reserved word", { case t: lexical.Keyword if t.chars.head.isLetter => t.chars })

  /** The sentence a reserved word written where a name is being **bound** is owed — `val: int` as a
   * field, or as a parameter.
   *
   * **The colon is part of the lookahead and is what keeps this off ground the expression grammar
   * needs.** A reserved word may perfectly well begin an argument — `f(true)`, `f(null)`, `f(if c
   * then 1 else 2)` — and a call written at statement position is tried against the *declaration*
   * grammar first, so a refusal keyed on the word alone raises an `Error` inside the parameter list
   * and takes every one of those with it. Followed by a colon it can only be a binding somebody
   * attempted.
   *
   * **It is written out rather than assembled from `guard`, because a lookahead leaks its own
   * position.** `guard(reservedWord ~ op(":"))` hands back the inner failure *as it stands* — at the
   * colon, one token past the word — and a `Failure` further along the line outranks the one `ident`
   * raises at the word itself. What that produced was `reserved word expected` where a field with no
   * name at all should say `identifier expected`, which is the artifact `ParseDiagnosticTests` exists
   * to catch, and did. Re-basing the failure onto `in` puts both candidates at one position, where
   * the last of them wins and `ident` is written last for exactly that reason.
   *
   * **The backtick form is named because it works**, and nothing else tells the reader so: a quoted
   * name is an ordinary name at every position, so `` `val` `` is a field, a parameter or a variable
   * called `val`. Without this the reader is told a name was expected at a place where they wrote
   * one, and the only thing wrong with it is that the language had already spent the word.
   */
  protected def reservedBinding(what: String): Parser[String] =
    Parser { in =>
      guard(reservedWord ~ op(":"))(in) match
        case Success(w ~ _, _) =>
          Error(
            s"'$w' is a reserved word, so it cannot stand as $what — write it '`$w`' if that is " +
              s"the name you want, which is what the backticks are for: a quoted word is an " +
              s"ordinary name wherever one may be written",
            in,
          )
        case ns: NoSuccess => Failure(ns.msg, in)
    }

  /** A name that was written without quoting, and only that — what a **module path** is made of. */
  protected lazy val bareIdent: Parser[String] =
    accept("identifier", { case t: lexical.Identifier => t.chars })

  /** A name that was written backtick-quoted, and only that.
   *
   * One production reads it: a pattern, where the quoting is what says the name is a reference to
   * something already declared rather than a new binding (`09`).
   */
  protected lazy val quotedIdent: Parser[String] =
    accept("quoted identifier", { case t: lexical.QuotedIdent => t.name })

  /** `_`, which the lexer hands over as an ordinary identifier.
   *
   * Two productions read it and they never read the same position: a pattern's wildcard (`09`) and
   * an expression's placeholder (`reference/expressions.md § _ — a parameter with the name left
   * out`). Both go through this one matcher so there is a single answer to what the token is,
   * rather than two spellings of the test that could drift apart.
   */
  protected lazy val wildcard: Parser[Unit] =
    accept("'_'", { case t: lexical.Identifier if t.chars == "_" => () })

  protected val newline: Parser[Unit] = accept("newline", { case lexical.Newline => () })
  protected val indent: Parser[Unit]  = accept("indent", { case lexical.Indent => () })
  protected val dedent: Parser[Unit]  = accept("dedent", { case lexical.Dedent => () })

  protected def newlines: Parser[Unit] = rep1(newline) ^^^ (())

  /** Blank lines, however many, including none — the spelling to reach for wherever a construct
   * tolerates them rather than requires them.
   *
   * It cannot fail, and that is the point rather than a convenience. `opt(newlines)` reads the same
   * tokens, but on a line that starts with something else it first *records* a failure saying a
   * newline was expected, and that expectation then competes with the real one: a file whose first
   * statement will not parse was reported as `newline expected` against a line where a newline would
   * have been no help at all. Nothing is ever owed a newline here, so nothing should say it was.
   */
  protected def skipNewlines: Parser[Unit] = Parser { in =>
    var rest = in

    while (!rest.atEnd && rest.first == lexical.Newline) rest = rest.rest

    Success((), rest)
  }

  /** A continuation the writer *may* have put on a following line — a `match`, an `else`, an `end`.
   *
   * Blank lines are crossed to look for it, and where it is not there the refusal is reported back
   * at the token the search started from. Reporting it where the search ended would be a demand for
   * a keyword nobody was writing, at a line the reader had not connected to this construct — and,
   * being further into the file, it would outrank the real mistake and be the one message shown.
   * `print(1)` followed by a stray `)` was reported as `'match' expected` against the `)`.
   */
  protected def onNextLine[T](p: => Parser[T]): Parser[T] = asOneToken(skipNewlines ~> p)

  /** Reports `p`'s refusal back at the token `p` began at, whatever `p` crossed before noticing.
   *
   * For a construct that is being *looked for* rather than required: what the search crossed is not
   * the reader's mistake, and a refusal recorded past it would outrank the real one by sitting
   * further into the file. Where the enclosing rule is a [[maybe]] or a [[repeatedly]], reporting at
   * the start is also what lets them recognise the construct as absent and say nothing at all.
   */
  protected def asOneToken[T](p: => Parser[T]): Parser[T] = {
    lazy val q = p

    Parser { in =>
      q(in) match {
        case f: Failure => Failure(f.msg, in)
        case other      => other
      }
    }
  }

  /** `opt(p)` for a construct whose **absence is ordinary**, such as a file's module header.
   *
   * `opt` records what `p` wanted even when `p` never began, so a file that opens with something the
   * grammar cannot read at all was told `'module' expected` — a header nobody omits by mistake, and
   * a demand that would not have helped. Here a refusal at the first token is dropped: the construct
   * simply is not there. A refusal *after* one is kept, exactly as `opt` keeps it, because by then
   * the writer had started the construct and the complaint is about how it goes on.
   */
  protected def maybe[T](p: => Parser[T]): Parser[Option[T]] = {
    lazy val q = p

    Parser { in =>
      q(in) match {
        case s @ Success(_, _)                 => s.map(Some(_))
        // Past the first token the writer had started the construct, so the complaint is worth
        // keeping — and `append` onto a `Success` is how the library itself records one.
        case f: Failure if in.pos < f.next.pos => f.append(Success(None, in))
        case _: Failure                        => Success(None, in)
        case e: Error                          => e
      }
    }
  }

  /** `rep(p)` for a construct whose **absence is ordinary** — the repeated form of [[maybe]].
   *
   * A repetition ends by `p` failing, and `rep` records that failure: the list of header attributes
   * a file may open with therefore left behind an expectation of one more attribute, on the very
   * line whose real problem was something else entirely. Ties are settled in favour of whichever was
   * recorded first, and this one is recorded before the statement that will actually fail — so it
   * won, and a file opening with a stray bracket was told `newline expected`.
   */
  protected def repeatedly[T](p: => Parser[T]): Parser[List[T]] = {
    // One parser for the whole repetition rather than one per element. The recursion is on the
    // `lazy val`, so every round of the loop reads the same two parsers instead of rebuilding
    // `maybe(p)` and the tail from scratch at each element.
    lazy val one: Parser[Option[T]] = maybe(p)

    lazy val more: Parser[List[T]] = one >> {
      case Some(x) => more ^^ (x :: _)
      case None    => success(Nil)
    }

    more
  }

  // --- reached across the grammar's areas -----------------------------------------------
  //
  // Each of these is defined in a later trait than one that uses it, so it is declared here for the
  // earlier one to reach. Overriding an abstract `def` with a `lazy val` is what keeps packrat
  // memoization on the concrete rule.

  /** A type, with or without a memory-mode sigil (`03`). */
  protected def typeRef: Parser[TypeRef]

  /** One statement, of any of the forms `statement` admits. */
  protected def statement: PackratParser[Stmt]

  /** An indented block of statements. */
  protected def suite: PackratParser[List[Stmt]]

  /** That a [[suite]] is what comes next, without reading any of it — for the places a block is one
   * *alternative* among others rather than the only thing that may stand there.
   *
   * It exists because of where the refusal lands. `suite` consumes the newline before asking for the
   * indent, so where there is no block it fails one token further along than an `expression`
   * alternative does — and the furthest failure is the one reported. A closure whose body was
   * forgotten, or a binding whose value was, therefore got `indent expected` against the *following*
   * line, demanding a block the writer had not begun and was not going to. [[asOneToken]] puts that
   * refusal back where the search started, so both alternatives fail at one token and the later of
   * them is the one whose message is shown.
   *
   * It guards the look-ahead only. Once the indent is there the block has *begun*, and a mistake
   * inside it is reported where it was made, exactly as [[maybe]] keeps a refusal past the first
   * token.
   */
  protected lazy val blockAhead: Parser[Unit] =
    asOneToken(guard(newline ~ indent)) ^^^ (())

  /** A block introduced by a keyword, written inline after it or indented under it. */
  protected def body(keyword: String): Parser[List[Stmt]]

  /** A block written on one line, which is the single statement it holds. */
  protected def inlineBody: PackratParser[List[Stmt]]

  /** The quoted symbol an `extern` or a `@link` names, where that differs from the word the program
   * calls it by (`reference/ffi.md § @link`).
   */
  protected def linkName: Parser[String]

  /** One pattern, as a `match` arm writes it. Declared here because a condition may test one too
   * (`reference/expressions.md § is — a pattern where a condition is wanted`), and the two
   * spellings must be the same grammar rather than two that drift.
   */
  protected def pattern: Parser[Pattern]

  /** A soft keyword: a word that is only special where the grammar expects it, and an ordinary
   * identifier everywhere else (`reference/lexical.md § Reserved words`). `sync`, `Fn`, `end`,
   * `new`, `within` and `where`
   * are all of them, which is why the three below are declared beside it.
   */
  protected def softWord(word: String): Parser[Unit]

  protected def softSync: Parser[Unit]
  protected def softEnd: Parser[Unit]
  protected def fnWord: Parser[Unit]

  /** A function's declared result: one type, or several separated by commas (`reference/declarations.md § Several results`). */
  protected def resultRef: Parser[TypeRef]

  /** `some Trait` — a result whose concrete type is read off the body. Declared here because a
   * **property** takes one and properties are read by the declaration grammar, which is a sibling of
   * the type grammar rather than built on it.
   */
  protected def opaqueRef: Parser[TypeRef]

  /** `[T, U: Show, V = int]` — the generic parameter list every declaration form reads the same way,
   * and the one bound within it, which a `trait`'s supertrait list also reads.
   */
  protected def boundedTypeParams: Parser[TypeParams]
  protected def boundRef: Parser[BoundRef]

  /** `where T: Display + Eq` — the same bounds, written after a signature instead of inside its
   * `[…]` list. Declared here because the declaration forms that read it and the rule that parses
   * it are in sibling traits, exactly as `boundedTypeParams` is.
   */
  protected def whereBounds: Parser[List[WhereBound]]

  /** A name that may be reached through the module it belongs to: `File`, `std.fs.File`, and the
   * `[int, string]` an applied generic one carries.
   */
  protected def qualifiedName: Parser[String]
  protected def typeArgs: Parser[List[TypeRef]]

  /** `private`, `private[M]`, or nothing at all — which is public (`reference/modules.md § Visibility`). */
  protected def visibility: Parser[Visibility]

  /** The string a contract clause or a `@test` carries — the sentence a reader is shown when the
   * check fails, or the name a test report gives the function.
   */
  protected def contractMsg: Parser[String]

  /** The refusal every member block opens each of its lines with, for the blocks that keep nothing:
   * an annotation's sigil where a member was wanted. Defined beside the annotations it is about, and
   * reached from the rules that read a member.
   */
  protected def noMemberAttr: Parser[Unit]

  /** The annotations a member may carry — the three that are about a **parameter** — with everything
   * else answered by the sentence `noMemberAttr` carries. Empty where the member wrote none.
   */
  protected def memberAttrs: Parser[List[Attr]]

  /** Those three folded onto the member they stood above. */
  protected def attributedMember(m: MethodDecl, as: List[Attr]): MethodDecl

  /** A statement written on the same line as the keyword that introduces it. */
  protected def inlineStatement: PackratParser[Stmt]

  /** A whole file's worth of statements. */
  protected def statements: PackratParser[List[Stmt]]

  /** The expression a one-line function body is, which is an expression that may also be the comma
   * list a multi-result signature returns.
   */
  protected def resultValue: PackratParser[Expr]

  // The six expression forms that are written like statements. `expression` admits them directly,
  // which is what makes `var x = if c then a else b` a binding rather than a special case.

  protected def ifExpr: PackratParser[Expr]
  protected def whileExpr: PackratParser[Expr]
  protected def doWhileExpr: PackratParser[Expr]
  protected def loopExpr: PackratParser[Expr]
  protected def forExpr: PackratParser[Expr]
  protected def matchExpr: PackratParser[Expr]

  /** `for all i in r do P` / `for some i in r do P` (`reference/verification.md § for all and for
   * some`), which is not one of the six above: it is written like an operand rather than like a
   * statement, and it yields a `bool`.
   */
  protected def quantifier: PackratParser[Expr]
}

/** A token's position, which is a **span**: a token occupies characters rather than sitting at one,
 * and both a diagnostic that underlines it and an editor that resolves a cursor to it need to know
 * how far it runs.
 *
 * It is a `Position` — rather than something carried beside one — because that is the only channel
 * a rule can reach it through. `PackratReader` holds its underlying reader as a plain constructor
 * parameter, so the `TokenReader` beneath it is unreachable from a parser; of the five things it
 * does forward, only `pos` can carry an extent without meaning something other than what it says.
 * Widening `offset` to mean the token's end would also have worked and would have left a reader
 * whose offset is not its own position.
 *
 * It also carries **where the token before it stopped**, which is not about this token at all: it is
 * the only way the end of a *rule* reaches the rule, since what a rule is handed when it finishes is
 * the reader at the token after everything it consumed. `SyslParserBase.spanned` says why the
 * following token's own start will not serve.
 *
 * `SyslParserBase.reader` builds these **once** and stores them in the token list. Rebuilding one
 * per access would defeat `PackratReader`'s memo cache, which is keyed on `(parser, pos)` and
 * compares positions by identity.
 */
final class TokenPos(val source: Source, val line: Int, val column: Int,
                     val endLine: Int, val endColumn: Int,
                     val prevEndLine: Int, val prevEndColumn: Int) extends Position {

  protected def lineContents: String = source.line(line)

  /** This span in the compiler's own terms, which is what a diagnostic is rendered against. */
  def toPos: Pos = Pos(source, line, column, endLine, endColumn)
}

object TokenPos {
  def apply(source: Source, line: Int, column: Int, endLine: Int, endColumn: Int,
            prevEndLine: Int, prevEndColumn: Int): TokenPos =
    new TokenPos(source, line, column, endLine, endColumn, prevEndLine, prevEndColumn)

  /** What a reader reports once its tokens are exhausted: **no position at all**, carrying where
   * the last token stopped.
   *
   * The first half is not a shortcut, it is the contract. Line zero is what `NoPosition` answers,
   * and the whole of the grammar's error selection is `in.pos < f.next.pos` comparisons that a
   * position at line zero loses — which is how a failure that merely ran out of input stays out of
   * the way of one that has something to say. Give the end of the file a real line and it becomes
   * the furthest failure in every parse that reaches it: `print(,)` reported `end of input` instead
   * of naming what it wanted, because running out is always further along than the mistake.
   *
   * The second half is why this exists at all: a rule that consumed the last token of the file has
   * nothing after it to carry its end, so the reader answers for it.
   */
  def after(source: Source, endLine: Int, endColumn: Int): TokenPos =
    new TokenPos(source, 0, 0, 0, 0, endLine, endColumn)
}
