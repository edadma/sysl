package sh.sysl

/** The annotations written above a declaration — `@test`, `@tailrec`, `@pure`, `@ghost`.
 *
 * Each alternative reads its own `@`, and the two misspellings a reader arrives with — an
 * unknown word, and `#` where `@` was meant — are rules here rather than parse failures, because
 * what such a reader is missing is which sigil marks what.
 *
 * The fold onto `FuncDecl` lives here too: an attribute exists to say something about the
 * declaration under it, so what it says and how it is applied are one area.
 */
trait AttrParser extends ExprParser {

  /** One annotation: what it says about the function under it. Each alternative reads the `@` for
   * itself so that the annotation's own position is the `@`, which is the line a test report names.
   */
  protected lazy val attribute: PackratParser[Attr] =
    testAttr ^^ Attr.Test.apply | hookAttr | tailrecAttr | pureAttr | ghostAttr | readsAttr | writesAttr |
      crossingAttr | needsAttr | packedAttr | alignAttr | exportAttr | sectionAttr | borrowsHere |
      unknownAttr | hashAttr

  /** What a member block reads where a member was wanted, for the three blocks that do not keep the
   * annotations: a trait's body, an `impl`'s, and a setter's line.
   *
   * It is `memberAttrs` with the answer thrown away, so the refusals below reach every block alike
   * and only the *keeping* differs. `attribute` is read at **statement** position and nowhere else,
   * so without this an annotation written above a method reached whichever rule was going to
   * complain about the line — `dedent expected` where a member had already been read, `identifier
   * expected` where none had. Both are about indentation and about names, which is the one thing
   * that is not wrong.
   *
   * `@assert` is told apart and answered separately: it stands *where* a declaration stands rather
   * than saying anything about one, so the sentence about what annotations mark is exactly the wrong
   * thing to say about it — the same distinction `assertDecl` is ordered before `attributedDecl`
   * for.
   *
   * **`#` is read here as well**, and it is the spelling the reader this rule is for actually writes:
   * `#[test]` above a method is Rust's, and an indented `#` never reaches the directive pass, which
   * takes only what sits at the margin. Answering it with `hashAttr`'s sentence alone would send them
   * to write `@test`, which a member is refused all the same — so the sigil is named and the member
   * rule is what the sentence is about.
   *
   * **The cases are told apart inside one lookahead rather than by separate alternatives**, and that
   * is the whole reason the word is read through `opt(ident)` instead of `attrWord`. Separate
   * alternatives fail at separate positions: `guard(op("@") ~ attrWord("assert"))` gets past the `@`
   * before it declines, and a `Failure` one token further along **outranks** an `Error` raised back
   * at the `@` — so the sentence a reader was meant to get lost to `'assert' expected` every time.
   * Written this way the lookahead cannot fail past the sigil, and the one refusal it then raises
   * points at the sigil itself, which is the line the reader has to change.
   */
  protected lazy val noMemberAttr: Parser[Unit] = memberAttrs ^^ (_ => ())

  /** The annotations a member may carry, which are the ones that are **about a parameter**.
   *
   * `@crossing`, `@reads` and `@writes` each name parameters, and a member has parameters exactly as
   * a free function does — so there was never anything for the refusal below to be about in their
   * case, and refusing them cost `Channel[T]` its `send` and `try_send`, which had to be free
   * functions taking the channel by address so that a wrapper existed to write the word on. Card
   * `0313`.
   *
   * Everything else stays refused, and the sentence is shorter for it: what `@test`, `@tailrec`,
   * `@export` and the layout attributes say is about a free function or about a type, and a member
   * is neither.
   */
  protected lazy val memberAttrs: Parser[List[Attr]] =
    rep(memberAttr <~ skipNewlines) >> { as =>
      duplicated(as) match
        case Some(dup) =>
          err(s"'@$dup' is written twice above one member, and it says nothing the once does not")
        case None => success(as)
    }

  /** One annotation above a member: read it, and answer with the sentence where it is not one of the
   * three.
   *
   * The word is read through `opt(ident)` rather than by alternation, for the reason the refusal
   * below records: separate alternatives fail at separate positions, and a `Failure` one token past
   * the `@` outranks an `Error` raised back at it.
   */
  private lazy val memberAttr: Parser[Attr] =
    guard((op("@") | op("#")) ~ opt(ident)) >> {
      case "@" ~ Some("crossing") => crossingAttr
      case "@" ~ Some("borrows")  => borrowsAttr
      case "@" ~ Some("reads")    => readsAttr
      case "@" ~ Some("writes")   => writesAttr
      case "@" ~ Some("assert") =>
        err("'@assert' stands where a declaration stands, and a type's body holds its members — " +
          "write it beside the type rather than inside it, where 'sizeof' and 'offsetof' still name " +
          "what it is about")
      case sigil ~ _ =>
        err("the only annotations a member may carry are the ones about a parameter — '@crossing', " +
          "'@borrows', '@reads' and '@writes'. What the rest say is about a free function or about a type: what " +
          "'sysl test' calls, what recurses, what a symbol names, how fields are laid out. A member " +
          "is neither, so it goes above a free function instead ('06')" +
          (if sigil == "#" then ". An annotation is written '@' in any case — '#' opens a directive, " +
             "which gates lines before the lexer sees them and sits at the margin"
           else ""))
    }

  /** The three folded onto the member they were written above. It is `attributed`'s counterpart and
   * is deliberately not `attributed` itself: the fold there is total over `Attr`, so a new
   * attribute makes it fail to compile rather than be silently dropped, and that property is worth
   * keeping in both places.
   */
  protected def attributedMember(m: MethodDecl, as: List[Attr]): MethodDecl =
    as.foldLeft(m) {
      case (d, Attr.Crossing(ns)) => d.copy(crossing = ns)
      case (d, Attr.Borrows(ns))  => d.copy(borrows = ns)
      case (d, Attr.Reads(ns))    => d.copy(reads = Some(ns))
      case (d, Attr.Writes(ns))   => d.copy(writes = Some(ns))
      case (d, _)                 => d
    }.setPos(m.pos)

  /** `@packed` — fields at their declared offsets with no interior padding, and an aggregate that
   * needs no alignment of its own (`reference/types.md § Structs`). It takes no arguments: there is
   * nothing to configure about the absence of a gap.
   */
  protected lazy val packedAttr: PackratParser[Attr] =
    op("@") ~> attrWord("packed") ^^ (_ => Attr.Packed)

  /** `@align(n)` — the boundary storage of this type must begin on, which may only be raised.
   *
   * The bound is an expression because it is folded rather than lexed: `@align(CACHE_LINE)` is the
   * form worth writing, and a program that had to repeat the number would be stating the same fact
   * in two places. What it may be is the constant set of `reference/modules.md § Platform
   * selection`.
   */
  protected lazy val alignAttr: PackratParser[Attr] =
    op("@") ~> attrWord("align") ~> (op("(") ~> expression <~ op(")") ^^ Attr.Align.apply | alignErr)

  private def alignErr: Parser[Attr] =
    err("'@align' names the boundary in parentheses — '@align(64)', or '@align(CACHE_LINE)' for a " +
      "constant that says what the number is for. There is no bare form: an alignment with no " +
      "number is not a weaker claim, it is no claim")

  /** `@export` and `@export("mylib_parse")` — the definition is C-callable, under its own name or
   * under the symbol named (`reference/ffi.md § @export`).
   *
   * The parenthesised form takes a **string** rather than an identifier, exactly as `extern`'s link
   * name does, and for the same reason: it is a symbol the other side chose and it is not required to
   * be anything sysl could lex. The two are one mechanism read in opposite directions, so they are
   * spelled alike.
   *
   * The refusal is raised **inside** the parentheses, per the rule a dead `err` taught: an
   * alternative that fails at the `(` is outranked by one that got past it, so a sentence written
   * after the closing parenthesis would never be the one reported.
   */
  protected lazy val exportAttr: PackratParser[Attr] =
    at(op("@") ~> attrWord("export") ~> opt(op("(") ~> (linkName | exportErr) <~ op(")"))
      ^^ (s => ExportAttr(s))) ^^ Attr.Export.apply

  private def exportErr: Parser[String] =
    err("'@export' names the C symbol as a string — '@export(\"mylib_parse\")' — or takes no " +
      "parentheses at all, which exports the function under its own name")

  /** `@section(".vectors")` — the linker section this object or definition is placed in
   * (`reference/attributes.md § @section("...")`).
   *
   * The name is a **string** for the reason `@export`'s symbol is one: it is the target's spelling
   * rather than sysl's, and `.vectors`, `__DATA,__mysection` and `.text.boot` are none of them things
   * sysl could lex. Nothing here checks what is in it beyond its being there at all — a character set
   * chosen in this file would refuse a section some target requires.
   *
   * The empty string is refused, and it is the one refusal the string form owes: every other spelling
   * is somebody's, and `""` is nobody's.
   *
   * Both refusals are raised **inside** the parentheses, per the rule a dead `err` taught — an
   * alternative that fails at the `(` is outranked by one that got past it.
   */
  protected lazy val sectionAttr: PackratParser[Attr] =
    op("@") ~> attrWord("section") ~> (op("(") ~> (sectionName | sectionErr) <~ op(")") | sectionErr)

  private lazy val sectionName: Parser[Attr] =
    linkName >> (s =>
      if s.isEmpty then
        err("'@section' names a section, and \"\" is not the name of one — a section's spelling is " +
          "the target's, so there is nothing an empty one could resolve to")
      else success(Attr.Section(s)))

  private def sectionErr: Parser[Attr] =
    err("'@section' names the linker section as a string — '@section(\".vectors\")' on the storage " +
      "or the definition that goes there. The spelling is the target's: '.noinit' and '.ramfunc' " +
      "are ELF's, '__DATA,__mysection' is Mach-O's")

  /** `@test`, and the three things it may say about the test: the name a report gives it, that it is
   * a run which should not come back, and the text such a run should have printed on its way out.
   */
  protected lazy val testAttr: PackratParser[TestAttr] =
    at(op("@") ~> attrWord("test") ~> opt(emptyTestErr | testArgList)
      ^^ (_.getOrElse(TestAttr(None, false, None))))

  /** The parenthesized part of `@test`, which **commits at the `(`**.
   *
   * Everything inside is a rule rather than a parse failure because of where a failure otherwise
   * lands. `opt` declines without consuming when what is between the parentheses is not a
   * description, so the `(` is left unread and the statement rule goes on to look for a declaration
   * there — and refuses the *function below*, with the sentence about an annotation marking a
   * function and only a function. That sends a reader to a declaration which is perfectly ordinary,
   * with a caret on a `(` the message never mentions.
   *
   * **All four spellings did that, not only the empty one**: `@test(3)`, `@test("x"` and
   * `@test(should_trap` gave the same unrelated sentence as `@test()`. Having read the `(` there is
   * nothing else the reader could have been writing, so from here on every road ends in a sentence
   * about the argument list.
   */
  private lazy val testArgList: Parser[TestAttr] =
    op("(") ~> (testArgs | badTestArgErr) <~ (op(")") | unclosedTestErr)

  /** `@test()` — parentheses with nothing between them.
   *
   * **The form stays illegal, which is the decision rather than the easy road.** `@test()` and bare
   * `@test` would mean the same thing, and accepting the first costs a line — but an empty argument
   * list is not a shorter way of saying nothing. It reads as a description that *was* going to be
   * there, and a reader who sees it accepted cannot tell whether the author meant to write one and
   * lost it. The language already has the spelling for saying nothing, and it is `@test`.
   *
   * The lookahead takes in both parentheses so that the refusal is raised at the `(` — the caret
   * then sits on the pair the reader has to delete, rather than one character past it.
   */
  private def emptyTestErr: Parser[TestAttr] =
    guard(op("(") ~ op(")")) ~> err("'@test' takes a description or nothing at all, and '()' is " +
      "neither — drop the parentheses, and the function's own name becomes its description")

  private def badTestArgErr: Parser[TestAttr] =
    err("'@test' takes the description a report shows it under, written as a string — " +
      "'@test(\"an index past the end is refused\")' — and 'should_trap' for a run that is meant " +
      "not to come back, optionally with the text such a run must have printed. The two compose, " +
      "in that order")

  private def unclosedTestErr: Parser[Unit] =
    err("'@test' closes what it opened — the description and 'should_trap' go between parentheses, " +
      "and there is no ')' here to end them")

  /** `@setup`, `@teardown`, `@setup_all` and `@teardown_all` — what a module runs around its tests
   * (`reference/attributes.md § The hooks a module may write`).
   *
   * None of the four takes an argument, and there is nothing one could take: a hook is called by the
   * runner with nothing, exactly as a test is, and what it does is its body. The alternatives are
   * written longest-first out of habit rather than necessity — `attrWord` matches a whole
   * identifier, so `setup_all` is never read as `setup` followed by something.
   *
   * The position kept is the `@`, for `@test`'s reason: it is the line a report points at when the
   * hook is what failed, and a hook failure has no diagnostic of its own to carry one.
   */
  protected lazy val hookAttr: PackratParser[Attr] =
    at(op("@") ~> hookWord <~ noHookArgs) ^^ Attr.Hook.apply

  private lazy val hookWord: Parser[HookAttr] =
    attrWord("setup_all") ^^ (_ => HookAttr(HookKind.SetupAll)) |
      attrWord("teardown_all") ^^ (_ => HookAttr(HookKind.TeardownAll)) |
      attrWord("setup") ^^ (_ => HookAttr(HookKind.Setup)) |
      attrWord("teardown") ^^ (_ => HookAttr(HookKind.Teardown))

  /** An argument list after a hook, which is a sentence rather than a parse failure for `@test`'s
   * reason: the word has been read, so there is nothing else the line could have been, and leaving
   * the `(` unread sends the statement rule on to refuse the perfectly ordinary declaration below.
   */
  private def noHookArgs: Parser[Unit] =
    guard(op("(")) ~> err("a hook takes no arguments — when it runs is what the word says, and what " +
      "it does is its body. There is nothing left for a parenthesis to carry") | success(())

  /** `@tailrec` — the assertion that this function's call to itself is the last thing it does
   * (`reference/declarations.md § Tail calls`). It takes no arguments: there is nothing to
   * configure about a jump, and what the annotation buys is the refusal when there is no jump to
   * make.
   */
  protected lazy val tailrecAttr: PackratParser[Attr] =
    op("@") ~> attrWord("tailrec") ^^ (_ => Attr.TailRec)

  /** `@pure` — the assertion that a caller can observe nothing about this call but its result
   * (`reference/verification.md § @pure`). Like `@tailrec` it takes no arguments: purity is not a
   * thing to configure, and what the annotation buys is the refusal when the body does something a
   * caller could observe.
   */
  protected lazy val pureAttr: PackratParser[Attr] =
    op("@") ~> attrWord("pure") ^^ (_ => Attr.Pure)

  /** `@ghost` — the function exists for the specification alone and is erased before codegen
   * (`reference/verification.md § @ghost — what costs nothing to say`).
   */
  protected lazy val ghostAttr: PackratParser[Attr] =
    op("@") ~> attrWord("ghost") ^^ (_ => Attr.Ghost)

  /** `@reads(a, b)` and `@writes(c)` — which module-level variables the function may touch
   * (`reference/verification.md § @reads and @writes — what a call may touch`). The parentheses are
   * mandatory and may be empty, because `@reads()` is a real and different claim from writing
   * nothing at all: the first says the function reads no module storage, the second says nobody has
   * written down what it does.
   *
   * The argument list is raised **inside** the parentheses rather than after the closing one, per
   * the rule a dead `err` taught: a form that gets further along the line outranks an alternative
   * that failed earlier, so a message written past the point of divergence is never the one
   * reported. Here the only way to fail after `@reads` is the parenthesis, so that is where the
   * sentence goes — otherwise `unknownAttr` below wins by position and says `reads` is not an
   * annotation, which is both wrong and unhelpful.
   */
  protected lazy val readsAttr: PackratParser[Attr] =
    op("@") ~> attrWord("reads") ~> (frameNames ^^ Attr.Reads.apply | frameErr("reads"))

  protected lazy val writesAttr: PackratParser[Attr] =
    op("@") ~> attrWord("writes") ~> (frameNames ^^ Attr.Writes.apply | frameErr("writes"))

  private lazy val frameNames: Parser[List[String]] =
    op("(") ~> repsep(ident, op(",")) <~ op(")")

  private def frameErr(w: String): Parser[Attr] =
    err(s"'@$w' names the module variables it covers, in parentheses — '@$w(count)', or '@$w()' " +
      "for none. The parentheses are what tell a frame that covers nothing from a function that " +
      "never said")

  /** `@crossing(state)` — the parameters through which a value reaches another concurrency domain
   * (`reference/memory.md § @crossing — where the rule is asked`).
   *
   * The parentheses are mandatory and, unlike a frame's, may **not** be empty. `@reads()` is a real
   * claim — the function reads no module storage — while a function that hands nothing to another
   * domain says so by not writing the annotation, so `@crossing()` would be a line that means what
   * its absence already means.
   *
   * The refusal is raised **inside** the parentheses, per the rule a dead `err` taught: an
   * alternative that fails at the `(` is outranked by one that got past it, so a sentence written
   * after the closing parenthesis would never be the one reported.
   */
  protected lazy val crossingAttr: PackratParser[Attr] =
    op("@") ~> attrWord("crossing") ~> (op("(") ~> (crossingNames | crossingErr) <~ op(")") |
      crossingErr)

  private lazy val crossingNames: Parser[Attr] =
    rep1sep(ident, op(",")) ^^ Attr.Crossing.apply

  /** What `@borrows` gets where it cannot mean anything, which is above a **free function**.
   *
   * The exemption it asks for exists because a call through a trait object is opaque — any
   * implementation could be behind it, so the analysis must assume the worst. A free function's body
   * is right here, and the analysis reads the answer out of it; a written promise would restate what
   * the compiler can see and would go stale the moment the body changed.
   *
   * Ordered before `unknownAttr`, for `@reads`' reason: the general refusal wins by position
   * otherwise, and says `borrows` is not an annotation — which is wrong as well as unhelpful.
   */
  private lazy val borrowsHere: Parser[Attr] =
    op("@") ~> attrWord("borrows") ~>
      err("'@borrows' is about a call whose body the compiler cannot see, so it says something only " +
        "on a trait's method. A free function's body is right here and the analysis reads the " +
        "answer out of it — a promise written above one would restate what the compiler already " +
        "knows, and would go stale the moment the body changed")

  /** `@borrows(bytes)` — the parameters a trait method promises not to keep past the call.
   *
   * `@crossing`'s shape exactly, including that there is no empty form: a method that keeps
   * everything says so by not writing the annotation, so `@borrows()` would be a line meaning what
   * its absence already means. The refusal is raised inside the parentheses for `@crossing`'s
   * reason — an alternative failing at the `(` is outranked by one that got past it.
   */
  protected lazy val borrowsAttr: PackratParser[Attr] =
    op("@") ~> attrWord("borrows") ~> (op("(") ~> (borrowsNames | borrowsErr) <~ op(")") |
      borrowsErr)

  private lazy val borrowsNames: Parser[Attr] =
    rep1sep(ident, op(",")) ^^ Attr.Borrows.apply

  private def borrowsErr: Parser[Attr] =
    err("'@borrows' names the parameters a trait's method promises not to keep past the call, in " +
      "parentheses — '@borrows(bytes)'. There is no empty form: a method that may keep what it is " +
      "handed says so by not writing the annotation")

  private def crossingErr: Parser[Attr] =
    err("'@crossing' names the parameters a value reaches another concurrency domain through, in " +
      "parentheses — '@crossing(state)'. There is no empty form: a function that hands nothing " +
      "across a boundary says so by not writing the annotation")

  /** `@needs(heap)`, `@needs(os, posix)` — the capabilities reaching this declaration requires
   * (`reference/modules.md § A declaration may name what reaching it needs`).
   *
   * Parenthesised and plural, exactly as the file header's `@requires(...)` is and for the same
   * reason: a declaration needs several capabilities at once and gives none up. **What the two
   * spellings buy is that neither can be read as the other** — a file header is a prefix of the
   * file, so an `@requires` written above the first declaration would be the file's whatever the
   * writer meant, and nothing in the grammar could tell. Two words, two positions, one meaning
   * each.
   *
   * The list is raised **inside** the parentheses, per the dead-`err` rule: a sentence written past
   * the point of divergence is outranked by whichever alternative got further along the line.
   */
  protected lazy val needsAttr: PackratParser[Attr] =
    op("@") ~> attrWord("needs") ~> (op("(") ~> (needsNames | needsErr) <~ op(")") | needsErr)

  private lazy val needsNames: Parser[Attr] =
    rep1sep(ident, op(",")) ^^ Attr.Needs.apply

  private def needsErr: Parser[Attr] =
    err("'@needs' names the capabilities reaching this declaration requires, in parentheses — " +
      "'@needs(heap)', '@needs(os, posix)'. There is no empty form: a declaration that needs " +
      "nothing beyond what its module has says so by not writing the annotation")

  private lazy val unknownAttr: PackratParser[Attr] =
    op("@") ~> ident >> (n =>
      err(s"'$n' is not an annotation a declaration takes — '@test', '@setup', '@teardown', " +
        "'@setup_all', '@teardown_all', '@tailrec', '@pure', " +
        "'@ghost', '@export', '@reads(...)', '@writes(...)' and '@crossing(...)' mark a function, " +
        "'@packed' and " +
        "'@align(n)' mark a struct's layout, '@export(\"...\")' names a struct in a generated C " +
        "header, '@section(\"...\")' marks either a binding or a " +
        "function, and '@needs(...)' marks a function or an 'extern'. '@no_<capability>', " +
        "'@requires(...)', '@link(\"...\")' and '@tests' belong in the file's header"))

  /** `#test` where `@test` was meant — the sigil a reader arriving from Rust or C reaches for first.
   *
   * It is answered here rather than left to the lexer because the two sigils mark two different
   * kinds of thing, and saying which is which is the whole of what the reader is missing: `#` gates
   * lines before the lexer sees them and sits at the margin, `@` says something about the
   * declaration under it. A directive word is *not* named here, since `#if` at the margin never
   * reaches the grammar at all.
   */
  private lazy val hashAttr: PackratParser[Attr] =
    op("#") ~> ident >> (n =>
      err(s"an annotation is written '@$n' — '#' opens a directive, which gates lines before the " +
        "lexer sees them and sits at the margin"))

  /** An annotation's name. Each stays an ordinary identifier — reserving them would spend the words
   * out of every program's namespace for the sake of one line apiece, which is the trade `alloc`
   * made and is still paying for.
   */
  protected def attrWord(w: String): Parser[Unit] =
    accept(s"'$w'", { case t: lexical.Identifier if t.chars == w => () })

  /** An annotation whose name *carries* its argument — `@no_alloc` is `no_` and then the capability.
   * What comes back is the part after the prefix, so the caller never sees the joint.
   *
   * The prefix alone is not one of these: `@no_` names no capability, and matching it would hand the
   * analyzer an empty name to complain about instead of the parser refusing a word that is visibly
   * unfinished.
   */
  protected def attrWordPrefixed(prefix: String): Parser[String] =
    accept(
      s"'$prefix…'",
      { case t: lexical.Identifier if t.chars.startsWith(prefix) && t.chars.length > prefix.length =>
        t.chars.drop(prefix.length)
      },
    )

  /** Whether an attribute says **when `sysl test` calls** the function under it, for the refusal of
   * two such attributes above one declaration.
   *
   * `@test` and the four hooks are the members, and they are alternatives rather than a set: each
   * names a different moment, and a declaration carrying two would have to be called at both.
   */
  protected def runnerRole(a: Attr): Boolean = a match
    case _: Attr.Test | _: Attr.Hook => true
    case _                           => false

  /** Whether an attribute is one half of a frame, for the refusal of `@pure` beside one. */
  protected def frame(a: Attr): Boolean = a match
    case _: Attr.Reads | _: Attr.Writes => true
    case _                              => false

  /** The attribute written twice, where one is, for the refusal above. */
  protected def duplicated(as: List[Attr]): Option[String] =
    as.map(_.word).groupBy(identity).collectFirst { case (w, ws) if ws.length > 1 => w }

  protected def attributed(f: FuncDecl, as: List[Attr]): FuncDecl =
    as.foldLeft(f) {
      case (d, Attr.Test(t)) => d.copy(test = Some(t))
      case (d, Attr.Hook(h)) => d.copy(hook = Some(h))
      case (d, Attr.TailRec) => d.copy(tailrec = true)
      case (d, Attr.Pure)    => d.copy(pure = true)
      case (d, Attr.Ghost)   => d.copy(ghost = true)
      case (d, Attr.Reads(ns))  => d.copy(reads = Some(ns))
      case (d, Attr.Writes(ns)) => d.copy(writes = Some(ns))
      case (d, Attr.Export(e))  => d.copy(exported = Some(e))
      // `@section` is the one attribute that marks either kind of thing, so it reaches this fold and
      // the binding's alike — a `.ramfunc` is a definition placed somewhere exactly as a vector table
      // is storage placed somewhere.
      case (d, Attr.Section(s)) => d.copy(section = Some(s))
      case (d, Attr.Crossing(ns)) => d.copy(crossing = ns)
      case (d, Attr.Needs(cs))    => d.copy(needs = cs)
      // `@borrows` never reaches here — `borrowsHere` refuses it at statement position, where a free
      // function's annotations are read. Listed rather than left out so that this fold stays total
      // over `Attr`, which is what makes a new attribute fail to compile instead of being dropped.
      case (d, _: Attr.Borrows) => d
      // A layout attribute never reaches here: the grammar routes a declaration carrying one to a
      // struct, and refuses the mix. Listed so that a new attribute makes this fold fail to compile
      // rather than silently drop what it was asked to record.
      case (d, Attr.Packed | _: Attr.Align) => d
    }

  private lazy val testArgs: Parser[TestAttr] =
    contractMsg ~ opt(op(",") ~> testExpectation) ^^ {
      case d ~ e => TestAttr(Some(d), e.isDefined, e.flatten)
    } | testExpectation ^^ (e => TestAttr(None, true, e))

  /** `should_trap`, alone or with the substring a trapping run must have printed. */
  private lazy val testExpectation: Parser[Option[String]] =
    accept("'should_trap'", { case t: lexical.Identifier if t.chars == "should_trap" => () }) ~>
      opt(op(":") ~> contractMsg)
}
