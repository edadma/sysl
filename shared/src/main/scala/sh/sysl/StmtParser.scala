package sh.sysl

/** Statements: the bindings, the declarations written among them, the contract clauses, and inline
 * assembly.
 *
 * Split out of `SyslParser`, which is left with the two spellings of a statement *list*, the program
 * rule, and the entry points. The line between the two files is a rule's subject: what one statement
 * may be is here, and how a run of them is read — including the recovering pass that skips the ones
 * it cannot read — is there, because that is the only rule the `recovering` flag reaches.
 *
 * A **declaration** is here rather than in `DeclParser` where a declaration's own body is read,
 * because at the top of a file the two are one thing (`reference/modules.md § Where a program starts`): `declaration` is an alternative of
 * `statement`, and which of the two a line turns out to be is the analyzer's question rather than
 * the grammar's. What `DeclParser` holds is the shape *inside* a declaration — a parameter list, a
 * struct's fields, a trait's members.
 */
trait StmtParser
    extends DeclParser,
      TypeParser,
      AttrParser,
      HeaderParser,
      PatternParser {

  // --- statements ----------------------------------------------------------------------

  lazy val statement: PackratParser[Stmt] =
    at(
      misplacedHeaderAttr | importDecl | implDecl | declaration | varDecl | refDecl | returnStmt |
        becomeStmt |
        breakStmt | continueStmt | deferStmt | asmStmt | requireStmt | ensureStmt | invariantStmt |
        variantStmt | multiAssign | resultListStmt | exprStmt,
    )

  /** A statement written on the same line as the keyword that introduces it.
   *
   * It is every statement **but** a result list, which is a whole line by construction: a branch
   * written inline is part of a larger expression, so a comma after it belongs to whatever that
   * expression is part of. Without this, `-> int, string = if c then 1 else 0, "x"` would read the
   * comma as the *branch's* result list and leave the function one value.
   */
  protected lazy val inlineStatement: PackratParser[Stmt] =
    at(
      importDecl | implDecl | declaration | varDecl | refDecl | returnStmt |
        becomeStmt |
        breakStmt | continueStmt | deferStmt | requireStmt | ensureStmt | multiAssign | exprStmt,
    )

  /** `a, b = b, a` — a comma list of places, a comma list of values (`reference/expressions.md § Several places at once`).
   *
   * It comes before `exprStmt` and after everything else, and it needs **two or more** targets to
   * commit: with one it would be an ordinary assignment written the long way round, which
   * `expression` already reads. Nothing below a statement admits a bare comma, so the first one is
   * enough to tell the two apart with no lookahead to speak of.
   */
  protected lazy val multiAssign: PackratParser[Stmt] =
    (logicalOr <~ op(",")) ~ rep1sep(logicalOr, op(",")) ~ assignOp ~ rep1sep(expression, op(",")) ^^ {
      case first ~ rest ~ o ~ values => MultiAssign(o, first :: rest, values)
    }

  /** `require <cond> [, "message"]` / `ensure <cond> [, "message"]` — a design-by-contract
   * clause. Only meaningful at the top of a function body; the analyzer rejects one that
   * appears after ordinary statements.
   */
  protected lazy val requireStmt: PackratParser[Stmt] =
    op("require") ~> expression ~ opt(op(",") ~> contractMsg) ^^ { case c ~ m => Require(c, m) }

  protected lazy val ensureStmt: PackratParser[Stmt] =
    op("ensure") ~> expression ~ opt(op(",") ~> contractMsg) ^^ { case c ~ m => Ensure(c, m) }

  /** `invariant <cond> [, "message"]` and `variant <expr>` — the loop clauses of
   * `reference/verification.md § invariant and variant on a loop`, and, for `variant`, the
   * recursion measure a function's contract block carries (`reference/verification.md § variant on
   * a function`).
   *
   * **Both words are contextual**, matched as soft words exactly as the struct `invariant` of
   * `reference/errors.md § Struct invariants` is — which is also where `invariant` was already
   * being read this way, so this spends no new word. The cost of that is the cost `is` and `not`
   * already pay: a *bare statement* that calls a function of the same name, `invariant(x)`, reads
   * as a clause over `(x)`. Anywhere that is not a bare statement — `val v = invariant(x)`, an
   * argument, a condition — the call is unambiguous, and a value named `invariant` is untouched.
   * That trade buys the clause its natural spelling in the position a reader writes it.
   */
  protected lazy val invariantStmt: PackratParser[Stmt] =
    invariantKw ~> expression ~ opt(op(",") ~> contractMsg) ^^ { case c ~ m => Invariant(c, m) }

  protected lazy val variantStmt: PackratParser[Stmt] =
    variantKw ~> expression ^^ Variant.apply

  protected lazy val variantKw: Parser[Unit] = softWord("variant")

  protected lazy val contractMsg: Parser[String] =
    accept("string literal", { case t: lexical.StrLit => t.value })

  /** `static val`, `static var` — a binding in the file the program starts in asking to be the
   * module's rather than that file's body's (`reference/modules.md § Where a program starts`).
   *
   * The modifier follows the visibility rather than preceding it, so `private static var ticks: u64`
   * reads in the order the two questions are asked: how far it reaches, then whose it is.
   *
   * **A function never takes it**, which is worth saying because it is the one form a reader expects
   * to. Whether a function at the top of the entry file belongs to the body is settled by whether it
   * reads one of the body's bindings (`Bodies.capturing`) — one that reads none is the module's
   * already and has nothing to ask for, and one that reads any is holding a frame, which is precisely
   * what a module member cannot do. So the modifier would be either redundant or impossible, and
   * neither is worth a keyword.
   *
   * A type, a `const`, an `extern` and an `import` are module members wherever they are written, so
   * `static` on one says nothing either. The sentence below covers all of them, rather than leaving
   * the grammar to complain that a declaration form was not among the two.
   */
  protected lazy val staticDecl: PackratParser[Stmt] =
    visibility ~ (op("static") ~> (valDecl | varDecl)) ^^ {
      case Visibility.Public ~ d => StaticDecl(d)
      case v ~ d                 => StaticDecl(restrict(v, d))
    } | (visibility <~ op("static")) ~> err(
      "'static' marks a 'val' or a 'var' in the file a program starts in, saying it belongs to the " +
        "module rather than to that file's body. Everything else there is the module's already: a " +
        "type, a constant, an 'extern' and an 'import' are wherever they are written, and a function " +
        "is one unless it reads a binding of the body — which is a frame to carry, and the one thing " +
        "a module member cannot have",
    )

  /** A declaration that may carry a visibility modifier (`reference/modules.md § Visibility`).
   *
   * The forms are grouped so the modifier is written once, before whichever of them follows, rather
   * than threaded through rules that would each have to remember it. An `impl` is not among them and
   * takes none: it declares no name, so there is nothing for a modifier to restrict.
   *
   * **`varDecl` is here as well as in `statement`, and that is what lets a module's storage be
   * private.** Outside the file a program starts in, a top-level `var` is the module's storage and is
   * the same declaration `static var` spells in that file (`reference/modules.md § Where a program starts`) — so it takes a visibility for the
   * same reason the `val` beside it does. Without this the modifier was a parse error reading
   * "identifier expected", which says the word was not followed by a name rather than that the form
   * takes none; `private static var` parsed all along, and the two spellings are one declaration.
   *
   * It changes nothing for a bare `var`: `visibility` succeeds as `Public` on the empty input, the
   * `Public` branch hands back exactly the node `varDecl` built, and `statement`'s own `varDecl` reads
   * the ones written inside a body. In the entry file the modifier restricts a local and so says
   * nothing, which is what `private val` there has always done.
   */
  protected lazy val declaration: PackratParser[Stmt] =
    assertDecl |
      attributedDecl |
      implVisibility |
      misplacedOverride |
      staticDecl |
      visibility ~ (structDecl | enumDecl | typeDecl | traitDecl | externDecl | cConstDecl |
        cTypeDecl | constDecl | valDecl | varDecl | funcDecl) ^^ {
        case Visibility.Public ~ d => d
        case v ~ d                 => restrict(v, d)
      }

  /** A function carrying annotations, which is a declaration with a line or more in front of it.
   *
   * Each annotation is its own line and the declaration follows the last of them, which is why the
   * newlines between them are consumed here: the statement separator would otherwise end the
   * statement at an annotation, leaving a prefix with nothing to attach to. Everything about the
   * declaration itself is still `declaration`'s — an annotated function may be `private`, and is
   * written exactly as any other.
   *
   * Only a function may carry one, and the refusal below is what says so. A struct or a `val` with
   * `@test` above it is a mistake about what a test *is* rather than a syntax error, so it is
   * answered with the sentence rather than with the list of forms the grammar could still have read.
   */
  protected lazy val attributedDecl: PackratParser[Stmt] =
    rep1(attribute <~ skipNewlines) >> { as =>
      // Once an attribute has been read the statement is committed to being an attributed
      // declaration, which is what `>>` buys: everything after it is read against that, so a
      // declaration that cannot carry one is answered with the sentence below rather than with the
      // grammar's complaint about whichever alternative it went on to try.
      duplicated(as) match
        case Some(dup) =>
          err(s"'@$dup' is written twice above one declaration, and it says nothing the once does not")
        // `@test` and the four hooks each say *when* `sysl test` calls the function, and they name
        // four different moments. Two of them above one declaration is not a stricter request; it is
        // two requests, and nothing decides which is honoured.
        case None if as.count(runnerRole) > 1 =>
          err(as.filter(runnerRole).map(a => s"'@${a.word}'").mkString("", " and ", "") +
            " each say when 'sysl test' calls this function, and they name different moments — a " +
            "function is a test or one of the hooks around one, and it is called once")
        // `@pure` *is* `@reads() @writes()` plus the further bans of `reference/verification.md §
        // @pure`, so the two together say one thing twice — and worse, they could be made to
        // disagree, which would leave nothing to say which of the two claims the function was held
        // to.
        case None if as.exists(_ == Attr.Pure) && as.exists(frame) =>
          err("'@pure' already says '@reads()' and '@writes()', so a frame beside it says one thing " +
            "twice — write the frame alone if the function touches module storage, and '@pure' alone " +
            "if it touches none")
        // `@packed` describes the arrangement of fields *within* a type and `@section` places one
        // object, so the two cannot be about the same declaration whichever kind it turns out to be:
        // a struct is not an object, and a binding has no fields.
        case None if as.exists(_ == Attr.Packed) && as.exists(places) =>
          err("'@packed' lays out a struct's fields and '@section(\"...\")' places one object, so " +
            "they cannot stand above one declaration — a type occupies no address of its own, and " +
            "the storage that holds a packed value is what a section would be about")
        // `@export` is the second attribute marking either kind of thing, and it pairs with the
        // layout attributes rather than with `@section`: a struct's C name sits beside its layout,
        // and a function's symbol beside `@pure` and the rest. This is the struct reading, and a set
        // holding nothing but `@export` reaches the function reading through `namedStruct`.
        case None if as.exists(names) && as.forall(a => layout(a) || names(a)) =>
          namedStruct(as)
        // A layout annotation and a function annotation describe different kinds of thing, so one
        // declaration cannot carry both — and saying which pair collided is more use than the
        // grammar's complaint about whichever alternative it went on to try. `@section` is in
        // neither camp: it marks whatever occupies an address, which is a binding or a function.
        case None if as.exists(layout) && as.exists(a => !layout(a) && !places(a)) =>
          err("'@packed' and '@align' describe a layout and the rest mark a function, so they " +
            "cannot stand above one declaration — a struct has no body to be tail-recursive or " +
            "pure in, and a function has no fields to lay out")
        // `@align` beside `@section` is a binding and only a binding: a struct takes the first and
        // not the second, and a function takes the second and not the first. It is also the pair a
        // statically placed stack is written with, so it is the case rather than a corner.
        case None if as.exists(layout) && as.exists(places) =>
          storageDecl(as) | err(
            "'@align(n)' beside '@section(\"...\")' marks one binding's storage — the boundary it " +
              "begins on and the section it sits in are both about one object, and this declares none",
          )
        case None if as.forall(layout) =>
          (visibility ~ structDecl) ^^ {
            case Visibility.Public ~ (s: StructDecl) => laidOut(s, as)
            case v ~ (s: StructDecl)                 => restrict(v, laidOut(s, as))
            case _ ~ other                           => other
          } | storageDecl(as) | err(
            if as.forall(aligns) then
              "'@align(n)' marks a struct or one binding's storage, and this is neither — an enum's " +
                "layout follows from its variants and a scalar's is the target's"
            else
              "'@packed' describes how a struct's fields are laid out, so it can only mark a struct " +
                "— a 'var' or a 'val' has no fields to pack, and '@align(n)' is the one of the two " +
                "that may stand above one",
          )
        // `@section` alone marks either kind, so both are read: a binding is settled by its own
        // keyword and a function by everything else, which is why the storage form goes first and
        // declines without consuming anything.
        case None if as.forall(places) =>
          storageDecl(as) | (visibility ~ funcDecl) ^^ {
            case Visibility.Public ~ (f: FuncDecl) => attributed(f, as)
            case v ~ (f: FuncDecl)                 => restrict(v, attributed(f, as))
            case _ ~ other                         => other
          } | err(
            "'@section(\"...\")' places one object in a linker section, so it marks a 'var', a " +
              "'val' or a function — a 'const' is folded into every use and has no storage to place, " +
              "and an 'extern' names something this program does not define",
          )
        case None =>
          (visibility ~ funcDecl) ^^ {
            case Visibility.Public ~ (f: FuncDecl) => attributed(f, as)
            case v ~ (f: FuncDecl)                 => restrict(v, attributed(f, as))
            case _ ~ other                         => other
          } | externNeeds(as) | err(
            (if as.forall(needsCap) then
               "'@needs(...)' names what reaching a declaration requires, so it marks a function or " +
                 "an 'extern' — and this declares neither"
             else
               "an annotation marks a function, and only a function — neither what 'sysl test' calls " +
                 "nor what recurses is anything a declaration of another kind supplies. '@packed', " +
                 "'@align(n)' and '@export(\"...\")' are the three that mark a struct instead"),
          )
    }

  /** An `extern` carrying `@needs(...)`, which is the one annotation it may take
   * (`reference/modules.md § A declaration may name what reaching it needs`).
   *
   * **It is the declaration the annotation exists for.** Every other declaration has a body the
   * compiler reads — a function that makes heap storage is found by looking — and an `extern` is a
   * name and a signature, so nothing but the declaration itself can say what calling it costs.
   *
   * Nothing else may stand above one, and the alternative declines rather than refusing so that
   * whichever sentence the caller wrote is the one reported: `@pure` above an `extern` is a claim
   * about a body that is not here, and that is the refusal already written for it.
   */
  private def externNeeds(as: List[Attr]): PackratParser[Stmt] =
    if !as.forall(needsCap) then failure("not an extern's annotation")
    else
      (visibility ~ externDecl) ^^ {
        case Visibility.Public ~ (e: ExternDecl) => e.copy(needs = capabilitiesOf(as)).setPos(e.pos)
        case v ~ (e: ExternDecl)                 => restrict(v, e.copy(needs = capabilitiesOf(as)).setPos(e.pos))
        case _ ~ other                           => other
      }

  /** Whether an attribute is `@needs(...)`, which is the one an `extern` may carry. */
  private def needsCap(a: Attr): Boolean = a match
    case _: Attr.Needs => true
    case _             => false

  /** The capabilities a set of attributes names, flattened — one `@needs` per declaration is what
   * `duplicated` already enforces, so this is a list of at most one attribute's worth.
   */
  private def capabilitiesOf(as: List[Attr]): List[String] =
    as.collect { case Attr.Needs(cs) => cs }.flatten

  /** A struct carrying `@export("…")`, and — where that is the only annotation above it — a function
   * carrying it instead.
   *
   * The two readings are here together because `@export` names *either* kind of thing: a function's
   * symbol and a struct's C name are one request made of the two declarations a generated header
   * holds. `structDecl` goes first and declines without consuming, which is how `@section` reaches
   * the function form past `storageDecl`.
   */
  private def namedStruct(as: List[Attr]): PackratParser[Stmt] =
    (visibility ~ structDecl) ^^ {
      case Visibility.Public ~ (s: StructDecl) => laidOut(s, as)
      case v ~ (s: StructDecl)                 => restrict(v, laidOut(s, as))
      case _ ~ other                           => other
    } | (if !as.forall(names) then failure("not a function's annotation")
         else
           (visibility ~ funcDecl) ^^ {
             case Visibility.Public ~ (f: FuncDecl) => attributed(f, as)
             case v ~ (f: FuncDecl)                 => restrict(v, attributed(f, as))
             case _ ~ other                         => other
           }) | err(
      if as.forall(names) then
        "'@export' names what C sees — a function's symbol, or the name a struct's 'typedef' " +
          "carries in a generated header — and this declares neither. A simple enum is spelled as " +
          "the integer it is, so it has no name in the header to choose, and an 'extern' names " +
          "something this program does not define"
      else
        "'@packed' and '@align(n)' lay out a struct and '@export(\"...\")' names one in a generated " +
          "header, so together they mark a struct — and this declares none",
    )

  /** Whether an attribute **names** what it marks to C, which is `@export` and only `@export`. It is
   * a category of its own for `@section`'s reason: it is one of the two attributes that mark either
   * a struct or a function, and the pair it may be written beside differs by which.
   */
  private def names(a: Attr): Boolean = a match
    case _: Attr.Export => true
    case _              => false

  /** Whether an attribute describes a **layout** rather than a function. */
  private def layout(a: Attr): Boolean = a match
    case Attr.Packed | _: Attr.Align => true
    case _                           => false

  /** Whether an attribute is one of the layout pair that a *binding* can carry, which is `@align`
   * and only `@align`.
   *
   * `@packed` describes the arrangement of fields *within* an aggregate, and a `var` has none — so it
   * is not merely unimplemented on a binding, it has nothing there to mean.
   */
  private def aligns(a: Attr): Boolean = a match
    case _: Attr.Align => true
    case _             => false

  /** Whether an attribute **places** what it marks rather than describing it, which is `@section` and
   * only `@section`. It is a category of its own because it is the one attribute that marks either a
   * binding or a function: both occupy an address, and placement is the same request about each.
   */
  private def places(a: Attr): Boolean = a match
    case _: Attr.Section => true
    case _               => false

  /** `@align(n)` above a `var` or a `val` — the boundary one object's storage begins on, which is C's
   * `alignas` rather than Rust's `#[repr(align)]`.
   *
   * The capability was already reachable through the type: a struct carrying the attribute aligns
   * every value of itself, and a named aligned type is reusable where a repeated attribute is not.
   * What the type form costs is at the *use* site — a buffer wrapped in a struct is read as
   * `region.bytes[i]` rather than `region[i]` — which is the whole argument for this spelling.
   *
   * All four forms take it, and they must: `static var` and `static val` are the entry file's
   * spelling of the same declaration a plain `var` and `val` are in every other file (`reference/modules.md § Where a program starts`), so an
   * attribute that reached one and not the other would mean different things in different files.
   * `staticDecl` is tried first because it begins the same way and is settled by its own word.
   *
   * A **pattern** or a **comma list** is read and then refused, rather than left out of the forms
   * offered. Both bind several names, so there is no one object for a boundary to be about — and a
   * grammar that simply did not accept them would report against the enclosing line instead of
   * against the binding, which is the diagnostic this whole shape exists to avoid.
   */
  private def storageDecl(as: List[Attr]): PackratParser[Stmt] =
    if !as.forall(a => aligns(a) || places(a)) then failure("not a binding's annotation")
    else
      (staticDecl | visibility ~ (valDecl | varDecl) ^^ {
        case Visibility.Public ~ d => d
        case v ~ d                 => restrict(v, d)
      }) >> { d =>
        if !oneBinding(d) then
          // Two sentences rather than one about "an annotation", because each names the thing it is
          // about — and because `@align`'s is quoted verbatim on the site, where a word inserted into
          // the middle of a diagnostic breaks the page at the next version bump and not before.
          if as.forall(aligns) then
            err("'@align(n)' is the boundary one object's storage begins on, and a binding that " +
              "names several has no one object for it to be about — declare them on lines of their own")
          else
            err("'@section(\"...\")' places one object, and a binding that names several has no one " +
              "object for it to be about — declare them on lines of their own")
        else
          success(as.foldLeft(d) {
            case (s, Attr.Align(n))   => aligned(s, n)
            case (s, Attr.Section(n)) => placed(s, n)
            case (s, _)               => s
          })
      }

  /** Whether a binding names exactly one thing, reaching through the `static` wrapper. */
  private def oneBinding(s: Stmt): Boolean = s match
    case _: VarDecl | _: ValDecl => true
    case StaticDecl(d)           => oneBinding(d)
    case _                       => false

  /** One layout attribute folded onto whichever binding it was written above, reaching through the
   * `static` wrapper to the declaration inside it.
   */
  private def aligned(s: Stmt, bound: Expr): Stmt = s match
    case d: VarDecl    => d.copy(align = Some(bound)).setPos(d.pos)
    case d: ValDecl    => d.copy(align = Some(bound)).setPos(d.pos)
    case StaticDecl(d) => StaticDecl(aligned(d, bound)).setPos(s.pos)
    case other         => other

  /** `@section("…")` folded onto whichever binding it was written above, reaching through the
   * `static` wrapper exactly as the boundary above it does.
   */
  private def placed(s: Stmt, name: String): Stmt = s match
    case d: VarDecl    => d.copy(section = Some(name)).setPos(d.pos)
    case d: ValDecl    => d.copy(section = Some(name)).setPos(d.pos)
    case StaticDecl(d) => StaticDecl(placed(d, name)).setPos(s.pos)
    case other         => other

  /** The struct these annotations describe, folded onto its declaration — the layout pair, and the
   * name a generated C header gives it.
   */
  private def laidOut(s: StructDecl, as: List[Attr]): StructDecl =
    as.foldLeft(s) {
      case (d, Attr.Packed)    => d.copy(packed = true)
      case (d, Attr.Align(n))  => d.copy(alignment = Some(n))
      case (d, Attr.Export(e)) => d.copy(cname = Some(e))
      case (d, _)              => d
    }


  /** A modifier written in front of an `impl` block, refused where it stands.
   *
   * This is the same refusal `noVisibility` gives the members *inside* one, and it is here for the
   * same reason: `impl` is not among the forms above, so without a rule of its own the reading is
   * "identifier expected" at the `impl` — the grammar's complaint that the modifier was not
   * followed by a name, which says nothing about why there is no name to follow it with.
   */
  private lazy val implVisibility: Parser[Nothing] =
    op("private") ~ opt(op("[") ~> ident <~ op("]")) ~ guard(op("impl")) ~> err(
      "an 'impl' block carries no visibility of its own — it declares no name for one to restrict, " +
        "and what it supplies is reached at the reach of the trait that asked for it",
    )

  /** `override` in front of a top-level declaration, refused where it stands and for the reason
   * `implVisibility` is refused: the word has a place, and a reader who writes it elsewhere is better
   * served by being told which place than by the grammar's complaint about the word after it.
   *
   * It is reached only after `implDecl` has declined the line, so `override impl` never arrives here.
   */
  protected lazy val misplacedOverride: Parser[Nothing] =
    op("override") ~> err(
      "'override' marks something that replaces an implementation already covering the same type, so " +
        "it goes in front of an 'impl' block or of a member inside one — a declaration of its own " +
        "replaces nothing",
    )

  /** `private`, `private[M]`, or nothing at all — which is public (`reference/modules.md §
   * Visibility`). There is no `pub` keyword; its absence *is* public, so the unmarked case is the
   * one that writes nothing.
   */
  protected lazy val visibility: Parser[Visibility] =
    op("private") ~> opt(op("[") ~> ident <~ op("]")) ^^ {
      case Some(m) => Visibility.Scoped(m)
      case None    => Visibility.File
    } | success(Visibility.Public)

  protected def restrict(v: Visibility, d: Stmt): Stmt = d match
    case s: StructDecl    => s.copy(vis = v).setPos(s.pos)
    case e: EnumDecl      => e.copy(vis = v).setPos(e.pos)
    case t: TraitDecl     => t.copy(vis = v).setPos(t.pos)
    case e: ExternDecl    => e.copy(vis = v).setPos(e.pos)
    case e: ExternVarDecl => e.copy(vis = v).setPos(e.pos)
    case c: ConstDecl     => c.copy(vis = v).setPos(c.pos)
    case b: CConstBlock   =>
      CConstBlock(b.consts.map(c => c.copy(vis = v).setPos(c.pos))).setPos(b.pos)
    case b: CTypeBlock    =>
      CTypeBlock(b.types.map(t => t.copy(vis = v).setPos(t.pos))).setPos(b.pos)
    case l: ValDecl       => l.copy(vis = v).setPos(l.pos)
    case r: VarDecl       => r.copy(vis = v).setPos(r.pos)
    case f: FuncDecl      => f.copy(vis = v).setPos(f.pos)
    case t: TypeDecl      => t.copy(vis = v).setPos(t.pos)
    case other            => other


  protected lazy val varDecl: PackratParser[Stmt] =
    multiDecl("var", mutable = true) |
      patternDecl("var", mutable = true) |
      op("var") ~> ident ~ opt(op(":") ~> typeRef) ~ opt(op("=") ~> initializer) ^^ {
        case n ~ t ~ e => VarDecl(n, t, e.map(Placeholders.lift))
      }

  /** What a binding's `=` takes: one expression, or an indented block whose trailing expression is
   * the value (`reference/lexical.md § An unbracketed line continues after an operator`).
   *
   * The block is tried first and costs nothing when there is not one — it opens on `Newline`+`Indent`,
   * which no expression can begin with, so a value written on the same line as the `=` reaches
   * `expression` having consumed nothing.
   *
   * **A block of a single expression is that expression.** The two are the same value written two
   * ways, and collapsing here is what keeps a module `val` with its value on the next line a constant
   * tree rather than a computed initializer — a distinction a reader who moved the line to fit the
   * margin never asked to make. A block that binds anything cannot collapse and does not.
   */
  protected lazy val initializer: PackratParser[Expr] =
    blockValue | expression

  /** [[blockAhead]] is what keeps the diagnostic for a value that was simply forgotten: without it
   * `val x =` is answered with `indent expected` against the following line, which describes a block
   * the writer had not begun.
   */
  private lazy val blockValue: PackratParser[Expr] =
    blockAhead ~> suite ^^ {
      case List(ExprStmt(e)) => e
      case stmts @ (h :: _)  => Block(stmts).setPos(h.pos)
      case Nil               => UnitLit()
    }

  /** `val (a, b) = …` / `var (a, b) = …` — a binding written as a **pattern** (`reference/types.md
   * § Tuples`).
   *
   * Two patterns may stand here, and they are the two that cannot fail to match: a **tuple**
   * pattern, and a **struct** pattern, which names a type that has exactly one shape.
   *
   * **A variant pattern is parsed here too, so that it can be refused with a reason.** It is not
   * legal — an enum has several shapes and naming one is a test — but leaving it out of the grammar
   * does not make it an error a reader can act on: the parse fails somewhere above and reports
   * against the enclosing declaration's own line, which is the diagnostic this whole form exists to
   * stop happening. Accepting it and complaining in the analyzer puts the message on the binding.
   *
   * It is tried after the comma form and before the plain one, which is all the ordering it needs:
   * a tuple pattern opens with a parenthesis, and the other two are a name followed by a brace or a
   * parenthesis, neither of which a plain binding or a comma list can begin with. The plain form
   * still reads a bare name, so nothing that parsed before this existed parses differently now.
   *
   * **The pattern is the whole of the left side, with no type annotation beside it.** That is
   * `reference/declarations.md § Several results`'s open question again rather than an oversight —
   * the parts of a destructuring have nowhere to carry a type, and inference covers what the form
   * is for.
   */
  protected def patternDecl(keyword: String, mutable: Boolean): PackratParser[Stmt] =
    (op(keyword) ~> destructuring) ~ (op("=") ~> initializer) ^^ {
      case p ~ v => PatternDecl(p, mutable, Placeholders.lift(v))
    }

  /** The patterns a **binding** may be written with: the two that cannot fail, the variant one that
   * is parsed to be refused, and any of those three named with `n @`.
   *
   * The name is optional rather than a fourth alternative so that `var whole @ Point{x, y} = p`
   * reads as the same form with a name on it, which is what it is. It commits on the `@`, so a plain
   * `var x = e` — which is not this production at all — is unaffected.
   */
  protected lazy val destructuring: Parser[Pattern] =
    opt(ident <~ op("@")) ~ (structPattern | variantPattern | tuplePattern) ^^ {
      case Some(n) ~ p => BindPattern(n, p)
      case None ~ p    => p
    }

  /** `ref name = place` (`reference/memory.md § ref — a name for a place`).
   *
   * Neither half of what `var` and `val` accept is offered here, and each absence is a rule rather
   * than an omission. There is **no type annotation**, because a ref is a local declaration and never
   * a type, so it states nothing to a reader elsewhere and its type is the place's by construction.
   * There is **no multiple form**, because the comma family binds several names to several *values*
   * (`reference/expressions.md § Several places at once`) and a place list is what a multi-assignment already is.
   *
   * The initializer is parsed as any expression and held to being a place by the analyzer, which is
   * where the question can be answered at all — `f()[i]` and `xs[i]` are the same shape until
   * something knows what `f` and `xs` are.
   */
  protected lazy val refDecl: PackratParser[Stmt] =
    op("ref") ~> ident ~ (op("=") ~> expression) ^^ { case n ~ p => RefDecl(n, Placeholders.lift(p)) }

  /** `val a, b = …` / `var a, b = …` — a binding that names several things (`reference/expressions.md § Several places at once`).
   *
   * Two or more names, and an initializer, are both required: one name is the ordinary form, and a
   * multiple binding with nothing to take apart names nothing. The parts carry no type annotation,
   * which is `reference/declarations.md § Several results`'s open question rather than an oversight
   * — inference covers what the form is for, and there is no spelling yet for the case it does not.
   */
  protected def multiDecl(keyword: String, mutable: Boolean): PackratParser[Stmt] =
    (op(keyword) ~> ident <~ op(",")) ~ rep1sep(ident, op(",")) ~ (op("=") ~> rep1sep(expression, op(","))) ^^ {
      case first ~ rest ~ values => MultiDecl(first :: rest, mutable, values)
    }

  /** `const name: type = value` (`reference/modules.md § const — a value`). Both halves are mandatory, which is what tells it apart
   * from a `var` at a glance as well as to the parser: a constant with no value is not a
   * declaration of anything, and a type left off would be the one declaration in the language whose
   * interface could not be read off its syntax.
   *
   * **A block is read here and refused in the analyzer**, which is the same arrangement a variant
   * pattern in a binding gets and for the same reason: a `const` folds, a block does not, and leaving
   * the form out of the grammar answers the reader who reached for it with `expression expected`
   * rather than with the rule.
   */
  protected lazy val constDecl: PackratParser[Stmt] =
    op("const") ~> ident ~ (op(":") ~> typeRef) ~ (op("=") ~> initializer) ^^ {
      case n ~ t ~ v => ConstDecl(n, t, v)
    }

  /** `c const` and its constants, each of whose values is a C expression in quotes
   * (`reference/ffi.md § A library may carry C`).
   *
   * ```
   * c const
   *     STATIC_TASK_SIZE: usize = "sizeof(StaticTask_t)"
   *     MAX_DELAY: u32          = "portMAX_DELAY"
   * ```
   *
   * **`c` is contextual and stays an ordinary identifier**, which the `const` after it is what
   * makes safe: nothing else in the language may follow a name with a keyword, so the two words
   * together cannot be anything but this, and a program is free to call a variable `c` — which one
   * counting characters certainly will. It is a cheaper disambiguation than `interrupt`'s
   * (`reference/ffi.md § interrupt`), which needs a lookahead past an optional parenthesized
   * argument to find the name it qualifies.
   *
   * **The C is quoted with a plain string and carries no prefix.** A `c"…"` form would be a second
   * literal kind bought to say what the header already said: inside this block a string can mean
   * nothing else, since there is no other thing a value here could be. The quotes themselves are not
   * optional, and that is the point of them — what is inside is a different language, and a reader
   * should be able to see where it starts without knowing which words are C's.
   *
   * The block is required rather than a `c const NAME: T = "…"` one-liner being offered beside it.
   * One form is what keeps the cost legible: the constants of a file are measured by a single probe,
   * so a run of them under one header reads the way the work is actually done.
   */
  protected lazy val cConstDecl: PackratParser[Stmt] =
    softWord("c") ~> op("const") ~> (
      newline ~> indent ~> skipNewlines ~> rep1sep(cConstItem, newlines) <~ skipNewlines <~ dedent ^^
        CConstBlock.apply |
        err("'c const' is followed by its constants, indented under it, each a name and a type and a " +
          "C expression in quotes: 'SIZE: usize = \"sizeof(struct s)\"'")
    )

  /** One line of a `c const` block. The type is mandatory for `const`'s reason and one more: it is
   * what decides whether the C is read back as signed, and therefore what the probe declares.
   */
  protected lazy val cConstItem: Parser[CConstDecl] =
    at(ident ~ (op(":") ~> typeRef) ~ (op("=") ~> linkName) ^^ { case n ~ t ~ c => CConstDecl(n, t, c) })

  /** `c type` — the sysl types a file's C typedefs turn out to be (`reference/ffi.md § A library
   * may carry C`):
   *
   * ```
   * c type
   *     Tick  = "TickType_t"
   *     Stack = "configSTACK_DEPTH_TYPE"
   * ```
   *
   * The same two words in the same order as `c const`, and for the same reason: `c` marks which
   * language the right-hand sides are written in and stays an ordinary identifier everywhere else,
   * which the keyword after it is what makes safe.
   *
   * **A line carries no sysl type**, which is the whole difference from a `c const` line — the type
   * is the answer rather than the question, and writing one would be asserting what the measurement
   * is for. What a program wanting to *assert* a width writes is `@assert`, against a `c const`
   * holding the `sizeof`, which says the same thing where it can be checked.
   */
  protected lazy val cTypeDecl: PackratParser[Stmt] =
    softWord("c") ~> op("type") ~> (
      newline ~> indent ~> skipNewlines ~> rep1sep(cTypeItem, newlines) <~ skipNewlines <~ dedent ^^
        CTypeBlock.apply |
        err("'c type' is followed by its types, indented under it, each a name and a C type name in " +
          "quotes: 'Tick = \"TickType_t\"'")
    )

  /** One line of a `c type` block: the sysl name, and the C type it stands for. */
  protected lazy val cTypeItem: Parser[CTypeDecl] =
    at(ident ~ (op("=") ~> linkName) ^^ { case n ~ c => CTypeDecl(n, c) })

  /** `@assert(cond)`, `@assert(cond, "why")` — a condition checked while compiling.
   *
   * It reads its own `@` and stands where a declaration stands, which is what tells it apart from
   * the annotations of `AttrParser`: those describe the function written under them, and this
   * describes nothing but itself. That is also why it must be tried **before** `attributedDecl` —
   * that rule ends in a refusal saying an annotation marks a function, which is exactly the wrong
   * thing to say about this one.
   *
   * The message is raised **inside** the parentheses rather than after them, by the rule a dead
   * `err` taught: a form that reaches further along the line outranks one that failed earlier, so a
   * sentence written past the point of divergence is never the one reported.
   */
  protected lazy val assertDecl: PackratParser[Stmt] =
    at(op("@") ~> attrWord("assert") ~> (missingAssertParens |
      op("(") ~>
      (expression ~ opt(op(",") ~> strLit) | err(
        "'@assert' takes a condition the compiler can settle, and an optional message: " +
          "'@assert(sizeof(T) == 16, \"why\")'")) <~ op(")") ^^ {
      case (e: Expr) ~ (m: Option[?]) =>
        AssertDecl(e, m.collect { case StrLit(s) => s })
    }))

  /** `@assert cond` — the parentheses left off, which without this rule is answered by a sentence
   * saying `@assert` is not an annotation.
   *
   * That message comes from `unknownAttr`, and it is reached because this rule declines at the `(`
   * and `attributedDecl` is tried next: the two land on the same token, and at equal positions the
   * later one wins. What it then prints is a roster of every annotation the language has, with this
   * one absent from it — so a reader comparing their line against the list concludes there is no
   * `@assert`, when the whole of their mistake is a missing bracket.
   *
   * The refusal is raised **before** the `(` rather than after it, so that it is this rule's `Error`
   * that survives: an `Error` outranks whatever the alternatives reach, but only while nothing has
   * consumed past the point they diverge at.
   */
  private lazy val missingAssertParens: Parser[Stmt] =
    not(op("(")) ~> err("'@assert' takes its condition in parentheses — " +
      "'@assert(sizeof(T) == 16)', with an optional message after a comma. It is the parentheses " +
      "that make it this declaration rather than an annotation about the one under it")

  /** `val name [: type] = value` — a binding that is written once (`07`, `reference/modules.md § val — a thing`).
   *
   * The **value is mandatory** and the type is not, which is the opposite arrangement from `const`
   * and for the opposite reason: a `val` with nothing to hold is not a declaration of anything,
   * while its type is readable off the value it was given. Whether a type may be left off is
   * nevertheless a question about *where* it was written — a module member states its interface —
   * and where is something only the analyzer knows, so the syntax accepts either and the rule is
   * applied there.
   */
  protected lazy val valDecl: PackratParser[Stmt] =
    multiDecl("val", mutable = false) |
      patternDecl("val", mutable = false) |
      op("val") ~> ident ~ opt(op(":") ~> typeRef) ~ (op("=") ~> initializer) ^^ {
        case n ~ t ~ v => ValDecl(n, t, Placeholders.lift(v))
      }

  /** A statement-level expression is the last of the three places a placeholder closes at
   * (`reference/expressions.md § _ — a parameter with the name left out`) — it is what stops one
   * from reaching past the statement it was written in.
   */
  protected lazy val exprStmt: PackratParser[Stmt] =
    expression ^^ (e => ExprStmt(Placeholders.lift(e)).setPos(e.pos))

  /** `a, b` standing alone — a function's result list as its trailing expression. It is tried
   * after the assignment form, which starts the same way and is settled by its `=`.
   */
  protected lazy val resultListStmt: PackratParser[Stmt] =
    expression ~ rep1(op(",") ~> expression) <~ endOfStatement ^^ { case e ~ more =>
      ExprStmt(ResultList(e :: more).setPos(e.pos))
    }

  /** That the list just parsed really was a whole line.
   *
   * Without it the form is greedy across a comma that belongs to something outside: an inline
   * `else` body is a statement, so `f(if c then a else b, x, y)` would read `b, x, y` as a result
   * list and leave the call one argument. A result list is the last thing on its line by
   * construction, so requiring that is exact rather than a heuristic.
   */
  protected lazy val endOfStatement: Parser[Unit] =
    guard(newline) | guard(dedent) | Parser(in =>
      if in.atEnd then Success((), in) else Failure("end of statement expected", in))

  protected lazy val returnStmt: PackratParser[Stmt] =
    op("return") ~> opt(resultValue) ^^ Return.apply

  /** `become f(…)` — a call that replaces this frame rather than adding to it
   * (`reference/declarations.md § become — a call that replaces the frame`).
   *
   * **A soft word rather than a reserved one**, so `become` goes on being a legal identifier
   * everywhere else. It is unambiguous where it stands: two identifiers in a row are not otherwise a
   * statement, and a call written through an index or a field starts with an identifier too — so
   * `become handlers[op](vm)` reads as one form and `become(x)`, a call to a function of that name,
   * reads as the other.
   *
   * The lookahead is what makes that true. Without it a `become` read as the keyword would commit
   * the line, and `become = 1` — an assignment to an ordinary variable of that name — would be a
   * parse error about a call. `guard` costs one token and gives the word back.
   */
  protected lazy val becomeStmt: PackratParser[Stmt] =
    guard(softWord("become") ~ (ident | op("("))) ~> softWord("become") ~> expression >> {
      case c: Call => success(Become(c))
      case _ =>
        err("'become' takes a call — it is the call that replaces this frame, so there has to be " +
          "one. 'return' is what hands a value back without replacing anything")
    }

  /** What a function hands back: one expression, or the several its result list declares. */
  protected lazy val resultValue: PackratParser[Expr] =
    expression ~ rep(op(",") ~> expression) ^^ {
      case e ~ Nil  => Placeholders.lift(e)
      case e ~ more => ResultList((e :: more).map(Placeholders.lift)).setPos(e.pos)
    }

  protected lazy val breakStmt: PackratParser[Stmt] =
    op("break") ~> opt(labelRef) ~ opt(expression) ^^ { case lbl ~ v => Break(lbl, v) }

  protected lazy val continueStmt: PackratParser[Stmt] =
    op("continue") ~> opt(labelRef) ^^ (lbl => Continue(lbl))

  /** `defer stmt` — what to run on the way out of this block (`reference/memory.md § Where defer
   * sits`).
   *
   * What follows is an inline statement, so the whole form is one line: the deferred thing is a
   * release, and a release that needs a block of its own is a function worth naming. Reading it as
   * `inlineStatement` rather than `expression` is what lets `defer xs.close()` and
   * `defer n = 0` both be written, without a result list's comma reaching across the `defer`.
   */
  protected lazy val deferStmt: PackratParser[Stmt] =
    op("defer") ~> inlineStatement ^^ Defer.apply

  /** `asm` with an architecture arm per line under it (`reference/inline-assembly.md § One arm per
   * architecture`).
   *
   * Every word the construct spends is contextual, `asm` included: each is recognized in one
   * position and is an ordinary identifier everywhere else, so a program may still call a variable
   * `out` or `clobbers` — and use it as an operand in the same function. What commits this rule is
   * the indented arm list, which a bare mention of a variable named `asm` does not have, so the
   * fall-through to an expression statement is exact rather than a matter of ordering.
   */
  protected lazy val asmStmt: PackratParser[Stmt] =
    softWord("asm") ~> newline ~> indent ~> skipNewlines ~>
      repsep(asmArm, newlines) <~ skipNewlines <~ dedent ^^ AsmStmt.apply

  /** `[x86_64, aarch64]` and what answers for them. The architecture names are not checked here —
   * the grammar has no idea which processors exist, and a name outside the set is a diagnostic
   * about a target rather than a parse error about a token.
   */
  protected lazy val asmArm: Parser[AsmArm] =
    at((op("[") ~> rep1sep(ident, op(",")) <~ op("]")) ~ asmArmBody ^^ { case archs ~ body =>
      AsmArm(archs, body)
    })

  /** The four things an arm may be: no answer, one instruction inline, an indented block, or
   * nothing at all. Nothing at all is last because it consumes no input and would otherwise take
   * every arm; it is the architecture on which the operation costs no instruction.
   */
  protected lazy val asmArmBody: Parser[AsmBody] =
    (softWord("unavailable") ~> asmText ^^ AsmUnavailable.apply) |
      (asmText ^^ (line => AsmCode(List(line), Nil, Nil))) |
      (newline ~> indent ~> skipNewlines ~> rep1sep(asmItem, newlines) <~ skipNewlines <~ dedent ^^ gatherAsm) |
      success(AsmCode(Nil, Nil, Nil))

  /** A line inside an arm: an instruction, an operand, or what the arm destroys. They are collected
   * by kind rather than kept in order, because only the instructions have an order that matters.
   */
  protected lazy val asmItem: Parser[AsmItem] =
    (asmText ^^ AsmItem.Line.apply) |
      (asmOperand ^^ AsmItem.Operand.apply) |
      (softWord("clobbers") ~> rep1sep(asmText, op(",")) ^^ AsmItem.Clobber.apply)

  /** `in name : reg` / `out name : "dx"`.
   *
   * The class slot is required even though `reg` is the only class there is, so that every operand
   * line has one shape and a second class arrives as a peer rather than as the exception to an
   * invisible default. Its `:` is not a type annotation — the operand names a variable that already
   * has a type — so what follows is a class or a machine register and never a type.
   */
  protected lazy val asmOperand: Parser[AsmOperand] =
    at((asmDir ~ ident <~ op(":")) ~ asmPlace ^^ { case dir ~ name ~ place =>
      AsmOperand(dir, name, place)
    })

  /** `in` is a reserved word already, for `for x in xs`, and is reused rather than added to. */
  protected lazy val asmDir: Parser[AsmDir] =
    (op("in") ^^^ AsmDir.In) | (softWord("out") ^^^ AsmDir.Out)

  /** A bare word is sysl's and a quoted one is the assembler's, which is the rule everywhere in the
   * construct: `reg` is a class this language names, and `"dx"` is a register only the assembler
   * knows about.
   */
  protected lazy val asmPlace: Parser[Option[String]] =
    (softWord("reg") ^^^ None) | (asmText ^^ Some.apply)

  protected lazy val asmText: Parser[String] =
    accept("string literal", { case t: lexical.StrLit => t.value })

  private def gatherAsm(items: List[AsmItem]): AsmCode =
    AsmCode(
      items.collect { case AsmItem.Line(t) => t },
      items.collect { case AsmItem.Operand(o) => o },
      items.collect { case AsmItem.Clobber(rs) => rs }.flatten,
    )

  /** A `'name` label reference, as used before a loop and after `break`/`continue`. */
  protected lazy val labelRef: Parser[String] =
    accept("label", { case t: lexical.Label => t.name })

  /** The label a loop may carry, which is written immediately before the loop's own keyword and
   * nowhere else.
   *
   * Reading it only where one of those follows is what keeps a stray label from being reported as a
   * loop nobody was writing. Without the lookahead, every labelled form takes the label and then asks
   * for its keyword on the token after it — so `break 'a 'b`, whose real problem is the second label,
   * was told `'for' expected` against the end of the line. The lookahead is `asOneToken` for the same
   * reason: what it crossed to find out is the label, and a refusal recorded past that would be the
   * same complaint one token along.
   */
  protected lazy val loopLabel: Parser[Option[String]] =
    opt(asOneToken(labelRef <~ guard(op("for") | op("while") | op("loop") | op("do"))))
}
