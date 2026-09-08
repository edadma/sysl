package sh.sysl

/** What a declaration looks like: parameters, functions and `extern`s, structs and their members,
 * enums and their variants, constrained type declarations, traits, and `impl` blocks.
 *
 * They are one area because they share one shape. Every form here is a name, an optional generic
 * parameter list, something in parentheses or indented under it, and an optional `end` marker naming
 * what it closes — so the rules that read those parts (`boundedTypeParams`, `paramList`, `suite`,
 * `checkedEndName`) are written once and reached from all of them. A member is the clearest case:
 * the same `methodTail` serves a struct's method, an enum's, a trait's requirement, and an `impl`'s
 * definition, because in this language those differ in where they appear and not in how they read.
 */
/** The two names the setter form needs on both sides of the parser, and the one rule about them.
 *
 * A **source name cannot hold `Modules.sep`**, so a setter filed under `count$set` is out of reach of
 * every lookup for a written name and collides with nothing a program can spell. What that buys is
 * that a setter is an ordinary method everywhere after the parser; what it costs is one rule, and it
 * is the rule 0139 is a card about: **no diagnostic may print the filed name.** Everything that has
 * to name one says *the setter of `count`*, which `sourceName` is here to make easy.
 */
object DeclParser {

  /** The name a setter is filed under, given the property it writes. */
  def setterName(name: String): String = s"$name${Modules.sep}set"

  /** The placeholder a setter's parameter carries until the property it pairs with supplies the
   * type. Nothing resolves it: `hoistMemberList` replaces it, and a setter with no getter is
   * refused there rather than reaching name resolution with this in hand.
   */
  val setterValue: String = s"${Modules.sep}value"

  /** Whether a filed member name is a setter's. */
  def isSetter(name: String): Boolean = name.endsWith(s"${Modules.sep}set")

  /** The name a setter was written with, for a diagnostic. Any other name is its own. */
  def sourceName(name: String): String =
    if isSetter(name) then name.dropRight(4) else name
}

trait DeclParser extends ExprParser {

  /** One `name: type` binding — a function parameter or a struct field.
   *
   * The name may be a **reserved word**, and what happens then is worth a rule rather than a parse
   * failure. Neither position has anything to say about the word itself: a struct's body ends, and
   * the reader is told `dedent expected` at a line that is plainly a field; a parameter list closes,
   * and the reader is told `')' expected` at a token that is plainly a parameter. Both messages are
   * about layout, which is the one thing that is not wrong.
   *
   * `reservedBinding` carries the lookahead and the reasons for its shape. **It is written first and
   * `ident` last**, because the two land on one position and the last candidate wins a tie there: a
   * field with no name at all — `: int` — must still say `identifier expected`, which is `ident`'s
   * refusal and not this rule's.
   */
  protected lazy val param: Parser[Param] =
    at((reservedBinding("a parameter's name or a field's") | ident) ~
      (op(":") ~> typeRef) ^^ { case n ~ t => Param(n, t) })

  /** A function's parameter, which unlike a struct's field may say what a call that leaves it out
   * gets instead (`reference/declarations.md § Default parameters and named arguments`). The
   * default is a full `expression`, so a call, a conditional, or anything else that yields a value
   * may stand there; whether it *may* — a suffix, naming nothing local, reaching as far as the
   * declaration does — is the analyzer's, since all three are questions about meaning rather than
   * about shape.
   */
  protected lazy val funcParam: Parser[Param] =
    restParam | at((byNameParam | param) ~ opt(op("=") ~> expression) ^^ {
      case p ~ d => p.copy(default = d.map(Placeholders.lift))
    })

  /** `xs: ...T` — the parameter that collects the call's trailing arguments
   * (`reference/declarations.md § A parameter may collect the rest of the call`).
   *
   * **The type it carries is the `[]const T` the body sees**, built here rather than left for the
   * analyzer, so that everything downstream reads an ordinary slice parameter and only the call
   * site knows this was written differently. Read-only because the values are the caller's, laid
   * out for the length of the call and not the callee's to write over.
   *
   * It takes no default: what a call that leaves it out gets is the empty slice, which is the whole
   * of what "collects the rest" means when there is no rest. Saying it twice could only disagree,
   * and the refusal below is where that is said.
   *
   * The C ellipsis is a different form and is read in `paramList`: that one is a bare `...` with no
   * name and no type, and its tail is walked with `va_arg` rather than handed over as a slice.
   */
  private lazy val restParam: Parser[Param] =
    at((ident <~ op(":") <~ op("...")) ~ typeRef ^^ {
      case n ~ t => Param(n, ArrayType(None, t, readOnly = true), rest = true)
    }) <~ (op("=") ~> err("a parameter that collects the rest of the call declares no default — a " +
      "call that leaves it out gets the empty slice, which is what 'the rest' means when there is " +
      "none, and a default beside that is a second answer to a settled question") | success(()))

  /** `x: -> T` — a parameter passed **by name** (`reference/declarations.md § Default parameters
   * and named arguments`).
   *
   * The arrow with nothing on its left is the nullary case of the bare-arrow sugar a parameter
   * already has: `f: A -> B` is `[F: Fn(A) -> B](f: F)`, and this is that at arity zero. So the type
   * it builds is the one `x: () -> T` builds, and only `byName` separates them — the second is a
   * callable the caller constructs, the first is an expression the caller writes bare and the call
   * does not evaluate.
   *
   * It belongs to `funcParam` rather than to `param`, so a **struct field** cannot be written this
   * way. A field is storage, and storage holding an unevaluated expression is a different feature
   * from this one.
   */
  private lazy val byNameParam: Parser[Param] =
    at(ident ~ (op(":") ~> op("->") ~> typeRef) ^^ {
      case n ~ r => Param(n, FnType(Nil, r, bare = true), byName = true)
    })

  /** A struct's field, which has no default to declare. Said here rather than left to whatever the
   * grammar happened to want where the `= v` was written, because writing one is a reasonable thing
   * to try and the reason it is refused is not guessable from a complaint about shape.
   */
  protected lazy val fieldParam: Parser[Param] =
    param <~ (op("=") ~> err("a field declares no default — what an unwritten field gets is decided " +
      "by the constructor that builds the value, not by the field") | success(()))

  /** A function declaration, Scala-style but keyword-less: `name[T…](params) -> ret = expr` or
   * a block body, `-> ret` optional (absent ⇒ `unit`). It is tried before an expression
   * statement; a bare call `foo(1)` fails here (its arguments are not `name: type` bindings,
   * and nothing follows to open a body) and falls through to `exprStmt`.
   */
  /** A calling convention written before a definition: `interrupt handler()`, or with the privilege
   * mode a processor distinguishes, `interrupt(supervisor) handler()` (`reference/ffi.md §
   * interrupt`).
   *
   * `interrupt` is a soft keyword, and the trailing `guard(ident)` is the whole of what keeps it one.
   * Three things start with that word and only the first is a convention: `interrupt timer()`
   * declares a handler, `interrupt(n: int) -> int` declares a *function called* `interrupt`, and
   * `interrupt(4)` calls one. Requiring a name after the modifier tells them apart with no
   * backtracking a reader could feel — and consuming nothing when it fails is what lets the other
   * two go on to parse as themselves.
   */
  protected lazy val callConv: Parser[CallConv] =
    at((softWord("interrupt") ~> opt(op("(") ~> ident <~ op(")")) <~ guard(ident))
      ^^ (CallConv("interrupt", _)))

  protected lazy val funcDecl: PackratParser[Stmt] =
    opt(callConv) ~ ident ~ opt(boundedTypeParams) >> { case conv ~ name ~ tps =>
      val tp = tps.getOrElse(TypeParams.none)
      (op("(") ~> paramList <~ op(")")) ~ opt(op("->") ~> resultRef) ~ whereOn(tp) ~ funcBody <~
        endName(name) ^^ {
        case ((params, variadic)) ~ ret ~ tpw ~ body =>
          FuncDecl(name, tpw.names, params, ret, body, tpw.bounds, variadic, tdefaults = tpw.defaults,
                   tvalues = tpw.values, tpacks = tpw.packs, conv = conv)
      }
    }

  /** `extern name(params) -> ret` — a header with no body at all, which is what tells it from a
   * function declaration — or `extern name: type`, the same seam pointed at a variable the other
   * side exports rather than a function. The result is optional and absent means `unit`, exactly as
   * for a function; `-> never` says the callee does not come back.
   *
   * What follows the name is what decides which of the two this is, so the name and its optional
   * link name are read once and both forms continue from there. That is also what makes the refusal
   * below able to name both: having got as far as an identifier, the declaration is an `extern`
   * whatever comes next, and the only question left is which kind.
   *
   * A string before the name is the *symbol*, and the name after it is what the program calls it by:
   * `extern "snprintf" fmt(…)` resolves to libc's `snprintf` without spending the name `snprintf`.
   * A leading string is unambiguous — a declaration otherwise begins with an identifier — so this
   * costs no keyword. Haskell's `foreign import ccall "snprintf" c_snprintf` is the same shape.
   */
  protected lazy val externDecl: PackratParser[Stmt] =
    op("extern") ~> opt(linkName) ~ ident ~ noTypeParams >> { case link ~ name ~ _ =>
      (op("(") ~> paramList <~ op(")")) ~ opt(op("->") ~> typeRef) ^^ {
        case ((params, variadic)) ~ ret => ExternDecl(name, params, ret, variadic, link)
      } |
        // The type is read *after* the colon has been consumed, so a mistake in it is reported as
        // the mistake it is rather than as this rule failing back to the sentence below.
        op(":") ~> (typeRef ^^ (t => ExternVarDecl(name, t, link)) |
          err(s"an 'extern' variable states the type the other side laid down — 'extern $name: T'")) |
        err(s"an 'extern' declares a function — '$name(params) -> result' — or a variable — " +
          s"'$name: type' — so what follows the name is '(' or ':'")
    }

  /** An `extern` may not declare type parameters. Monomorphization needs a body to specialize, and a
   * foreign symbol is one function at one signature however many sysl types would fit it — so the
   * bracketed list has nothing to do here, and saying that is worth more than the "'(' expected" a
   * grammar with no place for one would give. A program wanting one signature per type declares one
   * `extern` per type, each under its own name with the same link name.
   */
  protected lazy val noTypeParams: Parser[Unit] =
    op("[") ~> err("an 'extern' declares no type parameters — there is no body to monomorphize, and " +
      "a foreign symbol is one function at one signature") |
      success(())

  protected lazy val linkName: Parser[String] =
    accept("symbol name", { case t: lexical.StrLit => t.value })

  /** A declared parameter list, which may end in `...` — the C ellipsis, and the one arity a
   * declaration does not fix. Shared by `extern` and by a sysl function, which may be variadic too.
   * The `...`-only form parses so the analyzer can say why a variadic needs a named parameter before
   * it, rather than the grammar reporting a stray token.
   */
  protected lazy val paramList: Parser[(List[Param], Boolean)] =
    op("...") ^^^ (Nil, true) |
      // The variadic marker is tried before the trailing comma, so `f(a: int, ...)` still reads the
      // comma as the separator it is; `f(a: int,)` falls through to the trailing-comma case.
      repsep(funcParam, op(",")) ~ opt(op(",") ~> op("...")) <~ opt(op(",")) ^^ { case ps ~ dots =>
        (ps, dots.isDefined)
      }

  /** A function body is either an `= expr` short form (whose value is the return value) or an
   * indented block (whose trailing expression is the return value).
   */
  protected lazy val funcBody: PackratParser[List[Stmt]] =
    op("=") ~> (suite | resultValue ^^ (e => List(ExprStmt(e).setPos(e.pos)))) | suite

  /** One parsed line of a struct body, before the three kinds are sorted into their own lists. */
  private enum StructPart:
    case Fld(f: Param)
    case Mem(m: MethodDecl)
    case Inv(e: Expr)

  /** `opaque`, the modifier that withholds a struct's layout from every module but the one
   * declaring it (`reference/ffi.md § opaque`). A soft keyword: `opaque` is an ordinary word — an
   * alpha channel's fully-`opaque` end is the obvious field to want it for — and a language that
   * spent it would be taking a name away to save itself a lookahead.
   */
  protected lazy val opaqueKw: Parser[Unit] = softWord("opaque")

  /** `derives Eq, Ord, Hash, Display` — the traits the compiler writes a memberwise implementation
   * of, on the declaration of the type it writes them for.
   *
   * A **soft** keyword, for the reason `opaque` and `invariant` are: `deriving` is an ordinary word
   * and a language that spent one to save itself a lookahead is taking a name away from every
   * program that had a use for it. There is no ambiguity to trade against here — nothing else may
   * follow a type's header, so a word in this position can only be this clause.
   *
   * Each entry is read as a `boundRef`, which is what a bound is read as everywhere else: it carries
   * its own position, so a refusal about one trait out of four points at that one. A derived trait
   * takes no arguments, and `Deriving` is where that is said.
   */
  protected lazy val derivingClause: Parser[List[BoundRef]] =
    opt(softWord("derives") ~> rep1sep(boundRef, op(","))) ^^ (_.getOrElse(Nil))

  /** A **type pack** stands for a list of types and there is one place to write the list out —
   * `(..A)`, the tuple of it (`reference/generics.md § A parameter may stand for a list of types`).
   * A declaration whose parameters *are* its shape has nothing to do with one: a struct of a pack
   * would be a tuple with a name, which is the thing a program writes instead when the arity stops
   * being incidental.
   *
   * Raised where the parameter list closes rather than left to fail on the pack's use, since the use
   * is what a reader would then be sent to look at.
   */
  protected def noPacks(tp: TypeParams, what: String): Parser[Unit] =
    if tp.packs.isEmpty then success(())
    else
      err(s"'..${tp.packs.head}' is a type pack, and $what has no way to spread one over its own " +
        s"shape — a product of however many parts is a tuple, and '(..${tp.packs.head})' is how an " +
        "'impl' or a function takes one")

  protected lazy val structDecl: PackratParser[Stmt] =
    opt(opaqueKw) ~ (op("struct") ~> ident) ~ opt(boundedTypeParams) ~ derivingClause >> {
      case hidden ~ name ~ tps ~ derives =>
      val tp     = tps.getOrElse(TypeParams.none)
      val opaque = hidden.isDefined

      // Which of the three this is **decides** how the rest is read, so it is settled by lookahead
      // and committed to with `>>`. Written as an alternation it would not work: a body whose first
      // line is bad fails deep inside the file, and a combinator choice keeps whichever alternative
      // reached furthest — so the sentence below, raised back at the declaration, would lose to it
      // and a struct with a mistake in its body would be reported as having no body. Neither guard
      // consumes, so both are asked at the same position and at most one of them can succeed.
      noPacks(tp, "a struct") ~> whereOn(tp) >> { tp =>
      (opt(guard(newline ~ indent)) ~ opt(guard(onNextLine(softEnd)))) >> {
        case Some(_) ~ _ =>
          (newline ~> indent ~> skipNewlines ~> rep1sep(structItem, newlines) <~ skipNewlines <~ dedent) <~
            endName(name) ^^ { items =>
              val fields     = items.collect { case StructPart.Fld(f)  => f }
              val members    = items.collect { case StructPart.Mem(m)  => m }
              val invariants = items.collect { case StructPart.Inv(e)  => e }
              StructDecl(name, tp.names, fields, members, tp.bounds, invariants,
                         tdefaults = tp.defaults, opaque = opaque, tvalues = tp.values,
                         deriving = derives)
            }

        // A struct with **no fields**, whose emptiness is *written* rather than inferred from an
        // absence: an `end Name` and nothing between it and the declaration. A type like this is
        // one value carrying no data, which is what a sink standing for a fixed destination — the
        // console, a UART — has to be to be a value at all. Requiring the marker is what keeps a
        // misindented body from quietly becoming one; a lone `struct Name` is still the mistake it
        // has always been, and still says so below.
        case None ~ Some(_) =>
          endName(name) ^^ { _ =>
            StructDecl(name, tp.names, Nil, Nil, tp.bounds, Nil,
                       tdefaults = tp.defaults, opaque = opaque, tvalues = tp.values,
                       deriving = derives)
          }

        // A struct with **no body at all**, which only an `opaque` one may be: it is C's incomplete
        // type, `struct sqlite3;`, and it is what a handle from a C library should be declared as.
        // Nothing in sysl lays one out — the storage belongs to whoever allocated it — so the
        // declaration exists to give `*sqlite3` a type of its own that a `*u8` cannot be mistaken for.
        case _ if opaque =>
          success(StructDecl(name, tp.names, Nil, Nil, tp.bounds, Nil,
                             tdefaults = tp.defaults, opaque = true, tvalues = tp.values,
                             deriving = derives))

        case _ =>
          err(s"'struct $name' declares no fields — a struct's body is indented under it, a type " +
            s"with no fields at all is written 'struct $name' closed by 'end $name', and one with " +
            s"no layout of its own is written 'opaque struct $name'")
      }
      }
    }

  /** A line inside a struct body is a member declaration, an `invariant <bool>` clause, or a
   * `name: type` field. A member is tried first — it needs a `(` (a method or associated function)
   * or a `->` (a property) after the name; an `invariant` clause next — it is the contextual word
   * `invariant` followed by an expression; and a bare field falls through to `param` (so a field
   * may still be named `invariant`, since `invariant: type` matches neither of the first two).
   *
   * A field and a member may each carry a **visibility modifier** (`reference/modules.md §
   * Visibility`), written in the same place and the same spellings a top-level declaration writes
   * one. An `invariant` clause declares no name, so like an `impl` block it takes none.
   */
  private lazy val structItem: Parser[StructPart] =
    restrictedMember ^^ (StructPart.Mem(_)) |
      invariantClause ^^ (StructPart.Inv(_)) |
      visibility ~ fieldParam ^^ { case v ~ f => StructPart.Fld(f.copy(vis = v).setPos(f.pos)) }

  /** A member of a type's own body, which is the one kind that may say how far it is visible.
   *
   * It opens with `memberAttrs`, which reads the three a member may carry — `@crossing`, `@reads`
   * and `@writes`, the ones that are about a parameter — and answers everything else with the
   * sentence that rule carries. A struct's body and an enum's both read their lines through here,
   * and a member is the first alternative each tries, so the refusal is reached wherever the `@` is
   * — including the position a field or a variant would have been read at.
   */
  protected lazy val restrictedMember: PackratParser[MethodDecl] =
    memberAttrs >> { as =>
      // **Reading an annotation commits the line to being a member**, which is what keeps a
      // `@crossing` written above a *field* from falling back into the field rule and being reported
      // as a missing identifier. The three say something about a parameter, and a field and a
      // variant have none — so there is nothing for the alternation to still try.
      if as.isEmpty then plainMember
      else (fieldAhead ~> notAMember) | plainMember ^^ (attributedMember(_, as))
    }

  /** Whether the line the annotations stand above is a **field** — `name: type`, with or without a
   * visibility modifier in front of it.
   *
   * It is a lookahead rather than an `err` after the member form, and the reason is the one a dead
   * `err` taught: `member` reads the name and fails at the `->` it wanted, which is a position
   * further along than the `@`, and a `Failure` there **outranks** an `Error` raised back at the
   * annotation. So the refusal has to be reached *before* the member form is tried, which means
   * deciding on the shape rather than on the failure — and the field shape is the one that can be
   * recognised without ambiguity. A variant carrying fields is spelled `A(x: int)`, which is a
   * method's shape exactly, so it is left to the member form and the sentence it produces.
   */
  private lazy val fieldAhead: Parser[Unit] = guard(visibility ~> ident ~ op(":")) ^^^ (())

  private lazy val plainMember: PackratParser[MethodDecl] =
    visibility ~ (noOverride ~> (setter | member)) ^^ { case v ~ m => m.copy(vis = v).setPos(m.pos) }

  private def notAMember: Parser[MethodDecl] =
    err("'@crossing', '@reads' and '@writes' are about a parameter, so they stand above a method or " +
      "an associated function — a field and a variant have none, and there is nothing for one of " +
      "them to name there")

  /** The refusal a trait's member and an `impl`'s share (`reference/modules.md § Visibility`). Both
   * are reached at the reach the *trait* has — one asks for the member and the other supplies what
   * was asked — so there is nothing here for a modifier to decide, and saying that is worth more
   * than whatever the grammar happened to want where the modifier was written.
   */
  protected lazy val noVisibility: Parser[Unit] =
    op("private") ~> err("a trait's members and an 'impl' block's carry no visibility of their own — a " +
      "trait's member is as visible as the trait, and an implementation supplies what the trait asked for") |
      success(())

  /** The refusal a **trait's** member and a **type's own** member share: neither can be replacing
   * anything, so neither may say `override` (`reference/traits.md § Replacing a default says override`).
   *
   * A trait's member is where a default body is *written*, not where one is replaced. A member of a
   * type's own body implements no trait — a name a trait also declares is a collision there and is
   * reported as one — so an implementation replaces a default inside the `impl` block that keeps the
   * promise, which is the one place the keyword belongs.
   */
  protected lazy val noOverride: Parser[Unit] =
    op("override") ~> err("'override' says a member replaces a body its trait supplied, and it is " +
      "written on that member inside the 'impl' block — a trait's own member is where a default is " +
      "written rather than replaced, and a member of a type's body implements no trait") |
      success(())

  /** `invariant <bool>` among a struct's fields: a condition every value of the struct must satisfy,
   * re-checked whenever the struct is built or one of its fields is written. Bare field names are in
   * scope. `invariant` is contextual — an ordinary identifier everywhere else. */
  protected lazy val invariantClause: Parser[Expr] = invariantKw ~> expression

  protected lazy val invariantKw: Parser[Unit] = softWord("invariant")

  /** A member of a type's body. What follows the name decides the kind: `(params)` is a method
   * (or, with no `self`, an associated function), and `-> type = body` with no parameter list is
   * a computed property.
   *
   * A member may declare **type parameters of its own**, in the same bracketed list every other
   * generic declaration writes and in the same position — directly after the name. They are the
   * member's, not the type's: a call fixes them from what it passes, while the type's own are
   * already fixed by the receiver.
   */
  protected lazy val member: PackratParser[MethodDecl] =
    at(
      staticProperty |
        (ident ~ opt(boundedTypeParams) >> { case name ~ tps =>
          methodTail(name, tps.getOrElse(TypeParams.none)) |
            (if tps.isEmpty then propertyTail(name)
             else failure("a property takes no type parameters"))
        }),
    )

  /** `static count -> int` — a property of the **type** rather than of a value
   * (`reference/declarations.md § A static property`), read as `Type.count` with no parentheses.
   *
   * **A property has nowhere to say `self`, which is why this needs a word at all.** A member's
   * receiver is written in its parameter list, and a property is *"a method with the parameter list
   * left off"* — so the one thing that distinguishes an instance member from an associated one
   * cannot be spelled, and every property is an instance member by construction. `static` is what
   * says otherwise.
   *
   * **The word is the language's own, already reserved and already meaning this.** `static val` in
   * an entry file says a binding belongs to the module rather than to that file's body; this says a
   * member belongs to the type rather than to a value of it. So no reserved word is added, the
   * highlighting grammar needs no entry, and `reference/lexical.md`'s count does not move.
   *
   * **Inferring it from the body was refused.** "A property that never names `self` is static" would
   * make a member's reachability depend on its body, so deleting a `self.` from an expression would
   * silently move the member from the value to the type and break every call site. What a member is
   * has to be said.
   *
   * It takes no type parameters, for the reason an ordinary property does not: there is nothing at
   * the read to solve them from, a read having no arguments and no receiver.
   */
  protected lazy val staticProperty: PackratParser[MethodDecl] =
    (op("static") ~> ident) ~ opt(boundedTypeParams) >> { case name ~ tps =>
      if tps.nonEmpty then failure("a property takes no type parameters")
      else
        // The refusal is raised on the *lookahead* rather than after the form, because an `err`
        // written after an alternative that got further along the line is never reached — a
        // `Failure` at the later position outranks an `Error` back here. A missing body must stay
        // an ordinary `Failure` for the same reason in reverse: `staticPropertySig` is tried below
        // this in a trait's body, and an `Error` here would abort the alternation before it.
        guard(op("(")) ~> err(s"'static $name' declares a property of the type, which has a result " +
          s"and no parameter list — 'static $name -> T'. A member with a parameter list is an " +
          "associated function already, reached the same way and called with '()'") |
          (op("->") ~> (opaqueRef | typeRef)) ~ funcBody <~ endName(name) ^^ {
            case ret ~ body =>
              MethodDecl(name, None, isProperty = true, Nil, Nil, Some(ret), body, isStatic = true)
          }
    }

  /** `static lowest -> int` in a **trait's** body — the requirement, with no body to answer it.
   *
   * It stands to `staticProperty` exactly as `propertySig` stands to `propertyTail`, and it is a
   * rule of its own for the same reason: a trait's member is a signature, and the body every other
   * spelling of this form insists on is the one thing it must not have.
   */
  protected lazy val staticPropertySig: PackratParser[MethodDecl] =
    at((op("static") ~> ident) ~ (op("->") ~> (opaqueRef | typeRef)) ^^ { case name ~ ret =>
      MethodDecl(name, None, isProperty = true, Nil, Nil, Some(ret), Nil, isStatic = true)
    })

  protected def methodTail(name: String, generics: TypeParams): Parser[MethodDecl] =
    (op("(") ~> methodParams <~ op(")")) ~ opt(op("->") ~> resultRef) ~ whereOn(generics) ~
      funcBody <~ endName(name) ^^ {
      case (recv, params, variadic) ~ ret ~ g ~ body =>
        MethodDecl(name, recv, isProperty = false, g.names, params, ret, body, g.bounds,
          g.defaults, variadic = variadic, tvalues = g.values, tpacks = g.packs)
    }

  /** A property takes the same `funcBody` a method does, so `name -> T` may be answered by an
   * `= expr`, by an `=` opening an indented block, or by a block with no `=` at all. A property is a
   * function with the parameter list left off, and having only the one-expression spelling made it
   * the one member whose body could not be written out — which bit a default property in a trait
   * the same way it bit an inherent one.
   */
  protected def propertyTail(name: String): Parser[MethodDecl] =
    (op("->") ~> (opaqueRef | typeRef)) ~ funcBody <~ endName(name) ^^ {
      case ret ~ body => MethodDecl(name, None, isProperty = true, Nil, Nil, Some(ret), body)
    }

  /** `set count(x)` — the write half of a property (`reference/declarations.md § A property may be
   * settable`).
   *
   * **`set` is a soft keyword**, read only where a member declaration begins, so the word stays an
   * ordinary name everywhere else — a set is a container the library may yet want, and taking the
   * word outright to introduce one member would be a poor trade.
   *
   * **The parameter is named and untyped.** Its type is the getter's result and can be nothing else,
   * so writing it would be a second place for one fact to live and a disagreement to diagnose;
   * `hoistMemberList` fills it in from the property this pairs with. Swift, Kotlin and C# all leave
   * it out for the same reason, and all three then differ from this one by leaving the *name* out
   * too — an unwritten binding appearing in a body is the wart every one of them carries.
   *
   * The declaration it becomes is an ordinary **method**: a `*self` receiver, one parameter, and a
   * name holding `Modules.sep` so that nothing a program spells can reach it and no lookup for the
   * written name finds it by accident. Conformance, visibility, lowering and the method table then
   * need to know nothing about setters at all.
   */
  protected lazy val setter: PackratParser[MethodDecl] =
    at(
      (softWord("set") ~> ident) ~ (op("(") ~> ident <~ op(")")) >> { case name ~ param =>
        funcBody <~ endName(name) ^^ { body =>
          MethodDecl(DeclParser.setterName(name), Some(RecvMode.ByPtr), isProperty = false, Nil,
            List(Param(param, NamedType(DeclParser.setterValue))), None, body)
        }
      },
    )

  /** `set count(x)` with no body: a trait asking an implementation for the write half of a property.
   *
   * The parameter is named here exactly as a method signature's are, and for the same reason — the
   * name is the trait's way of saying what the value *is*, and an implementation is free to call it
   * something else. Its type is still the property's, so a trait asking for a setter is asking for a
   * property it also declares; `hoistMemberList` is where the two are put together.
   */
  protected lazy val setterSig: PackratParser[MethodDecl] =
    at(
      (softWord("set") ~> ident) ~ (op("(") ~> ident <~ op(")")) ^^ { case name ~ param =>
        MethodDecl(DeclParser.setterName(name), Some(RecvMode.ByPtr), isProperty = false, Nil,
          List(Param(param, NamedType(DeclParser.setterValue))), None, Nil)
      },
    )

  /** The parenthesised part of a method: an optional receiver shorthand (`self`, `*self`,
   * `&self`, `&sync self`) followed by ordinary `name: type` parameters. With no receiver the
   * member is an associated function.
   *
   * Either shape may end in the same trailing `...` a free function's list takes (`reference/ffi.md
   * § Variadic functions`), and for the same reason: a member is a function with a receiver in
   * front, so an ellipsis reaching one and not the other would be a difference in the grammar with
   * nothing behind it. With a receiver the ellipsis is tried after the parameters, so `add(self, n:
   * int, ...)` still reads its commas as the separators they are.
   */
  protected lazy val methodParams: Parser[(Option[RecvMode], List[Param], Boolean)] =
    receiver ~ rep(op(",") ~> funcParam) ~ opt(op(",") ~> op("...")) <~ opt(op(",")) ^^ {
      case r ~ ps ~ dots => (Some(r), ps, dots.isDefined)
    } |
      paramList ^^ { case (ps, variadic) => (None, ps, variadic) }

  protected lazy val receiver: Parser[RecvMode] =
    op("*") ~> op("self") ^^^ RecvMode.ByPtr |
      op("&") ~> softSync ~> op("self") ^^^ RecvMode.ByRef(sync = true) |
      op("&") ~> op("self") ^^^ RecvMode.ByRef(sync = false) |
      op("self") ^^^ RecvMode.ByValue

  /** `enum Name[T…]` with indented variants, and an optional `: iN` underlying-type annotation
   * that pins a simple enum's storage. A variant is a bare name (`Empty`), a name with an
   * explicit integer value (`Blue = 10`), or a name with a payload (`Circle(radius: int)`).
   */
  protected lazy val enumDecl: PackratParser[Stmt] =
    op("enum") ~> ident ~ opt(boundedTypeParams) ~ opt(op(":") ~> typeRef) ~ derivingClause >> {
      case name ~ tps ~ under ~ derives =>
      val tp = tps.getOrElse(TypeParams.none)

      noPacks(tp, "an enum") ~> whereOn(tp) >> { tp =>
      (newline ~> indent ~> skipNewlines ~> rep1sep(enumItem, newlines) <~ skipNewlines <~ dedent) <~ endName(name) ^^ {
        items =>
          val variants = items.collect { case Left(v)  => v }
          val members  = items.collect { case Right(m) => m }
          EnumDecl(name, tp.names, under, variants, members, tp.bounds, tdefaults = tp.defaults,
                   tvalues = tp.values, deriving = derives)
      }
      }
    }

  /** A line inside an enum body is either a variant or a member declaration, told apart the same
   * way a struct body's lines are: a member is tried first and needs a body to follow its header,
   * so `Circle(radius: int)` — a header with nothing after it — falls through to `enumVariant`.
   */
  protected lazy val enumItem: Parser[Either[EnumVariantDecl, MethodDecl]] =
    positionalPayload | restrictedMember ^^ (Right(_)) | enumVariant ^^ (Left(_))

  /** Whether a type is `self` under whatever a receiver may be written with, which is the one thing
   * `positionalPayload` has to stand aside for: `area(self)` and `write(*self, …)` are members, and
   * to a type parser they are a name and a pointer to one.
   */
  private def namesSelf(t: TypeRef): Boolean = t match
    case NamedType("self", _) => true
    case PtrType(inner)       => namesSelf(inner)
    case RefType(inner, _)    => namesSelf(inner)
    case _                    => false

  /** What a variant's parentheses say when they hold a bare **type** rather than `name: type`.
   *
   * **The habit it answers is a port rather than ignorance.** Rust, Swift, OCaml and Haskell all
   * take a positional payload, and a data enum is exactly the construct somebody is most likely to
   * be bringing from one of them — so `Url([]const u8)` is what gets written first. Card `0367`,
   * found twice in one file writing `sysl-lang/llhttp`.
   *
   * **It sits above `restrictedMember` because otherwise the message depends on the type**, and
   * three of the four answers were bad. Measured before it was moved: `[]const u8` and `[4]u8`
   * reached the variant parser and said `')' expected` with the caret on the `[` — a complaint that
   * reads as being about the type; `int` and `Buf[int]` were read as far as a member header and said
   * `':' expected`, which is fine; and **`*u8` and `&Node` said `'self' expected`**, which is worse
   * than either, because it tells the reader to write the one word that would not help. One rule
   * above all of them answers every shape the same way.
   *
   * **The suggestion is constructed rather than recited**: the type has been read by here, and
   * `TypeRef.show` writes its spelling back out, so the message names the line the reader should
   * have written instead of describing it.
   *
   * **The whole test is a `guard`, so nothing is consumed and the caret lands on the name** rather
   * than past the offending text. It declines on everything that is not this mistake: a named field
   * breaks at the `:`, empty parentheses have no type to read, and a receiver is `namesSelf`.
   */
  private lazy val positionalPayload: Parser[Either[EnumVariantDecl, MethodDecl]] =
    guard(ident ~ (op("(") ~> typeRef) <~ (op(")") | op(","))) >> { case _ ~ t =>
      if namesSelf(t) then failure("a receiver, not a payload")
      else
        err(s"a variant's payload names its fields, as 'Circle(r: real)' — write a name before the " +
          s"type, as 'name: ${t.show}'")
    }

  protected lazy val enumVariant: Parser[EnumVariantDecl] =
    at(
      ident ~ (op("(") ~> commaList(fieldParam) <~ op(")")) ^^ { case n ~ fs => EnumVariantDecl(n, None, fs) } |
        ident ~ (op("=") ~> expression) ^^ { case n ~ v => EnumVariantDecl(n, Some(v), Nil) } |
        ident ^^ (n => EnumVariantDecl(n, None, Nil)),
    )

  /** `type Name = [new] Base [within lo..hi] [where predicate]` — a constrained subtype (`16`).
   * `new`, `within`, and `where` are contextual: they are ordinary identifiers everywhere else, so
   * a function or field may still be named `where`, and are recognised as keywords only here.
   */
  protected lazy val typeDecl: PackratParser[Stmt] =
    op("type") ~> ident ~ (op("=") ~> opt(newKw) ~ typeRef ~ opt(withinClause) ~ opt(whereClause)) ^^ {
      case name ~ (nw ~ base ~ range ~ pred) => TypeDecl(name, base, nw.isDefined, range, pred)
    }

  /** `within lo..hi` (inclusive) or `within lo..<hi` (upper-exclusive). The `..<` token is a single
   * lexeme, so it is told from `..` by the tokenizer rather than here.
   */
  protected lazy val withinClause: Parser[RangeBound] =
    withinKw ~> boundLit ~ (op("..<") ^^^ true | op("..") ^^^ false) ~ boundLit ^^ {
      case lo ~ excl ~ hi => RangeBound(lo, hi, excl)
    }

  protected lazy val whereClause: Parser[Expr] = whereKw ~> expression

  /** A bound of a `within` range: any **constant expression**, which is the same thing an array
   * bound accepts — `within 0..<max_tasks` beside `[max_tasks]Task`, so a table's size and the
   * range of the type indexing it are one fact written once (`reference/modules.md § const — a
   * value`, `reference/errors.md § Ranges`).
   *
   * The level is `bitOr`, which is deliberately the one *tighter* than a range: `rangeExpr` is built
   * out of `bitOr`, so parsing a bound at any looser level would let `0..<max_tasks` be read as a range
   * expression and swallow the very operator that separates the two bounds. That ambiguity is what kept
   * this position at a literal, and naming the level is the whole of the fix.
   *
   * Whether the expression really is constant is not a question the grammar can answer — `n + 1` is a
   * fine parse and a fine constant when `n` is one — so it is answered where the bound is turned into a
   * number, which is also where its kind is checked against the base.
   */
  protected lazy val boundLit: Parser[Expr] = bitOr

  protected lazy val newKw: Parser[Unit]    = softWord("new")
  protected lazy val withinKw: Parser[Unit] = softWord("within")
  protected lazy val whereKw: Parser[Unit]  = softWord("where")

  /** An optional `where` clause folded into the parameter list it bounds, or a refusal naming the
   * parameter it could not bound (`reference/generics.md § A bound may be written out of line`).
   *
   * **The check is here rather than in the analyzer because the declaration's own list is the whole
   * of what decides it**, and the parser holds both at the moment the clause is read — so the caret
   * lands on the clause, which is what has to change.
   */
  protected def whereOn(tp: TypeParams): Parser[TypeParams] =
    opt(whereBounds) >> {
      case None => success(tp)
      case Some(clause) =>
        tp.whereUnknown(clause) match {
          case Some(why) => err(why)
          case None      => success(tp.withWhere(clause))
        }
    }

  /** A contextual keyword: an identifier spelled exactly `word`, matched where the grammar wants the
   * keyword but the word must stay a legal identifier everywhere else (the `sync` of `&sync T`).
   */
  protected def softWord(word: String): Parser[Unit] =
    accept(s"'$word'", { case t: lexical.Identifier if t.chars == word => () })

  /** `trait Name` with indented member declarations. Each is a method header — a receiver, a
   * parameter list, and an optional result — either bare, which requires an implementation to
   * supply it, or followed by a body, which supplies a **default** every `impl` inherits unless it
   * writes its own.
   *
   * `trait Name: Super + Other` names the traits this one **requires**, spelled exactly as a bound
   * on a type parameter is — the same `:` and the same `+` — because it asks the same thing of the
   * implementing type. A generic trait writes both: `trait Word[T]: Add`, the parameters first.
   *
   * **The members may be left out entirely, and only where the trait requires another.** Such a
   * trait declares no behaviour of its own; what it says is that its implementors have the ones it
   * names, which makes it a name for a family rather than for a capability — `Integer` is the case
   * this exists for, and a blanket `impl` written over it is what reads the name.
   *
   * Requiring the supertraits is what keeps the omission from being a hole. A trait with neither
   * members nor requirements says nothing at all, and a body indented by the wrong amount would
   * become one silently — so the form that means something is allowed and the form that cannot is
   * still the missing block it was.
   */
  protected lazy val traitDecl: PackratParser[Stmt] =
    op("trait") ~> ident ~ opt(boundedTypeParams) ~ opt(op(":") ~> rep1sep(boundRef, op("+"))) >> {
      case name ~ tps ~ supers =>
        val tp0 = tps.getOrElse(TypeParams.none)
        val body =
          (newline ~> indent ~> skipNewlines ~> rep1sep(traitItem, newlines) <~ skipNewlines <~ dedent) <~
            endName(name)

        def decl(tp: TypeParams)(items: List[Either[AssocDecl, MethodDecl]]) =
          TraitDecl(name, tp.names, items.collect { case Right(m) => m }, tp.bounds,
            supers.getOrElse(Nil), tdefaults = tp.defaults,
            assocs = items.collect { case Left(a) => a })

        noPacks(tp0, "a trait") ~> whereOn(tp0) >> { tp =>
          if supers.isEmpty then body ^^ decl(tp) else opt(body) ^^ (m => decl(tp)(m.getOrElse(Nil)))
        }
    }

  /** One line of a trait body: an associated type, or a member. The associated type is tried first
   * and cannot be confused with anything — `type` is reserved, so no member declaration can begin
   * with it.
   */
  protected lazy val traitItem: PackratParser[Either[AssocDecl, MethodDecl]] =
    assocSig ^^ (Left(_)) | traitMember ^^ (Right(_))

  /** `type Body: View` — a trait's **associated type**: a parameter the *implementation* supplies
   * rather than one written where the trait is applied.
   *
   * The bound list is spelled exactly as a type parameter's is, with the same `:` and the same `+`,
   * because it asks the same thing of the type that fills it. Writing none says the implementation
   * may supply anything.
   *
   * A trait writing `type Body = X` is refused by name: an associated type is what the trait leaves
   * *open*, so a trait supplying one has written an alias in the one place an alias means nothing.
   */
  protected lazy val assocSig: PackratParser[AssocDecl] =
    at(
      (op("type") ~> ident) >> { n =>
        op("=") ~> err(s"a trait's 'type $n' is the associated type an implementation supplies, so " +
          s"there is nothing for it to equal here — write 'type $n: Trait' to say what the " +
          s"implementation's must implement, and 'type $n = …' inside the 'impl'") |
          opt(op(":") ~> rep1sep(boundRef, op("+"))) ^^ (bs => AssocDecl(n, bs.getOrElse(Nil)))
      },
    )

  /** A line inside a trait body. A **definition** is tried first, since it is a signature with more
   * after it: `member` needs a body to follow the header, so a bare method signature falls through
   * to `methodSig` and a bare property signature to `propertySig`. A signature of either kind asks
   * an implementation for that member; one written with a body supplies a default instead.
   */
  protected lazy val traitMember: PackratParser[MethodDecl] =
    memberAttrs ~ (noVisibility ~> noOverride ~>
      (setter | setterSig | member | staticPropertySig | methodSig | propertySig)) ^^ { case as ~ m =>
      attributedMember(m, as)
    }
    // A trait's body holds nothing but members, so an annotation above one needs no commit of the
    // kind `restrictedMember` makes — there is no field rule underneath it to fall into.

  /** A trait method signature: a header with no `= body`. The receiver and parameters parse
   * exactly as a real method's do, so a signature and its implementation are compared shape for
   * shape.
   */
  protected lazy val methodSig: PackratParser[MethodDecl] =
    at(
      ident ~ opt(boundedTypeParams) ~ (op("(") ~> methodParams <~ op(")")) ~ opt(op("->") ~> resultRef) ^^ {
        case name ~ tps ~ ((recv, params, variadic)) ~ ret =>
          val tp = tps.getOrElse(TypeParams.none)
          MethodDecl(name, recv, isProperty = false, tp.names, params, ret, Nil, tp.bounds, tp.defaults,
            variadic = variadic, tvalues = tp.values, tpacks = tp.packs)
      },
    )

  /** A property signature — `name -> type` with neither a parameter list nor a body. */
  protected lazy val propertySig: PackratParser[MethodDecl] =
    at(ident ~ (op("->") ~> (opaqueRef | typeRef)) ^^ { case name ~ ret =>
      MethodDecl(name, None, isProperty = true, Nil, Nil, Some(ret), Nil)
    })

  /** `impl Trait for Type` with indented method definitions — ordinary members, reusing the same
   * grammar as a method written in a struct's own body. The block is closed by an optional
   * `end Type`.
   *
   * The type is a full type reference, not a name: `impl Show for []int` is as ordinary as
   * `impl Show for Point`, and which types an `impl` may be *for* is the analyzer's to decide
   * rather than something to leave the grammar unable to express.
   *
   * The block may declare **type parameters of its own**, in the same bracketed list a generic
   * function writes and in the same position — directly after the keyword that opens the
   * declaration: `impl[T: Show] Show for Box[T]`. They are what makes the implementation cover a
   * generic type as a whole, and the bounds on them are what make it conditional.
   *
   * The body itself is optional, because a trait whose every method has a default leaves a
   * conforming type nothing to write: `impl Zero for E` on its own line is the whole of that
   * implementation, and the opt-in it states is the point of writing it.
   *
   * The block may be marked **`override`** (`reference/traits.md § override — when the overlap is deliberate`), which says it deliberately replaces a
   * more general implementation already covering the same type. The keyword goes in front of `impl`
   * rather than anywhere inside, because what it qualifies is the block as a whole.
   */
  protected lazy val implDecl: PackratParser[Stmt] =
    overrideMod ~ (op("impl") ~> opt(boundedTypeParams) ~ implTrait ~ (op("for") ~> typeRef)) >> {
      case ov ~ (tps ~ ((tname, targs)) ~ forType) =>
        val tp = tps.getOrElse(TypeParams.none)

        whereOn(tp) >> { tp =>
          (implBody | success(Nil)) <~ endTypeRef(forType) ^^ { items =>
            ImplDecl(tname, forType, items.collect { case Right(m) => m }, tp.names, tp.bounds, targs,
                     tp.defaults, ov, tp.values, tp.packs,
                     assocs = items.collect { case Left(a) => a })
          }
        }
    }

  /** The `override` keyword in front of a declaration, or nothing — which is the ordinary case and
   * writes nothing, exactly as public visibility does.
   */
  protected lazy val overrideMod: Parser[Boolean] = opt(op("override")) ^^ (_.isDefined)

  /** The trait an `impl` is of: a name and its arguments, or a callable written as one
   * (`reference/types.md § Function types`).
   *
   * The arrow spelling is here so that the arity-carrying declaration behind a call trait stays out
   * of programs entirely — a type made callable by hand is written `impl Fn(int) -> int for Doubler`,
   * the same way the type of one is written everywhere else.
   */
  protected lazy val implTrait: Parser[(String, List[TypeRef])] =
    (fnWord ~> op("(") ~> commaList(typeRef) <~ op(")")) ~ (op("->") ~> typeRef) ^^ {
      case ps ~ r => (Type.Fn.base(ps.length), ps :+ r)
    } |
      qualifiedName ~ opt(typeArgs) ^^ { case n ~ args => (n, args.getOrElse(Nil)) }

  protected lazy val implBody: PackratParser[List[Either[AssocBind, MethodDecl]]] =
    newline ~> indent ~> skipNewlines ~> rep1sep(implItem, newlines) <~ skipNewlines <~ dedent

  /** One line of an `impl` block: an associated type supplied, or a member. */
  protected lazy val implItem: PackratParser[Either[AssocBind, MethodDecl]] =
    assocBind ^^ (Left(_)) | implMember ^^ (Right(_))

  /** `type Body = Column[Text, Button]` — the associated type this block supplies.
   *
   * The other spelling is refused by name for the reason the trait's is: a bound here would be the
   * implementation asking something of a type it is itself choosing.
   */
  protected lazy val assocBind: PackratParser[AssocBind] =
    at(
      (op("type") ~> ident) ~ (op("=") ~> typeRef |
        op(":") ~> err("an 'impl' supplies the associated type rather than bounding it — the bound " +
          "is the trait's, written 'type Name: Trait' there, and what goes here is 'type Name = …'")) ^^ {
        case n ~ t => AssocBind(n, t)
      },
    )

  /** A member of an `impl` block, which is the one place a member may say `override` — the trait it
   * implements is the only thing a member of a type can be replacing a body from.
   */
  protected lazy val implMember: PackratParser[MethodDecl] =
    memberAttrs ~ (noVisibility ~> overrideMod) ~ (setter | member) ^^ { case as ~ ov ~ m =>
      attributedMember(m.copy(overrides = ov).setPos(m.pos), as)
    }

  /** An optional `end Name` marker closing a declaration block, Scala-style. `end` is a soft
   * keyword; the trailing name must equal the declaration's own name, or it is a parse error.
   */
  protected def endName(name: String): Parser[Unit] =
    opt(onNextLine(softEnd) ~> checkedEndName(name)) ^^^ (())

  protected def checkedEndName(expected: String): Parser[Unit] =
    ident >> { n =>
      if n == expected then success(()) else err(s"'end $n' does not match '$expected'")
    }

  /** The same marker closing an `impl`, whose subject is a type rather than a name — `end []int`
   * as readily as `end Point`. The two references are compared as written, since nothing has
   * resolved either of them yet and matching the spelling is all this marker was ever doing.
   */
  protected def endTypeRef(expected: TypeRef): Parser[Unit] =
    opt(onNextLine(softEnd) ~> (typeRef >> { t =>
      if t == expected then success(()) else err(s"'end ${t.show}' does not match '${expected.show}'")
    })) ^^^ (())

  /** An indented block: a leading `Newline`+`Indent` (the lexer's off-side signal) wraps a
   * statement sequence closed by `Dedent`.
   */
  protected lazy val suite: PackratParser[List[Stmt]] =
    newline ~> indent ~> statements <~ dedent

  /** A single statement written on the same line as its control-flow keyword. */
  protected lazy val inlineBody: PackratParser[List[Stmt]] = inlineStatement ^^ (s => List(s))

  /** The body of a control-flow construct, Scala-style: the introducer keyword (`then` /
   * `do`) is required for a one-line body but optional before an indented block, since a
   * following `Newline`+`Indent` already marks the block unambiguously.
   */
  protected def body(keyword: String): Parser[List[Stmt]] =
    op(keyword) ~> (suite | inlineBody) | suite
}
