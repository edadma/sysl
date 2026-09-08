package sh.sysl

/** The type grammar: a type as it is written, and the `[…]` parameter list a declaration
 * carries.
 *
 * It is its own area because a type is written in places that have nothing else in common — a
 * field, a parameter, a result, a cast, a type argument — so every one of them reaches the same
 * rule rather than each admitting its own subset.
 *
 * The parameter list sits here rather than with the declarations that carry it because what it
 * reads is types and bounds, and every declaration form reads the identical list.
 */
trait TypeParser extends ExprParser {

  /** A type: a memory-mode sigil applied to a type, or a name optionally applied to type
   * arguments (`Box[int]`, `Result[T, string]`). `sync` stays a soft keyword — it is only
   * special immediately after `&`, and the `&sync T` alternative is tried first so that a
   * reference to a type actually named `sync` still parses.
   *
   * `weak` is a reserved word rather than a sigil, since a mode a program reaches for only for a
   * genuine back-reference (`03`) is better read than punctuated.
   */
  protected lazy val typeRef: Parser[TypeRef] =
    at(coreType ~ opt(op("->") ~> typeRef) ^^ {
      case t ~ None                        => t
      case TupleType(parts, false) ~ Some(r) => FnType(parts, r, bare = true)
      case t ~ Some(r)                     => FnType(List(t), r, bare = true)
    })

  /** A type with no arrow on it — everything a bare-arrow callable is written *out of*.
   *
   * The arrow is a suffix on this rather than an alternative among these, so `(A, B) -> C` reads its
   * left side as the parenthesized list it looks like and only then learns it was a parameter list.
   * That is what keeps one production for `(A, B)` whether a tuple or a callable was meant, and it
   * is why the two cannot disagree about how a comma inside parentheses is read.
   */
  protected lazy val coreType: Parser[TypeRef] =
    at(
      // `Fn(A) -> R`, the callable's type written out (`reference/types.md § Function types`). It
      // comes first because `Fn` is an ordinary identifier: without this the name alternative below
      // would take it and leave the parameter list stranded.
      (fnWord ~> op("(") ~> commaList(typeRef) <~ op(")")) ~ (op("->") ~> typeRef) ^^ {
        case ps ~ r => FnType(ps, r, bare = false)
      } |
        // `() -> R` — a callable of no arguments. Empty parentheses are not a type, so this is the
        // one place they may be written, and the arrow is what says so.
        (op("(") ~> op(")") ~> op("->") ~> typeRef) ^^ (r => FnType(Nil, r, bare = true)) |
        // `*extern(A) -> R`, C's function pointer (`reference/ffi.md § A function's address`). It
        // comes before the general `*` so the `extern` is read as part of this spelling rather than
        // as a type named `extern` — which it could not be anyway, the word being reserved, but the
        // alternative below would reach the name production and complain about the wrong thing.
        ((op("*") ~> op("extern") ~> op("(") ~> commaList(typeRef) <~ op(")")) ~ (op("->") ~> typeRef) ^^ {
          case ps ~ r => CFnType(ps, r)
        }) |
        op("*") ~> op("extern") ~> err("'*extern' is a foreign function's address, so it is written " +
          "with the signature that address is called at — '*extern(int) -> int', and '*extern() -> unit' " +
          "for one that takes nothing and yields nothing") |
        op("*") ~> coreType ^^ PtrType.apply |
        op("&") ~> softSync ~> coreType ^^ (t => RefType(t, sync = true)) |
        op("&") ~> coreType ^^ (t => RefType(t, sync = false)) |
        op("weak") ~> softSync ~> err("an atomic reference has no weak form yet — 'weak sync T' " +
          "wants the concurrency model of '06', which is not built") |
        op("weak") ~> coreType ^^ WeakType.apply |
        ((op("[") ~> opt(expression) <~ op("]")) ~ opt(op("const")) ~ coreType >> {
          // `const` after the brackets says the *view* refuses writes, so a length in them is a
          // contradiction: an array is storage rather than a view of it, and storage that is written
          // once is what `val` declares. Somebody reaching for one is owed that word rather than a
          // parse error, since the two spellings are a bracketed number apart.
          case Some(_) ~ Some(_) ~ t =>
            err(s"'const' says a view refuses writes, and an array is storage rather than a view of " +
              s"one — read-only storage is declared with 'val', as 'val name: [N]${t.show}'")
          case n ~ ro ~ t => success(ArrayType(n, t, readOnly = ro.isDefined))
        }) |
        // `<N>T`, a vector (`reference/types.md § Vectors`). The angle brackets are free in type
        // position: type arguments are spelled `[...]`, and nothing reaches `coreType` except after
        // a `:`, a `->` or another type constructor, where a comparison cannot appear.
        //
        // The empty spelling is caught here rather than left to fail as a stray `>`, because
        // somebody writing it has read `[]T` and is owed the reason the two are not parallel.
        (op("<") ~> op(">") ~> coreType >> { t =>
          err(s"a vector's lane count is part of its type, so '<>${t.show}' has no meaning — a " +
            s"slice drops its length because it carries one at run time, and a register's width is " +
            s"settled when the code is generated; write '<4>${t.show}' for four lanes")
        }) |
        ((op("<") ~> laneCount <~ op(">")) ~ coreType ^^ {
          case n ~ t => VectorType(n, t)
        }) |
        // `volatile T` (`reference/memory.md § Device memory`). It stays a soft word like `sync`,
        // so it is special only in front of another type — a program with a type of its own named
        // `volatile` still parses, since this alternative needs a second type after the word and
        // the name alternative below picks up what is left.
        softVolatile ~> coreType ^^ VolatileType.apply |
        tupleType |
        // A bare `..A` parses so that the analyzer can say what a pack is and where one may be
        // written (`reference/generics.md § A parameter may stand for a list of types`). Left to
        // the grammar it would be a stray token, and the reader would be told a newline was
        // expected rather than told about the feature they were reaching for.
        op("..") ~> ident ^^ PackType.apply |
        // `some Trait` says the type is inferred from a body, which only a result has — so a
        // reader who wrote one in a field, a parameter or a cast is told where it belongs rather
        // than told that a type was expected. It is tried before the name alternative and needs a
        // bound to follow, so a program with a type of its own called `some` still parses.
        (softSome ~> boundRef >> { b =>
          err(s"'some ${b.show}' is a result whose type is read off the body that produced it, so " +
            s"it may stand only as the result of a member of an 'impl' block — everywhere else the " +
            s"type has to be named")
        }) |
        // `T::Body` — an associated type, and a chain of them. The `::` is a suffix on the name
        // alternative rather than an alternative of its own, so `Buf[int]::Item` reads its arguments
        // as the arguments they are before the projection is applied to what they made.
        qualifiedName ~ opt(typeArgs) ~ rep(op("::") ~> ident) ^^ { case n ~ args ~ assoc =>
          assoc.foldLeft[TypeRef](NamedType(n, args.getOrElse(Nil)))(AssocType.apply)
        },
    )

  /** A vector's lane count: a literal, a name, or any expression **in parentheses**.
   *
   * **The closing `>` is why this is not simply `expression`, and the failure it prevents is a
   * silent one.** `>` is a comparison operator, so `<4>f32` handed to the general expression parser
   * reads `4 > f32` as a comparison and then wants the `>` that has already been eaten — the reader
   * gets *"'>' expected"* pointing at the end of the line, about a type they wrote correctly. An
   * array's `[N]T` has no such trouble, because `]` is not an operator and so cannot be mistaken for
   * part of what precedes it.
   *
   * The parenthesized form is the escape hatch, and it costs nothing to allow: `<(N * 2)>f32` is
   * unambiguous because the parentheses close before the `>` is reached. Everything the feature
   * actually needs — a literal width and a `const` parameter — is in the two bare forms.
   */
  protected lazy val laneCount: Parser[Expr] =
    at(intLit | (ident ^^ Ident.apply) | (op("(") ~> expression <~ op(")")))

  /** `(A, B)` — a tuple type. A single part is refused rather than read as a grouping, because the
   * two spellings would then differ by a comma and mean different things; `(T)` is the shape
   * somebody writes when they mean a one-tuple, and there is no such type (`reference/types.md §
   * Tuples`).
   */
  protected lazy val tupleType: Parser[TypeRef] =
    packTuple | (op("(") ~> commaList1(typeRef) <~ op(")")) >> {
      case List(one) =>
        err(s"'(${one.show})' is a type in parentheses, and a tuple has two or more parts — " +
          s"a product of one thing is that thing, so write '${one.show}'")
      case parts => success(TupleType(parts))
    }

  /** `(..A)` — the tuple of a type pack (`reference/generics.md § A parameter may stand for a list
   * of types`), which matches a tuple of any arity.
   *
   * Tried before the ordinary tuple, and the two cannot both parse: a pack is the whole of what is
   * between the parentheses. Mixing one with written-out parts — `(..A, int)` — is pack *expansion*
   * and is not built, so it is refused by name rather than left to fail as a type called `..A`.
   *
   * **The inner `PackType` is stamped by its own `at`.** The enclosing `at` around `coreType` only
   * reaches the `TupleType` this rule returns — its own top-level result — so without this the
   * `PackType` inside would carry no position at all, the same gap `elif` and string interpolation
   * had before both were given one.
   */
  protected lazy val packTuple: Parser[TypeRef] =
    (op("(") ~> at(op("..") ~> ident ^^ PackType.apply) <~ (op(")") | op(",") ~> err(
      "a type pack is the whole of the tuple it stands for — '(..A, T)' appends to a pack, which " +
        "is not built; write '(..A)' and reach the parts with 'for const'",
    ))) ^^ { p => TupleType(List(p)) }

  /** A function's declared result: one type, or several separated by commas
   * (`reference/declarations.md § Several results`).
   *
   * A result list is a property of the signature and not a type, so it is spelled here rather than
   * in `typeRef` — nothing that asks for a *type* can reach one, which is what keeps `-> int, int`
   * and a field or a parameter apart with no rule of its own.
   */
  protected lazy val resultRef: Parser[TypeRef] =
    opaqueRef |
      typeRef ~ rep(op(",") ~> typeRef) ^^ {
        case t ~ Nil  => t
        case t ~ more => TupleType(t :: more, results = true)
      }

  /** `some View` — a result whose concrete type is read off the body, promising only the bound.
   *
   * It is written out here rather than allowed among types, because it is not one: it stands in a
   * result and nowhere else, and `coreType` refuses it everywhere with a message that says so. A
   * result *list* cannot contain one either — several results are several types, and a type inferred
   * from the body is the whole of what the body produced.
   */
  protected lazy val opaqueRef: Parser[TypeRef] =
    at(softSome ~> rep1sep(boundRef, op("+")) ^^ SomeType.apply)

  /** The `[int, string]` argument list of an applied generic name, whether the name is a type's or
   * a trait's — a trait takes its arguments the same way and in the same place.
   */
  protected lazy val typeArgs: Parser[List[TypeRef]] =
    op("[") ~> commaList1(typeArg) <~ op("]")

  /** One argument of that list, which may stand for a **value** (`reference/generics.md § A
   * parameter may stand for a value`) — `Buf[4]`.
   *
   * A type is tried first and an expression only where nothing could be a type, so a bare `N` is
   * read as a name and left for the declaration to interpret: the grammar cannot tell a type
   * parameter's name from a value parameter's, and the declaration can.
   */
  protected lazy val typeArg: Parser[TypeRef] =
    assocArg | typeRef | (expression ^^ ValueArgType.apply)

  /** `Item = string` — an associated type supplied by a **trait object's** own brackets.
   *
   * It is tried before the two alternatives above rather than after, because `Item` on its own is a
   * perfectly good name: read as a type first, the argument would succeed and leave the `=` to fail
   * against the closing bracket, and the reader would be told a `]` was expected about a line that
   * has one. Where the brackets do not belong to a trait behind a mode, the analyzer is what says
   * so — the grammar cannot tell a trait's name from a struct's.
   */
  protected lazy val assocArg: Parser[TypeRef] =
    at(ident ~ (op("=") ~> typeRef) ^^ { case n ~ t => AssocArgType(n, t) })

  protected lazy val softSync: Parser[Unit] =
    accept("'sync'", { case t: lexical.Identifier if t.chars == "sync" => () })

  /** `some` stays a soft word for the reason `sync` and `volatile` do: it is special only in front
   * of a bound in a result position, so a program with a name of its own spelled `some` is
   * unaffected and the reserved-word table is untouched.
   */
  protected lazy val softSome: Parser[Unit] =
    accept("'some'", { case t: lexical.Identifier if t.chars == "some" => () })

  protected lazy val softVolatile: Parser[Unit] =
    accept("'volatile'", { case t: lexical.Identifier if t.chars == "volatile" => () })

  /** `Fn` stays a soft word for the reason `sync` does: it is only special immediately before a
   * parenthesized parameter list, so a program with a type of its own named `Fn` still parses.
   */
  protected lazy val fnWord: Parser[Unit] =
    accept("'Fn'", { case t: lexical.Identifier if t.chars == "Fn" => () })

  /** A type-parameter list where a parameter may carry a trait bound: `[T, U: Show, V: Ord + Hash]`.
   * It yields the parameter names alongside a name-keyed map of the bounds, so an unbounded
   * parameter is simply absent from the map.
   *
   * Every declaration that may be generic over types it does not know parses this one list — a
   * function, an `impl` block, a struct, an enum, a trait — because a bound means the same thing in
   * each: it is what the declaration assumes of the parameter, and what everything applying it must
   * supply.
   *
   * A bound is a trait **applied**, so it takes type arguments where the trait declares any:
   * `[E: From[IoError]]`. The arguments are types and parse as such, which is what lets one mention
   * another of the parameters being declared.
   *
   * A parameter may also carry a **default**, `[Rhs = Self]`, which stands in where the declaration
   * is applied to fewer arguments than it declares. The bound comes first and the default last, so
   * `[R: Show = Self]` reads as the two clauses it is; both are optional and independent. Whether a
   * default means anything in the position being parsed is the analyzer's question, since only it
   * can say so with the declaration under the message.
   */
  protected lazy val boundedTypeParams: Parser[TypeParams] =
    op("[") ~> commaList1(valueParam | packParam | boundedTypeParam) <~ op("]") ^^ { ps =>
      TypeParams(
        ps.map(_.name),
        ps.collect {
          case p: TypeParamSpec if p.bounds.nonEmpty => p.name -> p.bounds
          case p: PackParamSpec if p.bounds.nonEmpty => p.name -> p.bounds
        }.toMap,
        ps.collect { case p: TypeParamSpec if p.default.nonEmpty => p.name -> p.default.get }.toMap,
        ps.collect { case p: ValueParamSpec => p.name -> p.typ }.toMap,
        ps.collect { case p: ValueParamSpec if p.default.nonEmpty => p.name -> p.default.get }.toMap,
        ps.collect { case p: PackParamSpec => p.name }.toSet,
      )
    }

  /** `[const N: usize]` — a parameter standing for a **value** (`reference/generics.md § A
   * parameter may stand for a value`).
   *
   * The type is required and the marker is what makes it readable: without `const` this is
   * `ident ':' name`, which is exactly a bounded type parameter, and only name resolution could say
   * which was meant. Then a trait name misspelled into a type name would quietly change what kind of
   * parameter it is. The word is not a new one — `reference/modules.md § const — a value` already spells a compile-time constant
   * `const NAME: Type = expr`, and this is that with the initializer left to the caller.
   *
   * A **default is an expression here**, not a type, which is the second thing the marker buys: one
   * slot, two grammars, and no way to parse it without knowing which parameter is being read.
   *
   * **The missing type is refused where the `:` belongs, not by an alternative written after the
   * whole form**, and the difference is not stylistic. An alternative fails at the position it gets
   * to, and a combinator picking between two failures keeps the one that got *further*: the form
   * above reaches the `]` before it notices, so its failure outranks any error raised back at the
   * `const`, and the reader gets the generic complaint about whatever came next. Raised here the two
   * are at one position, and the one carrying a sentence wins.
   */
  protected lazy val valueParam: Parser[ValueParamSpec] =
    op("const") ~> ident ~ (op(":") ~> typeRef | err("a value parameter needs the type its " +
      "argument must have, as 'const N: usize' — the type is what says which values may stand " +
      "there")) ~ opt(op("=") ~> expression) ^^ {
      case n ~ t ~ d => ValueParamSpec(n, t, d)
    }

  /** `..A: Display` — a **type pack** (`reference/generics.md § A parameter may stand for a list of
   * types`), whose bound distributes over its members.
   *
   * The `..` is the marker, and it is in front for the same reason `const` is: without it `A: Display`
   * is an ordinary bounded type parameter and nothing in the grammar could say which was meant. It
   * reads the way the use does — `(..A)` — so a signature says pack in both places with one spelling.
   *
   * A pack takes **no default**: a default is one type, and there is no way to write a list of them.
   * Refused here rather than by an alternative written after the form, so the sentence is raised at
   * the `=` and outranks the generic complaint about what follows it.
   */
  protected lazy val packParam: Parser[PackParamSpec] =
    op("..") ~> ident ~ opt(op(":") ~> rep1sep(boundRef, op("+"))) ~ opt(op("=") ~> err(
      "a type pack takes no default — a default is one type, and a pack stands for a list of them",
    )) ^^ { case n ~ bs ~ _ => PackParamSpec(n, bs.getOrElse(Nil)) }

  protected lazy val boundedTypeParam: Parser[TypeParamSpec] =
    ident ~ opt(op(":") ~> rep1sep(boundRef, op("+"))) ~ opt(op("=") ~> typeRef) ^^ {
      case n ~ bs ~ d => TypeParamSpec(n, bs.getOrElse(Nil), d)
    }

  /** `where T: Display + Eq, U: Ord` — the bounds of a generic declaration, written after its
   * signature instead of inside its `[…]` list (`reference/generics.md § A bound may be written
   * out of line`).
   *
   * **It is pure syntax over the bracket list and lowers to exactly the same bounds.** Nothing
   * downstream learns that a clause was written: `TypeParams.withWhere` folds it into the same map
   * `[T: Display]` fills, so inference, coherence and every diagnostic are unchanged. What it buys
   * is that a signature's parameters and its requirements stop being interleaved, which is the whole
   * of the case for it and is why Rust, Swift and Kotlin all have one.
   *
   * **`where` is a contextual word and was already one** — `type Even = new int where is_even`
   * introduces a constrained subtype's predicate (`reference/types.md`). The two cannot collide,
   * because one follows a type inside a `type` declaration and this follows a signature, and a
   * program may still name a function `where`.
   *
   * A **value** parameter takes no clause: its `: usize` is the type its argument must have rather
   * than a bound, and a value implements no trait. `TypeParams.whereUnknown` refuses that along with
   * a name the declaration does not have.
   */
  protected lazy val whereBounds: Parser[List[WhereBound]] =
    softWord("where") ~> commaList1(
      ident ~ (op(":") ~> rep1sep(boundRef, op("+")) | err(
        "a 'where' clause names a type parameter and what it must implement, as " +
          "'where T: Display' — the ':' is what separates the two",
      )) ^^ { case n ~ bs => WhereBound(n, bs) },
    )

  protected lazy val boundRef: Parser[BoundRef] =
    at(qualifiedName ~ opt(typeArgs) ^^ { case n ~ args => BoundRef(n, args.getOrElse(Nil)) })
}
