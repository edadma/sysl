package sh.sysl

/** Statements and declarations — everything a file's body may hold.
 *
 * `Stmt` is sealed and is the one hierarchy that spans both, because at the top of a file the two
 * are the same thing: a declaration is a statement that binds a name rather than doing work, and
 * which of the two a given line is depends on the file it is in (`reference/modules.md § Where a program starts`).
 */

sealed trait Stmt extends Positioned

/** How far a top-level declaration is visible (`reference/modules.md § Visibility`), as the
 * modifier before it was written.
 *
 * Public is the unmarked default, so `Public` is what every declaration carries until one says
 * otherwise and what every declaration the compiler synthesizes carries outright. `private` names
 * the **file**, which is the one level that provably never crosses a file boundary; `private[M]`
 * widens that to a module and everything beneath it.
 *
 * `Scoped` holds the name **as written**, which is a simple name rather than a path: which module
 * it means is settled against the enclosing ones where the declaration sits, so the answer belongs
 * to hoisting rather than to the grammar.
 */
enum Visibility:
  case Public
  case File
  case Scoped(module: String)

/** One name an `import` brings in, and what it is to be called here: `read`, or `read as rd`.
 *
 * The alias is what a reader of the importing file sees, and the name is what the imported module
 * calls it — which is the direction that matters, since the two differ only where a name would
 * otherwise collide or read badly out of its home module.
 */
case class ImportSelector(name: String, alias: Option[String]) extends Positioned {

  /** The name this selector binds here. */
  def bound: String = alias.getOrElse(name)
}

/** `import a.b.c`, `import a.b.c as d`, `import a.b.{c, d as e}`, `import a.b.*` — a shorter
 * spelling for names that are already reachable by their full path (`reference/modules.md §
 * Imports`).
 *
 * The path is kept **as written**, undivided, because which part of `a.b.c` is the module and
 * which the member is a question only the analyzer can answer: `a.b.c` names a member `c` of
 * module `a.b` where that is the module, and the module `a.b.c` itself where *that* is. The
 * longest prefix that names a module wins, the same rule a qualified reference is read by.
 *
 * `selectors` is empty for the bare-path form, and `wildcard` marks the `.*` form; the two are
 * mutually exclusive by the grammar.
 *
 * `alias` is the **unbraced rename**, and it belongs to the bare-path form alone — `a.b.{c as d}`
 * carries its rename on the selector, where a list needs one per name. It renames whatever the path
 * turned out to name, module or member, because the two are one piece of syntax here and a reader
 * asking for a shorter word does not first have to know which they wrote.
 */
case class ImportDecl(
    path: List[String],
    selectors: List[ImportSelector] = Nil,
    wildcard: Boolean = false,
    alias: Option[String] = None,
) extends Stmt {

  /** The path as a programmer wrote it, for a diagnostic. */
  def show: String = path.mkString(".")

  /** The name the bare-path form binds here — the alias where one was written, the last segment
   * otherwise, which is the same rule `ImportSelector.bound` follows.
   */
  def bound: String = alias.getOrElse(path.last)
}

/** `var name [: type] [= init]`. A declaration with a type and no initializer starts at that
 * type's zero value, which is how a scratch buffer is written; a type that has no zero value
 * (one containing a `&T`, which always points at a live object) must be initialized.
 *
 * `align` is `@align(n)` written above it — the boundary this storage must begin on, carried as the
 * expression it was written as because the bound is folded rather than lexed, exactly as a struct's
 * is. It is on the *declaration* and not on the type: `@align` on a struct says every value of that
 * type is aligned, and this says this one object is.
 *
 * `section` is `@section("…")` — the linker section this object is placed in
 * (`reference/attributes.md § @section("...")`). It carries the string as written, because a
 * section name is the target's spelling and not sysl's.
 */
case class VarDecl(name: String, typ: Option[TypeRef], init: Option[Expr],
                   vis: Visibility = Visibility.Public, align: Option[Expr] = None,
                   section: Option[String] = None) extends Stmt

/** `const name: type = value` — a **module member** (`reference/modules.md § const — a value`). It is what a top-level `var` is not:
 * hoisted, order-free, and visible beyond its file under the ordinary rules, where a `var` at the
 * top of a file is a local of the entry point.
 *
 * The type is written rather than inferred because `reference/modules.md § Visibility`'s "anything
 * visible outside its file states its types" is what keeps interface extraction parse-only, and
 * this is the first declaration that rule has ever had to bind. Writing it is also what fixes the
 * initializer's type, so `const capacity: usize = 512` needs no suffix on the literal.
 *
 * It has no address and no storage: every use is folded to the value, which is why it needs no
 * initialization order and why an array bound may name one.
 */
case class ConstDecl(name: String, typ: TypeRef, value: Expr, vis: Visibility = Visibility.Public) extends Stmt

/** One line of a `c const` block: a constant whose value is a **C constant expression**, evaluated
 * by the C compiler for the target this build is for (`reference/ffi.md § A library may carry C`).
 *
 * It exists because a number a binding needs is sometimes one only C can work out.
 * `sizeof(StaticTask_t)` and `portMAX_DELAY` are not symbols to link against and not text to
 * transcribe: the first is a layout the headers compute and the second a macro that expands to an
 * arithmetic expression, and both change with the target, the header version and the macros the
 * project configures its headers with. Transcribing either produces a program that is right on the
 * machine it was written on.
 *
 * `reference/ffi.md § A library may carry C`'s answer for a macro — wrap it in three lines of C and
 * declare the wrapper `extern` — is still the answer for a *function*, and it is not one here. A
 * constant reached through a call is not a constant: it has no value until the program runs, so it
 * cannot size an array, cannot be a `match` arm and cannot be folded into anything. What this adds
 * is the value itself, at the time the rest of the language expects to have it.
 *
 * The C is held as written and never inspected by sysl. There is nothing to inspect: what is legal
 * in the expression is C's question, and the C compiler is what answers it — a refusal from clang is
 * the diagnostic, quoted, which is what makes "any constant expression" a claim this can honour
 * rather than a subset somebody has to maintain.
 *
 * **It is lowered to an ordinary `ConstDecl` before analysis** (`CProbe`), which is why nothing
 * downstream knows it exists: by the time a name is resolved, a bound is folded or a tree is
 * encoded, the value is a literal and the constant is the one `reference/modules.md § const — a value` already describes.
 *
 * **It is not a `Stmt`**, which is the same call `Param` and `AsmArm` are: it cannot be written
 * anywhere a statement can go, only inside the block below. Making it one would have bought nothing
 * and cost the two exhaustivity checks that keep a new statement from being quietly ignored — every
 * walk over `Stmt` would have had to carry a case for a node none of them can ever see.
 */
case class CConstDecl(name: String, typ: TypeRef, c: String, vis: Visibility = Visibility.Public)
    extends Positioned

/** A `c const` block and the constants under it.
 *
 * They are one declaration rather than several because they are **one question put to the C
 * compiler**: every constant in a file is measured by a single probe translation unit, so a block is
 * what the cost is actually shaped like and writing `c const` once per line would suggest a price
 * per line that is not being paid. It is also what lets the `c` be spent once — the word marks which
 * language the right-hand sides are written in, and that is a property of the run of them.
 *
 * A visibility written before the block belongs to every constant in it, for the reason it is a
 * block at all: they are declared together, and a modifier on the header is the one place a reader
 * would look for what governs the lines under it.
 */
case class CConstBlock(consts: List[CConstDecl]) extends Stmt

/** One line of a `c type` block: a name for the sysl type a **C typedef** turns out to be, measured
 * by the C compiler for the target this build is for (`reference/ffi.md § A library may carry C`).
 *
 * It is the type half of `c const` and exists for the same reason. A typedef whose width the target
 * or a `#define` decides — `TickType_t`, `time_t`, `off_t`, `wchar_t`, `sqlite3_int64` — cannot be
 * spelled in sysl, so a binding picks one integer type and is right by luck. That is not a size
 * mismatch anything can see: it is an `extern` declaring a different argument width from the function
 * it names, which links and then passes garbage in the high half.
 *
 * `c const` can already ask for `sizeof(TickType_t)` and has no way to *use* the answer, since
 * nothing turns a constant into the type of a parameter. This is that step, and it is the one C
 * itself takes — a typedef is a name for a type, and what the name means is a question only the
 * headers can settle.
 *
 * The C is held as written and never inspected by sysl, exactly as a `c const`'s expression is. What
 * comes back is a **size and a signedness**, and the type is whichever integer sysl spells that way.
 *
 * **It is lowered to a `TypeDecl` before analysis** (`CProbe`), which is why nothing downstream knows
 * it exists: by the time a name is resolved the type is an ordinary transparent subtype of the
 * measured integer, interchangeable with it and carrying no check.
 *
 * **It is not a `Stmt`**, for `CConstDecl`'s reason: it can be written only inside the block below.
 */
case class CTypeDecl(name: String, c: String, vis: Visibility = Visibility.Public) extends Positioned

/** A `c type` block and the typedefs under it.
 *
 * One block rather than a declaration per line, for the reason a `c const` block is one: the types
 * of a file are measured by a **single probe translation unit**, and that probe is the same one the
 * file's `c const` block uses. A file writing both blocks asks the C compiler one question, not
 * two, which is what keeps `reference/ffi.md § A library may carry C`'s "one clang per file that
 * writes a block" true rather than doubling it quietly.
 *
 * A visibility written before the block belongs to every type in it, exactly as a `c const` block's
 * does.
 */
case class CTypeBlock(types: List[CTypeDecl]) extends Stmt

/** `@assert(cond)`, `@assert(cond, "why")` — a condition checked while compiling.
 *
 * The condition is a constant expression (`reference/modules.md § const — a value`) folded by the
 * same machinery a `const` initializer goes through, so it may name constants, `sizeof`, `alignof`,
 * `offsetof` and the arithmetic over them. A false one is a compile error quoting the message; a
 * true one emits nothing at all.
 *
 * **It exists because `require` is the wrong tool and there was no right one.** A `require` is a
 * *runtime* precondition — `17` is explicit that it is still compiled, still branches and still
 * traps — so nothing could fail a build on a fact known while compiling. What wants that most is a
 * binding to C: sysl lays a struct out in declaration order and claims C compatibility by
 * construction (`reference/types.md § Structs`), and the claim was unverifiable from inside sysl,
 * because `sizeof` reports what sysl laid out rather than what the header says. Paired with a
 * `_Static_assert` in a `.c` beside it — which `reference/ffi.md § A library may carry C` already
 * compiles, for the target — the two pin both sides to one number and neither can drift silently.
 *
 * It is an **attribute rather than a word** for the reason the capability clauses are
 * (`reference/modules.md § Capabilities are a module property`): it says something *about* the
 * module rather than being a construct the language executes, and a reserved word would have cost
 * the lexer, the reference's reserved-word table and its stated count, and the highlighting grammar
 * — in two repositories — to buy nothing a sigil does not.
 */
case class AssertDecl(cond: Expr, message: Option[String]) extends Stmt

/** `static val`, `static var`, `static f() -> …` — a declaration in the file the program starts in
 * that belongs to the **module** rather than to that file's body (`reference/modules.md § Where a program starts`).
 *
 * The file carrying a program's statements is a body, so what it declares is local to that body: a
 * `val` is a stack local, a function is a nested function (`reference/declarations.md`). That is
 * what a reader wants nearly always, and there are three things it cannot be — a nested function
 * may not be generic, has no address, and is not a value — plus one a local cannot be, which is
 * visible to another file. This is how a declaration opts out and becomes an ordinary module
 * member.
 *
 * It is a **wrapper rather than a flag** because that is the whole of what it does: nothing past the
 * point where a file's declarations are separated from its statements ever sees one. `ProgramWalk`
 * unwraps it, hands the inner declaration to the hoisting passes exactly as another file's would be,
 * and no later pass has a case for it.
 *
 * It is meaningful in exactly one file per program, since only the entry file has a body for a
 * declaration to *not* belong to; anywhere else it says nothing and is refused rather than ignored.
 */
case class StaticDecl(inner: Stmt) extends Stmt

/** `val name [: type] = value` — a binding written once and never assigned to.
 *
 * One keyword, read at two levels, because it is one idea at both. Written at the top of a file it
 * is a **module member**: storage the program owns for its whole run, initialized before anything
 * runs, and read-only. Written inside a block it is a **local** — the immutable counterpart of
 * `var`, in the same frame with the same lifetime, differing only in that it may not be assigned
 * to again.
 *
 * What separates it from a `const` is an **address**. A constant is folded into every use and has
 * no storage at all, which is what lets an array bound name one; a `val` is a thing that sits
 * somewhere, so it may be indexed, iterated, and sliced — the slice being a `[]const T`, which is
 * how the read-only-ness survives being handed on. The rule for a reader is short: if it has to be
 * indexed, pointed at, or is bigger than a scalar, it is a `val`.
 *
 * The type is optional in the syntax and required by the analyzer at module level, where
 * `reference/modules.md § Visibility`'s "anything visible outside its file states its types"
 * applies. A local states nothing to anyone, so it infers exactly as a `var` does.
 */
case class ValDecl(name: String, typ: Option[TypeRef], value: Expr, vis: Visibility = Visibility.Public,
                   align: Option[Expr] = None, section: Option[String] = None) extends Stmt

/** `ref name = place` — a name for a place rather than for a value (`reference/memory.md § ref — a
 * name for a place`).
 *
 * The place is evaluated once, where this is written, and the name means the storage that was found
 * afterwards. So it neither copies what it names nor re-walks the path at each use, which are the
 * only two things a `var` and a re-written path respectively could offer.
 *
 * It carries no type annotation and no visibility, and both absences are the same fact: this is a
 * **local declaration and never a type**, so there is nothing for a reader elsewhere to be told. A
 * ref cannot be a field, a parameter, a return type, or a type argument, which is what keeps the
 * analyzer holding the place expression for as long as the name exists — and that, rather than the
 * address it lowers to, is what lets a write through the name re-check the invariants of every
 * struct the place lies inside.
 */
case class RefDecl(name: String, place: Expr) extends Stmt

/** `a, b = b, a` — several places written from several values in one step (`reference/expressions.md § Several places at once`).
 *
 * It is a statement rather than an expression, and that is what keeps it small: a single assignment
 * yields the value it stored, so a multiple one would have to yield several, and there is nothing an
 * expression could be that carries several values without becoming a tuple by another name.
 *
 * `op` is the operator that was written. Only `=` means anything here — a compound form is read so
 * that the rule against it can be explained rather than left to a parse failure.
 */
case class MultiAssign(op: String, targets: List[Expr], values: List[Expr]) extends Stmt

/** `val a, b = …` / `var a, b = …` — one binding that names several things at once (`reference/expressions.md § Several places at once`).
 *
 * The names are declared only after every value has been produced, so a value on the right may
 * still name whatever the surrounding scope calls one of them.
 */
case class MultiDecl(names: List[String], mutable: Boolean, values: List[Expr]) extends Stmt

/** `val (a, b) = …` / `var (a, b) = …` — one binding that takes a tuple apart by **pattern**
 * (`reference/types.md § Tuples`).
 *
 * This is the comma form's sibling and not a replacement for it: `val a, b = f()` takes a result
 * list or a tuple apart at one level, while a pattern says the *shape* and so reaches inside a
 * nested one — `val ((a, b), c) = p` names three things across two levels, which no list of names
 * can say.
 *
 * **Only an irrefutable pattern may stand here** — a tuple pattern, a **struct** pattern, a name, a
 * wildcard, and those nested inside one another. A binding has no arm to fall through to, so a
 * pattern that can fail to match would leave its names standing for nothing;
 * `reference/statements.md § match`'s refutable forms are refused with that as the reason.
 *
 * **A struct pattern qualifies because a struct has exactly one shape**, which is the same property
 * that makes a tuple pattern irrefutable — `reference/patterns.md § Struct patterns` calls a tuple
 * pattern the positional form of this
 * one. A *variant* pattern is the one that does not qualify: an enum has several shapes and naming
 * one of them is a test.
 */
case class PatternDecl(pattern: Pattern, mutable: Boolean, value: Expr) extends Stmt

case class ExprStmt(expr: Expr)                                    extends Stmt

/** `return` / `return expr` inside a function body. */
case class Return(value: Option[Expr]) extends Stmt

/** `become f(…)` — a call that **replaces** this frame rather than adding to it
 * (`reference/declarations.md § become — a call that replaces the frame`).
 *
 * It is `return f(…)` with the jump guaranteed rather than hoped for. `@tailrec` recognizes a
 * function's calls to *itself* and lowers them as a jump back to its own entry; this is the same
 * guarantee for a call to a **different** function, which is what a chain of them needs to be a loop
 * rather than a stack.
 *
 * **`become` is a soft word, not a reserved one.** A reserved word is spent out of every program's
 * namespace for the sake of one line apiece — the trade `alloc` made and is still paying for — and
 * this needs no reservation: two identifiers in a row are not otherwise a statement, so
 * `become next(vm)` is unambiguous wherever it stands.
 */
case class Become(call: Expr) extends Stmt

/** `break ['label] [expr]` — leaves an enclosing loop, optionally carrying the loop's value; the
 * label names which loop, defaulting to the nearest. `continue ['label]` skips to a loop's next
 * iteration.
 */
case class Break(label: Option[String], value: Option[Expr]) extends Stmt
case class Continue(label: Option[String]) extends Stmt

/** `defer stmt` — a statement to run on the way out of the block containing it, whichever edge
 * control leaves by (`reference/memory.md § Where defer sits`). It is registered when control
 * reaches it and not before, so one in a branch never taken schedules nothing.
 */
case class Defer(stmt: Stmt) extends Stmt

/** A design-by-contract clause at the top of a function body. `require` is a precondition,
 * checked once on entry; `ensure` is a postcondition, checked before every return. The optional
 * `msg` accompanies the runtime trap. Inside an `ensure` condition the identifier `result`
 * denotes the value being returned.
 */
case class Require(cond: Expr, msg: Option[String]) extends Stmt
case class Ensure(cond: Expr, msg: Option[String]) extends Stmt

/** `invariant <bool> [, "message"]` at the head of a loop body — a condition that holds on every
 * entry to the body (`reference/verification.md § invariant and variant on a loop`).
 *
 * It is a statement rather than a slot in each loop's header so that one rule serves all five loop
 * forms. Where it may stand is the analyzer's: at the head of a loop's body and nowhere else.
 */
case class Invariant(cond: Expr, msg: Option[String]) extends Stmt

/** `variant <int>` — a measure that strictly decreases (`reference/verification.md § invariant and
 * variant on a loop`, `reference/verification.md § variant on a function`).
 *
 * At the head of a loop body it decreases from one iteration to the next. In a function's contract
 * block it decreases at each direct recursive call, and there it may read only the parameters, which
 * is what lets the check be made entirely at the call site.
 */
case class Variant(expr: Expr) extends Stmt

/** `asm` — machine instructions, in an arm per architecture (`reference/inline-assembly.md`).
 *
 * Exactly one arm is selected, and every architecture the compiler can build for has to appear in
 * one, so a processor with no answer is reported while building for a different one. What an arm
 * holds is deliberately not a statement list: the instructions are text sysl does not read, and the
 * operands beside them are the whole of what it does.
 */
case class AsmStmt(arms: List[AsmArm]) extends Stmt

/** One architecture arm. `archs` names the processors it answers for, spelled as `#if` spells them. */
case class AsmArm(archs: List[String], body: AsmBody) extends Positioned

sealed trait AsmBody

/** Instructions, the operands they name, and what they destroy besides. All three lists may be
 * empty, and an arm written with nothing under it is exactly that — an architecture on which the
 * operation costs no instruction, which is a different claim from having no answer.
 */
case class AsmCode(lines: List[String], operands: List[AsmOperand], clobbers: List[String]) extends AsmBody

/** `unavailable "reason"` — there is no answer on these processors, and the reason is what a call
 * from one of them is told.
 */
case class AsmUnavailable(reason: String) extends AsmBody

/** Which way a value crosses the instruction boundary. Reading and writing one variable is not
 * spelled with both of these — it is a single operand the language does not have yet, and the pair
 * would compile to two registers rather than one.
 */
enum AsmDir {
  case In, Out
}

/** One operand: a variable already in scope, a direction, and where it has to live. `reg` is the
 * machine register named for it, or `None` for the `reg` class — any general-purpose register the
 * allocator likes.
 */
case class AsmOperand(dir: AsmDir, name: String, reg: Option[String]) extends Positioned

/** One line inside an arm, before the lines are sorted into the three lists `AsmCode` holds. It
 * exists only between the parser reading a block and building the arm from it: the instructions
 * keep their order and nothing else in the block has one, so a single pass over a mixed list is
 * simpler than a grammar that insists on which kind comes first.
 */
enum AsmItem {
  case Line(text: String)
  case Operand(operand: AsmOperand)
  case Clobber(regs: List[String])
}

/** How an instance member takes its receiver — the memory-mode sigil written before `self`.
 * A property receiver is implicit and not spelled, so it is absent here (a property carries no
 * `RecvMode`); an associated function has no receiver at all.
 */
enum RecvMode:
  case ByValue                 // self
  case ByPtr                   // *self
  case ByRef(sync: Boolean)    // &self, &sync self

/** A member declared in a type's body. Exactly one of three kinds, told apart by shape:
 *
 *   - an **instance method** has a `receiver` (the `self` sigil form) and a parameter list;
 *   - an **associated function** has no `receiver` and a parameter list, called `Type.name(…)`;
 *   - a **computed property** has `isProperty` set, no parameter list at all, and an implicit
 *     borrow receiver, read as `value.name` with no parentheses.
 *
 * An **empty `body` means a signature rather than a definition**, which is a shape only a trait's
 * members have: the grammar gives every other member a body, and a trait member written with one
 * is a default an `impl` inherits.
 *
 * `tparams` and `bounds` are the member's **own** type parameters, which are not the type's: a
 * method of a `Box[T]` that also takes a `[U]` is generic over `U` at each call, while `T` is fixed
 * by the receiver. A property has none — there would be nothing at the read to fix them with.
 *
 * `vis` is how far the member may be named from (`reference/modules.md § Visibility`). The unmarked
 * default means *its type's* reach rather than public, so `Public` here is "said nothing" and not
 * "said public" — which is why a trait's member and an `impl`'s, neither of which may say anything,
 * carry it too.
 *
 * `overrides` is the `override` keyword written in front of the member (`reference/traits.md § Replacing a default says override`), which says
 * it replaces a body the trait already supplied rather than answering a requirement the trait left
 * open. It is required where that is what the member does and refused where it is not, so a reader of
 * an `impl` block can tell the two apart without going to the trait to find out.
 */
case class MethodDecl(
    name: String,
    receiver: Option[RecvMode],
    isProperty: Boolean,
    tparams: List[String],
    params: List[Param],
    retType: Option[TypeRef],
    body: List[Stmt],
    bounds: Map[String, List[BoundRef]] = Map.empty,
    tdefaults: Map[String, TypeRef] = Map.empty,
    vis: Visibility = Visibility.Public,
    variadic: Boolean = false,
    overrides: Boolean = false,
    /** Which of `tparams` stand for a **value** rather than a type (`reference/generics.md § A parameter may stand for a value`), and at what type. */
    tvalues: Map[String, TypeRef] = Map.empty,
    /** Which of `tparams` stand for a **list** of types (`reference/generics.md § A parameter may
     * stand for a list of types`).
      *
      * A member's parameter list is its own, exactly as a function's is, so a pack may stand in it.
      * What cannot carry one is a declaration whose parameters *are* its shape — a struct, an enum
      * or a trait — and that refusal is about the shape rather than about members.
      */
    tpacks: Set[String] = Set.empty,
    /** `@crossing(…)` — see `FuncDecl.crossing`.
      *
      * **A member may carry the annotations that are about its PARAMETERS, and only those.** What
      * `@test`, `@tailrec` and `@export` say is about a free function — what a runner calls, what
      * recurses, what a symbol names — and a member supplies none of it. What these three say is
      * about a parameter, which a member has exactly as a free function does, so refusing them here
      * would mean an API asking for one had to route every such call through a wrapper whose only
      * purpose was to carry the word. `Channel[T]`'s transfers were exactly that.
      */
    crossing: List[String] = Nil,
    /** `@reads(…)` — see `FuncDecl.reads`. `None` and `Some(Nil)` divide as they do there. */
    reads: Option[List[String]] = None,
    /** `@writes(…)` — see `FuncDecl.writes`. */
    writes: Option[List[String]] = None,
    /** `@borrows(…)` — the parameters this member promises not to keep past the call.
      *
      * **Meaningful on a trait's members and nowhere else**, which is checked rather than assumed:
      * the exemption exists because a call through a trait object is opaque, and a body the compiler
      * can see needs no promise — the analysis reads the answer out of it, and a written one would
      * restate what is there and go stale the moment the body changed.
      */
    borrows: List[String] = Nil,
    /** `static count -> int` — a property of the **type** rather than of a value
      * (`reference/declarations.md § A static property`), read as `Type.count`.
      *
      * A property has no parameter list, so it has nowhere to say `self` and is an instance member
      * by construction; this is the word that says otherwise. `static` is the one the language
      * already spends on the same idea one scope out — `static val` in an entry file says a binding
      * belongs to the module rather than to the body — so nothing new is reserved.
      *
      * **`recvMode` is where it takes effect and is the whole of the mechanism**: a static property
      * answers `None` there, so everything that dispatches on a receiver treats it as the associated
      * function it is, with no second case to write.
      */
    isStatic: Boolean = false,
) extends Positioned {

  /** The mode this member takes its receiver in, or `None` for an associated function — which is the
   * one kind that has no receiver at all.
   *
   * A property's is by value and unwritten, so asking `receiver` about one answers `None` and means
   * something else entirely. Everything that dispatches on a receiver — a lowered `self` parameter,
   * a vtable slot, the object-safety rule — asks here instead, and a property is then the instance
   * member it is rather than a shape each of those has to special-case.
   */
  def recvMode: Option[RecvMode] =
    receiver.orElse(Option.when(isProperty && !isStatic)(RecvMode.ByValue))

  /** Whether this member is reached through its **type** rather than through a value — an associated
    * function, or a static property. The two differ only in whether the call site writes `()`.
    */
  def isAssociated: Boolean = recvMode.isEmpty
}

/** The calling convention a definition is entered under, where that is not the ordinary one
 * (`reference/ffi.md § interrupt`).
 *
 * **A name and an optional argument, rather than an LLVM convention spelled through.** What
 * `interrupt` *is* differs by processor — a calling convention on x86-64, a function attribute on
 * RISC-V, and nothing at all on AArch64 — so the source names the concept and the back end decides
 * what that becomes. A pass-through would put an LLVM spelling in a source file and be wrong for
 * every other machine the file is built for.
 *
 * `arg` is the parenthesised word: RISC-V's privilege mode, `interrupt(supervisor)`. It is optional
 * because most conventions have nothing to say and because the common mode has a default.
 *
 * One convention is known today and the shape carries a name anyway, so that the second one is an
 * analyzer change rather than a change to every tree that holds a function.
 */
case class CallConv(name: String, arg: Option[String] = None) extends Positioned

/** A function declaration. The body is a statement list whose trailing expression is the
 * implicit return value; an `= expr` short body is stored as a single-element list. A
 * missing `retType` means the function returns `unit`. `tparams` names the type parameters of
 * a generic function, which is instantiated afresh for each set of type arguments.
 *
 * `bounds` maps a type parameter to the traits it is bounded by (`f[T: Show, U: Ord + Hash]`),
 * keyed by name so it carries no positional dependence on `tparams`; a parameter with no bound
 * is absent from the map. A bound is what a caller must satisfy — the concrete type it supplies
 * for that parameter must implement every trait named — and is checked at each call site.
 *
 * `variadic` is the trailing `...` of `sum(n: int, ...)`: the same ellipsis an `extern` takes, under
 * the same rules for what the tail may hold. The body reads it through `va_start`/`va_arg`/`va_end`
 * (`12-functions-and-closures.md` §9).
 */
case class FuncDecl(
    name: String,
    tparams: List[String],
    params: List[Param],
    retType: Option[TypeRef],
    body: List[Stmt],
    bounds: Map[String, List[BoundRef]] = Map.empty,
    variadic: Boolean = false,
    vis: Visibility = Visibility.Public,
    tdefaults: Map[String, TypeRef] = Map.empty,
    /** Which of `tparams` stand for **values** rather than types, and the type each argument must
      * have — `[const N: usize]` (`reference/generics.md § A parameter may stand for a value`). The
      * two kinds share `tparams` because they share one list, one namespace and one argument
      * position; this map is what tells them apart.
      */
    tvalues: Map[String, TypeRef] = Map.empty,
    /** Which of `tparams` stand for a **list** of types — `[..A: Display]` (`reference/generics.md § A parameter may stand for a list of types`). */
    tpacks: Set[String] = Set.empty,
    test: Option[TestAttr] = None,
    /** `@setup`, `@teardown`, `@setup_all` or `@teardown_all` — the moment under `sysl test` at
      * which this function is called (`reference/attributes.md § The hooks a module may write`).
      *
      * One field for the four because they are one role written four ways: a declaration either is
      * a hook or is not, and which of the four moments it names is what `HookKind` carries. Two of
      * them above one declaration is refused where `@test` beside one is — a function is called at
      * one moment rather than at two.
      */
    hook: Option[HookAttr] = None,
    conv: Option[CallConv] = None,
    /** `@tailrec` — see `TFunc.tailrec`. */
    tailrec: Boolean = false,
    /** `@pure` — see `TFunc.pure`. */
    pure: Boolean = false,
    /** `@ghost` — see `TFunc.ghost`. */
    ghost: Boolean = false,
    /** `@reads(…)` — see `TFunc.reads`. `None` is a function that wrote no frame at all, which is a
      * different thing from one that wrote an empty one: the first says nothing about its effects
      * and the second says it has none (`reference/verification.md § @reads and @writes — what a
      * call may touch`).
      */
    reads: Option[List[String]] = None,
    /** `@writes(…)` — see `TFunc.writes`. `None`/`Some(Nil)` divide as they do for `reads`. */
    writes: Option[List[String]] = None,
    /** `@export` — see `ExportAttr`. */
    exported: Option[ExportAttr] = None,
    /** `@section("…")` — the linker section this definition is placed in (`reference/attributes.md § @section("...")`). */
    section: Option[String] = None,
    /** `@crossing(…)` — the parameters through which a value reaches another concurrency domain
      * (`reference/memory.md § @crossing — where the rule is asked`).
      *
      * Carried as the names that were written rather than as positions, for the reason a frame is:
      * the refusal of a word that names no parameter has to say which word, and a position could
      * only say which slot. A list rather than a set, so a name written twice is reported at the
      * second one.
      */
    crossing: List[String] = Nil,
    /** `@needs(…)` — the capabilities reaching this declaration requires (`reference/modules.md § A
      * declaration may name what reaching it needs`). See `Attr.Needs` for why it is a different
      * word from the file header's `@requires(…)`.
      */
    needs: List[String] = Nil,
) extends Stmt

/** What `@export` says about the function it is written above (`reference/ffi.md § @export`).
 *
 * `symbol` is the name the linker files the definition under, and `None` means the function's own.
 * That is `extern` read the other way: `extern exit(code: int)` resolves the symbol `exit`, and
 * `extern "opendir" c_opendir(…)` renames it — so `@export` and `@export("mylib_parse")` are the
 * same pair pointing the other direction, and neither side invents a spelling the other lacks.
 *
 * The rename is the form a real C API wants rather than a convenience. A library's symbols share a
 * prefix so that linking two of them is not a coin toss, and the sysl side has a module path doing
 * that job already — so `parse` in module `mylib` is the name to write and `mylib_parse` is the name
 * to export, and requiring the function to be *called* `mylib_parse` everywhere inside would be
 * spelling the module path twice.
 */
case class ExportAttr(symbol: Option[String]) extends Positioned

/** What `@test` says about the function it is written above (`reference/attributes.md § @test — a function with a caller nothing else has`).
 *
 * A test is a function the program does not call: `sysl test` calls it, and whether it *returned* is
 * the whole of the result. So the attribute carries only what the runner cannot work out for itself
 * — what to call the test in its report, and whether returning is the outcome it was after.
 *
 * `display` is the name a report shows, defaulting to the function's own. It exists because a
 * function name is a name and a test's subject is a sentence: `@test("an empty slice has no first
 * element")` says something `first_of_empty` only gestures at.
 *
 * `shouldTrap` inverts the verdict: the test passes exactly when the process does *not* come back.
 * That is how a runtime check is tested at all — a bounds violation, a broken `require`, a `within`
 * that does not hold each end in `llvm.trap`, which no program survives to report anything about.
 * `expected` narrows it to a run whose output holds a given substring, which is what tells a trap
 * from the *right* trap where the failure prints something first.
 */
case class TestAttr(display: Option[String], shouldTrap: Boolean, expected: Option[String]) extends Positioned

/** Which of the four moments under `sysl test` a hook function is called at
 * (`reference/attributes.md § The hooks a module may write`).
 *
 * The scope is the **module**: a module writes at most one of each, and what it writes runs for the
 * `@test` functions that module declares. `word` is the spelling, which is both what the grammar
 * reads and what a refusal names.
 *
 * `Setup` and `Teardown` bracket **each** test, `SetupAll` and `TeardownAll` bracket the module's
 * whole run. The division is what a hook can share with a test: a per-test hook runs in the test's
 * own process and so shares that process's module storage, and an `_all` hook is a run of its own
 * and shares only what outlives a process.
 */
enum HookKind(val word: String) {
  case Setup       extends HookKind("setup")
  case Teardown    extends HookKind("teardown")
  case SetupAll    extends HookKind("setup_all")
  case TeardownAll extends HookKind("teardown_all")
}

/** `@setup` and its three siblings, as written. The position is carried for the reason `TestAttr`'s
 * is: a hook that faulted has an exit status rather than a diagnostic, and this is the line a report
 * can point at.
 */
case class HookAttr(kind: HookKind) extends Positioned

/** One attribute written above a declaration, before it has been folded into the `FuncDecl` it
 * qualifies. It exists for the fold and for the refusal of a repeat — `word` is the spelling that
 * refusal names, and is what makes two attributes the same one.
 */
enum Attr(val word: String) {
  case Test(attr: TestAttr) extends Attr("test")

  /** `@setup`, `@teardown`, `@setup_all`, `@teardown_all` — one case for the four, taking its
    * `word` from the kind so that each is its own spelling to the refusal of a repeat, and so that
    * a fifth moment would be a `HookKind` case rather than a fifth attribute.
    */
  case Hook(attr: HookAttr) extends Attr(attr.kind.word)
  case TailRec              extends Attr("tailrec")
  case Pure                 extends Attr("pure")
  case Ghost                extends Attr("ghost")

  /** `@reads(a, b)` and `@writes(c)` — the module storage this function may touch
   * (`reference/verification.md § @reads and @writes — what a call may touch`).
    *
    * They carry a list rather than a set so the refusal of a name written twice can name the
    * position it was written at a second time; the check turns them into sets once it has looked.
    */
  case Reads(names: List[String])  extends Attr("reads")
  case Writes(names: List[String]) extends Attr("writes")

  /** The two that qualify a **layout** rather than a function, and the only attributes a struct
    * takes (`reference/types.md § Structs`).
    *
    * They are two axes and compose: `@packed` removes the padding *between* fields and drops the
    * aggregate's own alignment to one, `@align(n)` raises where the aggregate must *start*. Written
    * together they mean "no interior gaps, but begin on an `n` boundary", which is a real shape and
    * not a contradiction — a wire header that has to sit in a DMA-capable buffer is both.
    *
    * `@align` carries the expression rather than a number because the bound is folded, not lexed:
    * `@align(CACHE_LINE)` is the form a program actually wants, and `reference/modules.md §
    * Platform selection` already admits a `const` and the arithmetic over it wherever a constant is
    * required.
    */
  case Packed                extends Attr("packed")
  case Align(bound: Expr)    extends Attr("align")

  /** `@export` and `@export("mylib_parse")` — the definition is C-callable under an unmangled
    * symbol (`reference/ffi.md § @export`). See `ExportAttr` for why the rename is the form that
    * matters.
    *
    * **It marks a struct as well as a function, and the two are one idea: the name C sees.** On a
    * function that is the symbol the linker resolves; on a struct it is the name the `typedef`
    * carries in the generated header, which is otherwise derived from the mangled instantiation and
    * so reads `sh_sysl_box2d_c_Id` in a package. A binding mirroring a C library wants to hand back
    * that library's own spellings, and the type name was the one thing in a generated header nobody
    * chose.
    */
  case Export(attr: ExportAttr) extends Attr("export")

  /** `@section(".vectors")` — where the linker puts this one object or definition
   * (`reference/attributes.md § @section("...")`).
    *
    * It is the one attribute that marks **either** a binding or a function, because both are things
    * that occupy an address: a vector table is storage and a `.ramfunc` is code, and placement is
    * the same request about each. It marks no *type*, since a section holds an object and a type is
    * not one.
    *
    * The name is carried as the string it was written as, not lexed into anything: `.vectors` is
    * ELF's spelling and `__DATA,__mysection` is Mach-O's, and a set of characters chosen here would
    * refuse one some target requires. That is `extern`'s link name and `@export`'s symbol read a
    * third time — a spelling belongs to whoever consumes it.
    */
  case Section(name: String) extends Attr("section")

  /** `@crossing(state)` — the parameters a value reaches another concurrency domain through
    * (`reference/memory.md § @crossing — where the rule is asked`).
    *
    * It names parameters where `@reads` and `@writes` name module storage, and carries a list for
    * the same reason theirs do: a word that names no parameter is refused by name, and a name
    * written twice is refused at its second position.
    */
  case Crossing(names: List[String]) extends Attr("crossing")

  /** `@borrows(bytes)` — the parameters a **trait method** promises not to keep past the call
    * (`reference/traits.md § A method may promise to borrow`).
    *
    * A call through a trait object is opaque: any implementation could be behind it, so the escape
    * analysis has to assume every argument is kept, and a local array passed through one is
    * promoted to the heap. This is how a trait says otherwise — and the compiler holds every
    * implementation to it, so the promise is checked rather than trusted.
    *
    * **It is `borrows` rather than `borrowed` because sysl names a promise by the action.** The same
    * question was settled once for `@no_alloc`, which is not `@no_heap`: a clause is a promise about
    * conduct, and what is checked here is conduct — that no implementation keeps what it is handed.
    *
    * A list of names rather than a flag, because a method may take a buffer it borrows *and*
    * something it legitimately retains, and because the analysis is parameter-indexed already.
    */
  case Borrows(names: List[String]) extends Attr("borrows")

  /** `@needs(heap)`, `@needs(os, posix)` — the capabilities reaching this **declaration** requires
    * (`reference/modules.md § A declaration may name what reaching it needs`).
    *
    * **It is a different statement from the file header's `@requires(...)`, which is why it is a
    * different word.** A file's clause is about the **module**: it cannot be built at all without
    * the capability, and it is checked once, against the target. This is about one declaration:
    * reaching it needs the capability, and it is checked at the **call**, in the caller's module,
    * where the line a reader can change is. A module too coarse for the question is the whole reason
    * the finer form exists — `@requires(heap)` written on `sysl` would say something false about
    * most of it.
    *
    * **It is what an `extern` had no way to say.** Every other declaration has a body the compiler
    * reads: a function that makes heap storage is found by looking, which is what `NoAlloc` does. An
    * `extern` is a name and a signature, so a module that gave up an environment capability could
    * reach `open()` straight through one — and the only thing that could ever close that is the
    * declaration saying so itself.
    */
  case Needs(caps: List[String]) extends Attr("needs")
}

/** `extern name(params) -> ret` — a function this program does not define but may call, resolved
 * by the linker under the name it is declared with.
 *
 * It is a declaration and nothing else: no body, no type parameters, and no way to see what it
 * does. That is what makes it the seam a language reaches the outside world through — libc's
 * `exit`, a driver's MMIO helper — and why the escape analysis has to assume the worst of it
 * (`05-escape-analysis.md`): every argument may be kept, and the result may view any of them.
 *
 * `link` is the optional leading string of `extern "snprintf" fmt(…)`: the symbol the linker
 * resolves, when that differs from the name the program calls it by. Absent, the two are the same.
 * The distinction exists because a symbol's spelling belongs to whoever exported it — it may be
 * taken already, or shaped nothing like sysl — and because a library declaration reaching a C name
 * directly would otherwise spend that word out of every program's namespace.
 *
 * `variadic` is the trailing `...` of `extern printf(fmt: *u8, ...) -> int`: the C ellipsis, which a
 * sysl function may carry too (`12-functions-and-closures.md` §1, §9) under the same rules for what
 * the tail may hold.
 */
case class ExternDecl(name: String, params: List[Param], retType: Option[TypeRef],
                      variadic: Boolean = false, link: Option[String] = None,
                      vis: Visibility = Visibility.Public,
                      /** `@needs(…)` — the capabilities calling this requires. It is the declaration
                        * that had no other way to say so: an `extern` is a name and a signature, and
                        * there is no body for the compiler to read the answer out of.
                        */
                      needs: List[String] = Nil) extends Stmt:
  /** The symbol the linker resolves this to. */
  def symbol: String = link.getOrElse(name)

/** `extern name: type` — storage this program does not lay down but may read and write, resolved by
 * the linker exactly as an `extern` function is.
 *
 * It is the same seam as the declaration above, pointed at the other kind of thing a C library
 * exports. `stdout` and `environ` are variables rather than functions, and a language whose only way
 * out is a call cannot name either: `fputs(s, stdout)` needs the *variable*, and there is no getter
 * to reach it through.
 *
 * The type is written and never inferred — there is no initializer to infer it from, and what the
 * other side laid down is not something this compiler can see. Writing the wrong one is the same
 * kind of promise a wrong parameter list to an `extern` function is (`reference/ffi.md § extern — a declaration with no body`).
 *
 * `link` is the leading string of `extern "environ" env: **u8`, and means what it means for a
 * function: the symbol the linker resolves, when that differs from what the program calls it by.
 */
case class ExternVarDecl(name: String, typ: TypeRef, link: Option[String] = None,
                         vis: Visibility = Visibility.Public) extends Stmt:
  /** The symbol the linker resolves this to. */
  def symbol: String = link.getOrElse(name)

/** `struct Name[T…]` with `name: type` fields and, intermixed, member declarations (methods,
 * properties, associated functions). Positional construction is `Name(a, b, …)`.
 *
 * `bounds` is what the type asks of its own parameters — `struct SortedList[T: Ord]` — keyed by
 * parameter name, with an unbounded one simply absent. Every application of the type is held to
 * them, and its members may assume them: they are what makes a member checkable at its definition
 * rather than once per instantiation (`reference/generics.md § Bounds`).
 *
 * `opaque` withholds the **layout** from every module but the one declaring it (`reference/ffi.md §
 * opaque`): outside, the type may be named only as the pointee of a `*`. It is not a `vis`, and the
 * two are orthogonal — `vis` decides who may say the *name*, `opaque` decides who may know the
 * *shape*, and a type whose name nobody could say would have nothing to be opaque to.
 */
case class StructDecl(
    name: String,
    tparams: List[String],
    fields: List[Param],
    members: List[MethodDecl] = Nil,
    bounds: Map[String, List[BoundRef]] = Map.empty,
    invariants: List[Expr] = Nil,
    vis: Visibility = Visibility.Public,
    tdefaults: Map[String, TypeRef] = Map.empty,
    opaque: Boolean = false,
    /** Which of `tparams` stand for **values** rather than types, and the type each argument must
      * have — `struct Buf[const N: usize]` (`reference/generics.md § A parameter may stand for a
      * value`).
      */
    tvalues: Map[String, TypeRef] = Map.empty,
    /** `@packed` — fields at their declared offsets with no interior padding (`reference/types.md § Structs`). */
    packed: Boolean = false,
    /** `@align(n)` — the boundary this type's storage must begin on, as written. Folded by the
      * analyzer rather than the parser, so what is held here is the expression.
      */
    alignment: Option[Expr] = None,
    /** `@export("b2BodyId")` — the name this type's `typedef` carries in a generated C header
      * (`reference/ffi.md § @export`), where without it the name is derived from the mangled
      * instantiation.
      *
      * The whole attribute is held rather than the string, so a refusal can point at the annotation
      * rather than at the declaration under it — and a bare `@export` is a real form here, meaning
      * the declared name.
      */
    cname: Option[ExportAttr] = None,
    /** The traits named by a `derives` clause — `struct Size derives Eq, Ord`.
      *
      * Each is a trait the compiler knows how to write memberwise, and what it writes is an ordinary
      * `impl` block synthesised before anything is hoisted (`Deriving`). The clause is held as
      * written, positions and all, because every refusal about it names one trait out of a list and
      * has to point at that one.
      */
    deriving: List[BoundRef] = Nil,
) extends Stmt

/** One variant of an `enum`. A variant with `fields` is a data-carrying (tagged-union)
 * variant; a variant with an optional `value` and no fields is a simple integer constant.
 */
case class EnumVariantDecl(name: String, value: Option[Expr], fields: List[Param]) extends Positioned

/** `enum Name[T…]` with indented variants and, intermixed, member declarations (methods,
 * properties, associated functions) exactly as a struct body holds them. All-dataless variants make
 * a *simple* enum (integer constants, auto-incrementing, with optional explicit `= value`); any
 * data-carrying variant makes a *data* enum (a tagged union whose variants are constructed and
 * destructured).
 *
 * `underlying` is the `: iN`/`uN` annotation that pins a non-generic simple enum's storage type;
 * unspecified it is `int`. It is meaningless on a generic or data enum, which the analyzer rejects.
 *
 * `bounds` is what the type asks of its own parameters, exactly as a struct's are.
 */
case class EnumDecl(name: String, tparams: List[String], underlying: Option[TypeRef],
                    variants: List[EnumVariantDecl], members: List[MethodDecl] = Nil,
                    bounds: Map[String, List[BoundRef]] = Map.empty,
                    vis: Visibility = Visibility.Public,
                    tdefaults: Map[String, TypeRef] = Map.empty,
                    /** Which of `tparams` stand for **values** rather than types
                      * (`reference/generics.md § A parameter may stand for a value`), and the type
                      * each argument must have.
                      */
                    tvalues: Map[String, TypeRef] = Map.empty,
                    /** The traits named by a `derives` clause, exactly as a struct's are. */
                    deriving: List[BoundRef] = Nil) extends Stmt

/** The `within lo..hi` clause of a constrained subtype. `exclusiveHi` marks `..<`, which excludes
 * the upper endpoint; a plain `..` includes it. Bounds are literal expressions — an integer, a
 * float, or a character literal, optionally negated — evaluated to constants when the type resolves.
 */
case class RangeBound(lo: Expr, hi: Expr, exclusiveHi: Boolean) extends Positioned

/** `type Name = [new] Base [within lo..hi] [where predicate]` — one syntax over two things, told
 * apart by whether anything was added to the base.
 *
 * **With nothing added it is a transparent ALIAS**, which declares no type: `Name` and `Base` are
 * one type under two spellings, and `Base` may be anything a type expression can name — a struct, a
 * pointer, an array, a callable signature, a generic instantiation. Nothing is emitted and nothing
 * is checked when a value crosses between the names, because there are not two things for anything
 * to be emitted between.
 *
 * **With a constraint it is a subtype**, and there `Base` is a scalar (an integer, a float, or
 * `char`). Without `new` the subtype is **transparent**: a value flows to and from its base with no
 * cast, and every value produced into it is checked at run time against `range` and `pred`,
 * trapping on violation. With `new` it is a **derived** type: nominally distinct from its base and
 * from other deriveds, mixed only through an explicit cast. `range` is the `within` clause and
 * `pred` the `where` predicate; either or both may be present, and `new` needs neither. Inside
 * `pred`, the contextual name `value` binds the value being checked.
 *
 * `fromC` marks the one declaration the compiler writes itself: a `c type` measured against the C
 * compiler. It has an alias's shape — transparent, no range, no predicate — and is deliberately
 * **not** one: what it declares is a distinct scalar whose width the C compiler answered for, and a
 * program spells that width nowhere else.
 */
case class TypeDecl(
    name: String,
    base: TypeRef,
    derived: Boolean,
    range: Option[RangeBound],
    pred: Option[Expr],
    vis: Visibility = Visibility.Public,
    fromC: Boolean = false,
) extends Stmt

/** `trait Name` with indented method declarations — a method with a receiver and a parameter list,
 * written either as a bare **signature** (`show(self) -> string`) or with a body, which makes it a
 * **default**. A trait is nominal: a type participates only through an explicit `impl`, never by
 * coincidence of method names.
 *
 * A signature is a `MethodDecl` with an empty `body`, and that is the whole of the difference: a
 * method with one is a definition every `impl` inherits unless it writes its own.
 *
 * `tparams` makes the trait **generic**: `trait From[T]` is a different promise for every `T`, so a
 * type may implement it once per argument list and a bound naming it must say which one it means.
 * `bounds` is what the trait asks of those parameters, exactly as a struct's are.
 *
 * `supers` are the traits this one **requires**, written after the name as a bound is written:
 * `trait Word: Add + BitXor`. A supertrait is a promise the trait itself makes, so an `impl` supplies
 * it and everything that names the trait — a bound, a trait object — gets the required traits'
 * members along with its own.
 *
 * `tdefaults` are the `= Type` clauses of `trait Mul[Rhs = Self]`, keyed by the parameter they
 * belong to. A trait is one of the three declarations whose arguments are *written* where it is
 * applied, so a default has somewhere to stand in — the others are a struct and an enum. The
 * declarations whose parameters are solved instead carry the field only so the analyzer can say
 * why they may not have one.
 */
case class TraitDecl(
    name: String,
    tparams: List[String],
    methods: List[MethodDecl],
    bounds: Map[String, List[BoundRef]] = Map.empty,
    supers: List[BoundRef] = Nil,
    vis: Visibility = Visibility.Public,
    tdefaults: Map[String, TypeRef] = Map.empty,
    /** The `type Body: View` lines — the trait's **associated types**, in the order written.
      *
      * They are parameters of the trait exactly as `tparams` are, and are kept in a list of their
      * own for the one thing that separates them: an ordinary argument is written where the trait is
      * *used*, so it selects between implementations, while one of these is written by the
      * implementation and is settled by the subject. So they are absent from every arity check, from
      * every applied argument list, and from the key an implementation is filed under.
      */
    assocs: List[AssocDecl] = Nil,
) extends Stmt {

  /** Every parameter a member's signature may name: the trait's own, then its associated types. The
    * order is what `implAssoc` substitutes positionally against, and the two lists never overlap —
    * `checkTraitAssocs` refuses an associated type sharing a parameter's name.
    */
  def allParams: List[String] = tparams ::: assocs.map(_.name)
}

/** `type Body: View` inside a trait — one associated type, and what the type supplying it must
 * implement.
 *
 * The bounds are the whole of what a generic caller may do with the type: a `[V: View]` body reaching
 * `V::Body` gets something bounded by exactly these and licensed to do exactly what they promise,
 * which is the same rule a type parameter's bounds already state. Writing none is legal and says the
 * implementation may supply anything at all.
 */
case class AssocDecl(name: String, bounds: List[BoundRef] = Nil) extends Positioned

/** `type Body = Column[Text, Button]` inside an `impl` — one associated type supplied.
 *
 * It has an alias's shape and is deliberately not one: an alias declares a second spelling for a type
 * and this fills in an argument the trait left for the implementation. The grammar is shared because
 * the two really do say the same thing in the two places — a name, and the type it stands for — and
 * a reader arriving at either needs no second form to learn.
 */
case class AssocBind(name: String, typ: TypeRef) extends Positioned

/** `impl Trait for Type` with indented method **bodies**. Every method the trait declares without a
 * default must be present with a matching signature, and no method the trait does not declare; the
 * methods then become inherent members of `forType`, callable as `value.method(…)` exactly as a
 * method written in the type's own body. A default the block leaves out becomes a member of
 * `forType` just the same, from the body the trait supplied.
 *
 * `forType` is a full type reference rather than a name, because a trait may be implemented for a
 * type that has no name to be written under — `impl Show for []int` says how a slice of ints
 * renders, and `[]int` is a type the same way `Point` is.
 *
 * `tparams` are the block's own type parameters, written between `impl` and the trait —
 * `impl[T] Show for Box[T]` implements the trait for **every** `Box`, and its methods are
 * monomorphized per instantiation the way a generic type's own members are. `bounds` are what the
 * block asks of them: `impl[T: Show] Show for Box[T]` is **conditional conformance**, so a
 * `Box[int]` implements `Show` exactly when `int` does. Both are empty for an ordinary `impl`,
 * whose subject is one concrete type.
 *
 * `traitArgs` are the arguments the **trait** is applied to, for a trait that takes any:
 * `impl From[int] for Celsius`. They are what makes one type able to implement a trait more than
 * once, so they are part of what an implementation is filed under.
 *
 * `overrides` is the `override` keyword written in front of the block (`reference/traits.md § override — when the overlap is deliberate`), which says
 * it deliberately replaces a more general implementation that already covers the same type —
 * `override impl Display for []Point` against the library's `impl[T: Display] Display for []T`.
 * Without it the second implementation is refused, so the accidental duplicate the rule exists to
 * catch is still caught.
 */
case class ImplDecl(
    traitName: String,
    forType: TypeRef,
    methods: List[MethodDecl],
    tparams: List[String] = Nil,
    bounds: Map[String, List[BoundRef]] = Map.empty,
    traitArgs: List[TypeRef] = Nil,
    tdefaults: Map[String, TypeRef] = Map.empty,
    overrides: Boolean = false,
    /** Which of `tparams` stand for **values** rather than types, and the type each argument must
      * have — `impl[const N: usize, T: Display] Display for [N]T` (`reference/generics.md § A
      * parameter may stand for a value`). It is what tells a block covering every array length from
      * one covering the length it named, which the resolved subject cannot: a value parameter
      * stands at zero for the walk that checks the body, so `[N]T` and `[0]T` resolve alike and
      * only the syntax says which was written.
      */
    tvalues: Map[String, TypeRef] = Map.empty,
    /** Which of `tparams` stand for a **list** of types rather than one — `impl[..A: Eq] Eq for
      * (..A)` (`reference/generics.md § A parameter may stand for a list of types`). Recorded for
      * the reason `tvalues` is: a pack stands at two types for the walk that checks the body, so
      * `(..A)` and a written-out pair resolve alike and only the syntax says which was meant.
      */
    tpacks: Set[String] = Set.empty,
    /** The `type Body = …` lines — the **associated types this block supplies**, in the order
      * written. A block that supplies one through a `some` result writes none of these, and the
      * binding is filled in from the body instead.
      */
    assocs: List[AssocBind] = Nil,
) extends Stmt
