package sh.sysl

import scala.collection.mutable

/** What the program is made of, as the analyzer holds it.
 *
 * Every declaration the sources wrote, every instantiation made from one, and every accounting of
 * what something has reached: the tables themselves, with the questions asked *of* them living
 * elsewhere. A table is registered by hoisting, read by the passes, and — for the ones an
 * instantiation adds to — grown as a walk discovers what a program actually uses.
 *
 * It also carries which declarations are the **library's**, because that is a property of where a
 * declaration came from and is asked wherever one is read: a library body is analyzed only once
 * something reaches it, and a library name is in scope with no import.
 *
 * What it deliberately does *not* hold is the reading of those tables. Asking what members a type
 * has, whether it satisfies a bound and through which implementation, and what to say when it does
 * not, is `TraitLookup`.
 */
trait DeclTables extends Reporting {

  /** The undo log the speculative walks rewind through (`AnalyzerBase.sandboxed`).
   *
   * It is declared first because the tables below are built against it, and a trait's fields are
   * initialized in the order they are written.
   */
  protected val journal = new Journal

  /** How many writes this analysis had to remember how to undo (`Journal.recorded`). */
  private[sysl] def undoEntries: Long = journal.recorded

  /** The longest the undo log ever got, which is how deep the speculation nested (`Journal.deepest`). */
  private[sysl] def deepestUndoLog: Int = journal.deepest

  /** The library modules this compilation is **producing** rather than being supplied with.
   *
   * Empty for every ordinary compilation, which is what makes the library something a program never
   * has to think about. It is non-empty only where the library's own source is what is being
   * compiled — `sysl build-lib library --std` — and there it does two things: it stops the compiler
   * from handing those files a second copy of themselves, and it makes what they declare count as
   * the library's, which is `libraryOwns` below.
   */
  protected def building: Set[String]

  /** The modules this compilation is **producing** as against the ones it was **handed**, where
   * anything was handed to it at all — a `--lib` source root's modules and a fetched package's are
   * handed, and the program's own are not.
   *
   * `None` where the compilation has no libraries behind it, which is most of them, and the answer is
   * then that every module is the program's own. This is `Reachability.contributing`'s parameter of
   * the same name, and it means the same thing: only the caller knows, because `units` is one flat
   * list by the time it gets here and nothing in a tree says which road it arrived by.
   *
   * `building` is the neighbouring fact and a different one: it names the modules of the **standard
   * library** where this compilation is the one producing them. A library reached by `--lib` is
   * neither — it is not the program's, and it is not the compiler's.
   */
  protected def own: Option[Set[String]]

  /** Whether a module is one this compilation is producing rather than one handed to it. */
  protected def ownModule(module: String): Boolean = own.forall(_.contains(module))

  /** The files this compilation was handed to compile. */
  protected def units: List[Program]

  /** Whether a declaration came from one of *those* files — asked by identity, for the reason
   * `Stdlib.owns` is: a copy of a library file, however faithful, is not that file.
   */
  protected def producedHere(d: Positioned): Boolean = d.pos.exists(p => producing(p.source))

  private lazy val producing: Set[Source] = units.map(_.source).toSet

  /** The standard module this compilation is compiled against, and where its trees came from
   * (`Stdlib`). It is carried rather than looked up because a compilation may be handed a library
   * artifact instead of the library's source, and every question about *which* declarations are the
   * library's has to be answered over the one it actually got.
   */
  protected def std: Stdlib

  /** The machine this compilation is **for**. A handful of rules are the processor's rather than
   * the language's — `reference/ffi.md § interrupt`'s calling conventions are the case — and asking
   * here is what lets those be ordinary diagnostics instead of something codegen discovers with
   * nowhere to report it.
   */
  protected def target: Target

  /** Which package each file came from, and what its import lines mean (`reference/packages.md §
   * What a dependency's modules are called`).
   *
   * Empty for a compilation with no dependencies, which is every compilation that does not go
   * through the package system — so nothing about a one-project build is changed by this being here.
   */
  protected def packages: Packages

  /** What the target **provides**, which the project config says and the registry does not
   * (`reference/packages.md § What a project is called`, `reference/modules.md § The target's half
   * needs no clause at all`).
   *
   * This is the ceiling half of the two-level rule: a module's effective set is what the target
   * offers intersected with what the module gave up, so a capability absent here is absent
   * everywhere in the build no matter what any file declares. A compilation with no config behind it
   * gets all of them, which is what every build did before there was a file to say otherwise.
   */
  protected def provides: Set[String]

  /** Whether the target offers `cap` at all. */
  protected def targetProvides(cap: String): Boolean = provides(cap)

  /** Whether a declaration written in `module` is one the **library** supplies.
   *
   * Normally that is a question about which file it came from and nothing else (`Stdlib.owns`), and
   * it is asked of the `Source` rather than of the module because a `Source` is the stronger answer:
   * a user file that happened to sit at `library/sysl/render.sysl` is not one of the library's. The
   * compilation that *builds* a library module is the one
   * case where the source is the library's without being what this compilation was handed, and it has
   * to be, or the rest of the library could not name what it declares: the library's
   * `impl Display for (A, B)` calls `display_pad` and resolves it among the library's own, and while
   * `library/sysl/render.sysl` is being compiled that is the file in front of it.
   */
  protected def libraryOwns(d: Positioned, module: String): Boolean =
    std.owns(d) || building(module)

  /** Whether a declaration written in `module` is one the library **offers unqualified** — that is,
   * one of the names in scope everywhere with no import (`reference/modules.md § Separate
   * compilation`, `libraryNames`).
   *
   * Not every library declaration is. Only the standard module's names arrive unasked-for; a
   * submodule's are reached by naming the module or importing it, like any other module's
   * (`Library.autoImported`). Without the second half, splitting the library into submodules would
   * put every name back into every file by another route and change nothing at all.
   *
   * A module the standard module does not carry is left alone, which is what keeps this about the *library*: a
   * compilation building some other library is also one whose declarations `libraryOwns` counts,
   * and what that library offers unqualified is its own affair (`AutoImport.including`).
   */
  protected def libraryOffers(d: Positioned, module: String): Boolean =
    libraryOwns(d, module) && (Library.autoImported.contains(module) || !std.carries(module))

  /** Whether a declaration was **supplied** to this compilation by the library, rather
   * than being one this compilation is producing. It is what decides whether a body is analyzed only
   * once something reaches it, so that a program that never prints carries no printing surface.
   *
   * The second half is not a detail. `std.owns` asks which `Source` a declaration came from, and a
   * compilation *building* the library can be handed the very `Source` objects the compiler embeds —
   * the sbt task that builds the standard module artifact has them in memory and no reason to go to disk for a
   * second copy. Asking ownership alone there holds back every function in the library, nothing
   * reaches any of them, and the artifact comes out with an **empty object half**: it still carries
   * every tree, so every program that used it would compile and run, and the whole point of
   * precompiling would be silently gone. Read off disk the same source answers the other way, so the
   * two builds of one library would disagree with nothing failing.
   *
   * **What is being produced is a question about the file, and only the file answers it.** Asking it
   * of the *key* — is the module this declaration is filed under one of the modules being built —
   * looks equivalent and is not: a member of a builtin type is keyed under the type, which has no
   * module at all, so `string.contains` and `real.sqrt` are filed under the root however far inside
   * the library they were written. Those came back as supplied, were held back, and were compiled
   * only if something in the library itself happened to call them — while the same library read off
   * disk compiled them all. The difference reached the object half one instantiation at a time: a
   * default body that was never analyzed never asked for `Option.is_some` at `usize`, so one build
   * shipped it and the other did not.
   *
   * This is the reverse of `libraryOwns`, which asks whether a declaration counts as the library's
   * for *scope* — and there a library being built has to count, or the rest of it could not name what
   * it declares.
   */
  protected def suppliedByLibrary(d: Positioned): Boolean =
    std.owns(d) && !producedHere(d)

  // --- what the sources declared ---------------------------------------------------------

  protected val structDecls = mutable.LinkedHashMap.empty[String, StructDecl]
  protected val enumDecls   = mutable.LinkedHashMap.empty[String, EnumDecl]
  protected val funcDecls   = mutable.LinkedHashMap.empty[String, FuncDecl]

  /** The keys of an **overload set**, under the key the name resolves to
   * (`reference/declarations.md § Overloading`).
   *
   * Overloading is a fact about a *name*, and every table here is keyed by a name that must stand
   * for one declaration — so the second function of a name is filed under a key of its own, and this
   * is what relates the two. The first keeps the plain key, which is what makes overloading cost
   * nothing anywhere it is not used: a name declared once has no entry here at all, and `funcKey`
   * answers exactly as it did.
   *
   * The distinguishing suffix is a **numeric** segment — `m$paint.2` — which no other producer of a
   * key can collide with. A generic instantiation appends a mangled type (`f.int`), a member appends
   * a name (`Point.dist`), and `Type.mangle` never yields a segment that is only digits: an array is
   * `arr3`, a value argument `c5`. `qn` takes it back off, so no diagnostic ever shows one.
   */
  protected val overloadSets = mutable.LinkedHashMap.empty[String, List[String]]

  /** Every declaration of the name this key resolves to, in the order they were written. A name with
   * no overloads answers with itself, so a caller need not ask which case it is in.
   */
  protected def overloadKeys(key: String): List[String] = overloadSets.getOrElse(key, List(key))

  /** The key a name resolves to, given the key of any one of its overloads — the numbered segment
   * taken back off. A key with no such segment is its own answer, so this is safe to ask of any
   * function key.
   */
  protected def overloadPlain(key: String): String = {
    val cut = key.lastIndexOf('.')

    if cut > 0 && cut < key.length - 1 && key.drop(cut + 1).forall(_.isDigit) then key.take(cut)
    else key
  }

  /** The **type keys** that were given a destructor by an `impl Drop` (`reference/memory.md § A
   * destructor`).
   *
   * Kept as the keys an `impl` names rather than as instantiated types, because that is what is
   * known when the block is lowered — a generic type's `impl` is one block covering every
   * instantiation, and which of those a program actually makes is not settled until the walk ends.
   * `ProgramWalk` asks this of each type it did instantiate.
   */
  protected val dropsDeclared = mutable.Set.empty[String]

  /** The `@test` functions the sources declared, in the order hoisting met them (`reference/attributes.md § @test — a function with a caller nothing else has`). A
   * report lists tests in the order they were written, and this is where that order comes from —
   * the typed functions are grouped by what reaches them and say nothing about where they sat.
   */
  protected val tests = mutable.ListBuffer.empty[TTest]

  /** The hooks the sources declared, in the order hoisting met them
   * (`reference/attributes.md § The hooks a module may write`).
   *
   * Flat rather than grouped by module, because it is filled one declaration at a time and the
   * grouping is what `ProgramWalk` does with it once every file has been read. A second hook of one
   * kind in one module is refused against what is already here, which is also what makes the refusal
   * able to name the first.
   */
  protected val hooks = mutable.ListBuffer.empty[THook]

  /** Every file whose header said `@tests` (`reference/attributes.md § @tests — a file of
   * scaffolding`), so that hoisting can mark what each one declares under the key it hands out.
   *
   * Compared by identity, as every other question about a file is: two files of one module are two
   * `Source`s whatever their text.
   */
  protected val testOnlyFiles = mutable.Set.empty[Source]

  /** Everything declared in a file whose header said `@tests` (`reference/attributes.md § @tests — a file of scaffolding`), by the module-qualified
   * key every other table here uses.
   *
   * It is filled **while hoisting**, under the key each declaration actually got, from
   * `testOnlyFiles` and the file being hoisted. Filling it from the parsed files instead — by the
   * plain module-qualified spelling, before any key was handed out — was wrong for exactly the names
   * two files contend for: a `private` name a sibling file also declares privately, and a name a
   * `@tests` file overloads. Either way the second declaration gets a key of its own while the plain
   * spelling stays the *first's*, so recording the plain one marked whichever file was hoisted
   * first. Where that was the ordinary file, its own helper was reported as test scaffolding at its
   * own call site; where it was the test file, the ordinary file's declaration was the one that got
   * a key nobody had marked, and shipped.
   *
   * A set rather than a list: nothing asks what order test scaffolding was declared in, only whether
   * a given name is some.
   */
  protected val testOnlyDecls = mutable.Set.empty[String]

  /** Whether the body being analyzed is one a test build keeps and every other build drops — a
   * declaration written in a `@tests` file, or a `@test` function written anywhere (`reference/attributes.md § @tests — a file of scaffolding`).
   *
   * It is carried rather than looked up because a **closure has no declaration to ask**. Lowering
   * one produces a function under a name the compiler made up (`Closures.base`), and that name is in
   * no table saying which file it came from — so a closure written inside a test would be
   * indistinguishable from one written in the program, which is both halves of the rule wrong at
   * once: the walk reports the test's own helpers as though a shipped function had named them, and
   * the drop leaves the lowered body behind.
   *
   * Saved and restored across `analyzeNested` beside `currentFunctionName`, for the reason that one
   * is: a closure interrupts a body that is still going, and a closure inside a closure is still
   * inside whatever the outermost body was.
   */
  protected var inTestBody = false

  /** Every type an id has been asked for, by the id it got — the collision check `TypeId` says it
   * owes (`02`).
   *
   * A hash is what makes two compilations agree about one type, and the price is that two *different*
   * types could hash alike. Nothing downstream could notice: the ids would simply compare equal, and
   * a cache keyed on one would answer with the other's entry. So every id this compilation hands out
   * is recorded under the name it was computed from, and a second name arriving at the same number
   * stops the compilation instead.
   *
   * It cannot see across a compilation boundary and does not claim to. What it covers is every type
   * that meets in one program, which is every pair that could be compared.
   */
  private val typeIdsIssued = mutable.HashMap.empty[BigInt, String]

  /** The compile-time identity of a type, checked for collision against everything asked so far.
   *
   * Both readers go through here — `T::Id` folded into a literal, and the word a method table
   * carries — so the two cannot drift, and the check sees the union of what either asked.
   */
  protected def typeIdOf(ty: Type): BigInt = {
    val id   = TypeId.of(ty)
    val name = Type.memberSymbol(ty)

    typeIdsIssued.get(id) match
      case Some(other) if other != name =>
        err(s"'${show(ty)}' and '$other' have the same compile-time identity, which two different " +
          "types may not — '::Id' is a hash of the type's name, so this is a collision rather than " +
          "a mistake in either declaration. Renaming either type moves it")
      case _ =>
        typeIdsIssued(id) = name
        id
  }

  /** Types whose *declaration* was reported as a mistake, so that using one does not report it
   * again. A declaration is instantiated eagerly and therefore judged once, but a name can be
   * mentioned any number of times afterwards, and each mention would otherwise rebuild the same
   * type and raise the same complaint at its own position — telling the reader the same thing about
   * `enum Colour` at every line that says `Colour`. A use of one of these raises `Poisoned` instead,
   * which abandons that statement in silence, exactly as touching a `Type.Unknown` does.
   */
  protected val brokenDecls = mutable.Set.empty[String]

  /** Declared constrained subtypes by key (`16`). Each `type Name = Base …` is registered here; the
   * resolved `Type.Constrained` it stands for is built and cached the first time the name is used.
   */
  protected val constrainedDecls = mutable.LinkedHashMap.empty[String, TypeDecl]

  /** The resolved constrained subtype for a key, built once and reused so every reference to a name
   * is the same `Type.Constrained` — and so its bounds are validated a single time.
   */
  protected val constrainedInsts = mutable.LinkedHashMap.empty[String, Type.Constrained]

  /** The function key a `where` predicate is synthesised under, derived from the subtype's key. The
   * `$` keeps it clear of any name a program could write, so it never collides with a user function.
   */
  protected def predKey(typeKey: String): String = s"$typeKey$$pred"

  /** The function key a struct's `invariant` clauses are synthesised under — the conjunction of them
   * as one `bool` function of the struct's fields. `$` keeps it clear of any user name, as `predKey`
   * does for a `where` predicate.
   */
  protected def invKey(typeKey: String): String = s"$typeKey$$inv"

  /** Declared constants by key (`reference/modules.md § const — a value`). A constant is folded into each use and has no storage, so
   * this table is read by the analyzer and never by codegen — there is nothing downstream to emit.
   */
  protected val constDecls = mutable.LinkedHashMap.empty[String, ConstDecl]

  /** `@assert` conditions, in the order they were read, each with the scope that read it.
   *
   * A list rather than a map, because an assert declares no name: there is nothing to key it by,
   * nothing can refer to one, and two saying the same thing are two checks rather than a duplicate.
   * Like a constant it never reaches codegen — a true one emits nothing, and a false one stops the
   * compilation.
   *
   * **The scope travels with it**, which every other table gets for free from `declScope` and this
   * one cannot, having no key to file under. It is not optional: the check runs long after the walk
   * has left the file, so an assert naming a constant in its own module would otherwise be resolved
   * in whatever module the walk happened to be in — and would report that the condition is not a
   * constant expression, which is both wrong and misleading.
   */
  protected val assertDecls = mutable.ListBuffer.empty[(AssertDecl, Scope)]

  /** Declared module-level `val`s by key (`reference/modules.md § val — a thing`). Unlike a constant, this one reaches codegen: it
   * is storage, and every use of it is a read through an address rather than a copy of a literal.
   */
  protected val valDecls = mutable.LinkedHashMap.empty[String, ValDecl]

  /** Declared module `var`s by key (`reference/modules.md § val — a thing`) — the mutable half of module storage, written
   * `static var` in the file the program starts in and plain `var` in any other.
   *
   * A separate table from the `val`s only because the two declarations have different shapes: a
   * `var` may omit its value where a `val` may not, and it must state its type where a `val`
   * may infer one. Everything downstream treats them as one kind of thing, which is what `globalKey`
   * and `globalType` are for, and what makes a name declared by one clash with a name declared by
   * the other.
   */
  protected val staticVarDecls = mutable.LinkedHashMap.empty[String, VarDecl]

  /** Declared traits by name. A trait is a set of method signatures a type opts into through an
   * explicit `impl`; nothing conforms structurally.
   */
  protected val traitDecls = mutable.LinkedHashMap.empty[String, TraitDecl]

  /** Which module licenses what a key names, or `None` for the library's.
   *
   * Asked of the **declaration** rather than of `libraryNames`, which holds a library enum's variant
   * names beside its type names — so a program declaring a `struct Ok` of its own would have been
   * told its own type was the library's.
   *
   * It lives here rather than beside the coherence check that first needed it because it is a table
   * lookup with no phase of its own, and a *diagnostic* has to be able to ask the same question: any
   * message advising a program to write an `impl` is only advice where the coherence rule would let
   * one be written.
   */
  protected def declaringModule(key: String): Option[String] = {
    val decl: Option[Positioned] = structDecls.get(key)
      .orElse(enumDecls.get(key))
      .orElse(traitDecls.get(key))
      .orElse(constrainedDecls.get(key))

    decl match
      // `libraryOwns` rather than the key's module, because the two disagree while the library's own
      // source is what is being compiled: there its declarations *are* the module being built, and a
      // `sysl$Display` would otherwise be reported as belonging to whoever is compiling it.
      case Some(d) if libraryOwns(d, Modules.moduleOf(key)) => None
      case Some(_)                                          => Some(Modules.moduleOf(key))
      // A name nothing declares is a built-in, which has no module of its own and is the library's.
      case None                                             => None
  }

  // --- implementations and members -------------------------------------------------------

  /** Every `impl Trait for Type` as written, in source order. Kept unresolved because the type it
   * names may be declared further down the file; `hoistImpl` resolves each one after every type is
   * registered, and that is where `traitImpls` gets filled.
   */
  protected val implDecls = mutable.ListBuffer.empty[(Scope, ImplDecl)]

  /** The composed types written out in full that implement a trait, keyed by (trait name, the
   * arguments the block **wrote** for it, the **shape** each one has). It is what a shape-matched
   * `impl` consults to find that the thing it would cover has already been covered one type at a
   * time.
   *
   * `impl Display for []int` and `impl[T] Display for []T` are two implementations of `Display` for
   * a `[]int`, and by default whichever is written second is refused — this is how the one written
   * first is found however the file ordered them.
   *
   * The flag is whether the written-out block said **`override`** (`reference/traits.md § override — when the overlap is deliberate`), which is what
   * makes the pair deliberate rather than a mistake. It has to be recorded rather than asked at the
   * shape's own declaration, because the shape may be hoisted either before or after the type it
   * covers and the answer must not depend on which.
   *
   * The trait's arguments are deliberately **not** in the key, though they are what lets one type
   * keep several implementations elsewhere. A shape's members and a written-out type's are filed
   * under two different owner keys, and a member lookup takes the type's own key or the shape's and
   * never both — so two implementations split across that boundary would leave one of them
   * unreachable by name whatever their arguments were. What makes several implementations work at
   * all is that they share a namespace to be told apart in, and here they do not.
   */
  protected val writtenShapes = mutable.LinkedHashMap.empty[(String, String), (String, Boolean)]

  /** The members a composed type written out in full was given, keyed by (its shape, the member's
   * name) and holding the type that was written. A shape-matched block may not give a member of the
   * same name to every type of that shape.
   *
   * A type's members are one namespace whatever trait brought them (`08`), which is why two traits
   * declaring a `show` cannot both be implemented for one type. A shape covers types that may
   * already have a member of the name, so the same rule reaches across the two.
   */
  protected val composedMembers = mutable.LinkedHashMap.empty[(String, String), String]

  /** What `Self` means inside a member of a *generic* type, as the reference it was written from —
   * `Box[T]` for the members of `Box`, whichever declaration form brought them — **and the terms it
   * was written in**.
   *
   * A concrete type's members have their `Self` resolved once, at hoist, into `memberSelf`. A
   * generic type's cannot: `Box[T]` is not a type until a call fixes `T`. So the *reference* is
   * kept, and resolving it under the substitution an instantiation supplies is what gives `Self` its
   * meaning there — the same answer, one step later.
   *
   * The scope travels with it because the reference is the one part of an **inherited default** that
   * the trait did not write: the copy is read in the trait's terms (`Hoisting.defaultHome`), and the
   * subject came from the `impl` block, which may be in another module entirely. For every other
   * member the two are the same scope and carrying it changes nothing.
   */
  protected val genericSelf = mutable.LinkedHashMap.empty[String, (TypeRef, Scope)]

  /** What the **trait's** own type parameters mean inside a member of a generic `impl`, by the name
   * the member was lowered to.
   *
   * `impl[U] From[int] for Box[U]` supplies a member whose signature may be written in the trait's
   * `T`, and a default it inherits certainly is. `U` is fixed per instantiation and `T` is fixed by
   * the block, so the two answers come from different places and only this one has anywhere to wait.
   */
  protected val genericOuter = mutable.LinkedHashMap.empty[String, Map[String, Type]]

  /** The members of a generic `impl`, each as the generic function it was lowered to, for the
   * definition-time pass of `reference/generics.md § Bounds` to walk.
   *
   * A member of a generic *type* is not checked there and cannot be: it inherits the type's
   * parameters, which carry no bounds, so holding it to them would be holding it to nothing. A
   * generic `impl` is the case that changed — the block declares its own parameters and may bound
   * them — so its members are checked once, at the definition, exactly as a bounded generic function
   * is.
   */
  protected val abstractMembers = mutable.ListBuffer.empty[FuncDecl]

  /** Generic `impl` members whose body the definition-time pass reported, by the name each was
   * lowered to. The instantiations made for concrete type arguments are dropped rather than
   * analyzed, so one mistake stays one diagnostic.
   */
  protected val brokenMembers = mutable.HashSet.empty[String]

  /** The method tables the program's trait objects dispatch through, keyed by the global each is
   * emitted under and registered the first time an erasure needs one. A program that never erases a
   * type carries none.
   */
  protected val vtables = new JournaledMap[String, TVtable](journal)

  /** A type's inherent members, keyed by (type name, member name). Methods, properties, and
   * associated functions all live here; each is also lowered to an ordinary function under the
   * mangled name `Type.member`, so calling one is a call and codegen needs no method concept.
   */
  protected val memberDecls = mutable.LinkedHashMap.empty[(String, String), MethodDecl]

  /** The closure structs whose `call` reads its environment and never writes it
   * (`Closures.writesEnvironment`).
   *
   * `call` is declared `*self` on every arity of the call trait, because a closure captures by
   * value and one that counts has to be able to write its own copy. That mode is what a *place*
   * is asked for at a call, so a closure bound with `val` could not be called at all — a name only
   * ever read, refused for being written once.
   *
   * A body that writes none of its captures emits no store, so the address a `*self` receiver takes
   * is used for reading and nothing else, and the refusal protects nothing. This set is what lets
   * the check say so. It holds **concrete** closure structs only: a call through `&Fn(...)` is
   * dispatched on the trait's own declaration, where the mode stays `*self` for every closure alike
   * and the box supplies the place.
   */
  protected val readOnlyClosures = mutable.Set.empty[String]

  /** Every member a type has under one **source** name, where more than one implementation of one
   * trait gave it one — keyed by (type name, the name as written) and holding the names those
   * members are actually filed under, in source order.
   *
   * A type with one `mul` has no entry here and is reached by the name it was written with, which
   * is what keeps the ordinary case a lookup. A `Complex` that is both `Mul[Complex]` and
   * `Mul[real]` has two, and a call naming `mul` is answered by the one whose parameters accept the
   * arguments — a resolution the call fully determines, not an overload set to search
   * (`reference/traits.md § Two traits may declare a member of one name`).
   */
  protected val memberAlts = mutable.LinkedHashMap.empty[(String, String), List[String]]

  /** Which trait a member came from, keyed by (type name, the name it is filed under), for the
   * members an `impl` block brought. A type's **own** members are absent, which is what tells the
   * two apart at a lookup.
   *
   * A member is reachable only where the trait that declared it can be named (`reference/modules.md
   * § Visibility`), so this is what a call is filtered by: an entry here is a question to ask of
   * the use site's scope, and no entry is a member that arrives with its type and is reachable
   * wherever the type is. Without it every trait in a program would share one namespace per type,
   * and the first library to implement a wide trait for a built-in would claim those names from
   * everybody.
   */
  protected val memberTrait = mutable.LinkedHashMap.empty[(String, String), String]

  /** Which trait default a member was copied from, keyed by the name the copy was lowered to.
   *
   * A default is materialized per implementing type (`02`), so one source body becomes several
   * functions. This says which — so a default the definition-time pass already reported is not
   * reported again by every copy of it.
   */
  protected val defaultOrigin = mutable.LinkedHashMap.empty[String, String]

  /** Trait defaults whose body the definition-time pass reported, by the `Trait.method` name each
   * was checked under. The copies made for the implementing types are dropped rather than analyzed.
   */
  protected val brokenDefaults = mutable.HashSet.empty[String]

  /** A member of a *generic* type, lowered to a function that is itself generic over the type's
   * parameters and keyed by (type name, member name). Unlike a member of a concrete type — which
   * is hoisted eagerly into `funcInsts` — a generic member is instantiated on demand at each call
   * site, once the receiver's concrete type arguments are known.
   */
  protected val genericMembers = mutable.LinkedHashMap.empty[(String, String), FuncDecl]

  /** The function a member was lowered to, keyed the way the member table is — (type name, the name
   * it is filed under).
   *
   * The two names are not the same string and cannot be derived from each other: a member is *filed*
   * under the key an `impl` may name and *lowered* under that type mangled, and a type an `impl` may
   * name is not always a name — `[]int` is a fine key and an impossible LLVM symbol, so its members
   * are emitted under `slice.int`. Everything that starts from a receiver's **type** builds the
   * lowered name with `Type.memberSymbol`; this is for the passes that start from the member table
   * instead and have no type in hand.
   *
   * What it buys is the member's resolved signature: `funcInsts` holds one under the lowered name
   * for every member of a concrete type, so a pass reading a declaration can ask what a parameter's
   * type actually came out as. A generic type's members have no entry there — their signatures are
   * written in terms nothing has fixed — and are absent here for the same reason.
   */
  protected val memberFuncs = mutable.LinkedHashMap.empty[(String, String), String]

  /** What `Self` means inside one lowered member, keyed by the name it was lowered to.
   *
   * A member of a concrete type — its own, or one an `impl` gave it — may write `Self` for the type
   * it belongs to, in its signature and in its body alike (`reference/traits.md § Declaring a
   * trait`). The binding is recorded at hoist and folded into the body's substitution, so the body
   * resolves `Self` exactly as it resolves a type parameter: through the one map that already
   * answers that question.
   */
  protected val memberSelf = mutable.LinkedHashMap.empty[String, Map[String, Type]]

  // --- what instantiation made -----------------------------------------------------------

  /** Instantiated types, keyed by their display name (`Point`, `Option[int]`) and held in
   * dependency order — a type is inserted only after the types it contains.
   */
  protected val structInsts = new JournaledMap[String, Type.Struct](journal)
  protected val enumInsts   = new JournaledMap[String, Type.Enum](journal)

  /** Where an in-progress instantiation was entered: how many `*T` / `&T` wrappers the resolver was
   * inside, and how many **type argument** positions it was inside.
   *
   * Both are needed and they answer different halves of the same question. The indirection says
   * whether a cycle back to this type has anything pointing on it. The argument count says whether
   * the path that got back here left through an argument list *after* this type was entered — which
   * is not containment and is somebody else's question — as against having been inside one all
   * along, where it is this type's own business again.
   */
  protected case class Entered(indirection: Int, typeArgs: Int)

  /** Instantiations whose fields are still being resolved, each recorded with where it was entered.
   * A type that reaches itself finds its own entry here, and `cycleCheck` is what reads it.
   */
  protected val resolving = mutable.LinkedHashMap.empty[String, Entered]

  /** The same instantiations, by display name, so a recursive occurrence resolves to the object
   * whose fields are still being filled in rather than starting a second one.
   */
  protected val inProgress = mutable.LinkedHashMap.empty[String, Type]

  /** How many `*T` / `&T` wrappers the resolver is currently inside. */
  protected var indirection = 0

  /** How many **type argument** positions the resolver is currently inside.
   *
   * A type argument is not itself containment: `Buf[Node]` holds a `[]Node`, which is an
   * indirection, and `Wrap[Node]` over `struct Wrap[T] { x: T }` holds a `Node` by value. Which of
   * those it is is the *instantiated* type's business and cannot be read off the argument, so
   * reaching an in-progress type here is left alone and the question is asked again where the
   * substitution is used — see `cycleCheck` and the `subst` case in `resolveShape`.
   */
  protected var typeArgDepth = 0

  /** Instantiated function signatures, keyed by the name codegen will emit. */
  protected val funcInsts = new JournaledMap[String, (List[(String, Type)], Type)](journal)

  /** Every `extern` the program declares. A call to one resolves exactly as a call to a sysl
   * function does — the signature is in `funcInsts` like any other — so this exists only to say
   * *which* names have no body: codegen declares them instead of defining them, and the escape
   * analysis assumes the worst of them.
   */
  protected val externDecls = mutable.LinkedHashMap.empty[String, ExternDecl]

  /** The externs something in the program actually calls, in the order they were first reached.
   * An unused one is not declared in the output at all.
   */
  protected val externsUsed = new JournaledSet[String](journal)

  /** Every `extern` **variable** the program declares (`reference/ffi.md § An extern also declares a variable`), by key. Storage the linker supplies
   * rather than storage this module lays down, which is the whole of what this table says about it:
   * a reference to one is the same `TGlobal` a module-level `val`'s name becomes, and the only thing
   * downstream reads this for is which symbols to declare and whether the name is one at all.
   */
  protected val externVarDecls = mutable.LinkedHashMap.empty[String, ExternVarDecl]

  /** Every reference to a **local** the walk resolved, recorded by `Scoping.lookupOpt`.
   *
   * Every other kind of name is recovered afterwards from the typed tree, which names what it
   * resolved to — see `DefinitionIndex`. A local cannot be: what a typed node carries is the unique
   * name, and that is unique within a function and reused freely across them, so there is nothing to
   * look it up in. Resolution is the only moment both halves are in hand at once, so it is where
   * this is filled.
   *
   * Nothing in compilation reads it.
   */
  protected val references = mutable.ListBuffer.empty[Reference]

  /** Whether to fill `references` at all — **off unless an editor asked**, which `Analyzer.indexed`
   * is what does.
   *
   * `lookupOpt` is one of the busiest functions in the analyzer, and every name in every file of
   * the library goes through it. A `Reference` per resolution is a real allocation on a path that
   * compiles programs nobody is looking at, for an answer nobody is going to read, so an ordinary
   * build pays a boolean test and nothing else.
   */
  protected var recordingReferences = false

  /** What each **instantiation** of a generic function was made from, by the name the instantiation
   * goes under.
   *
   * `instantiateFunc`'s own comment says why this cannot be derived: the mangled name it builds
   * "cannot be read back to say so". `abstractInsts` beside it records the same thing for the
   * definition-time pass alone, because that is all that pass needed; this records it for every
   * instantiation, so that a call to a generic can be traced back to the declaration a reader would
   * want to open.
   */
  protected val funcOrigin = mutable.LinkedHashMap.empty[String, String]

  /** The extern variables something in the program actually reads or writes, in the order they were
   * first reached — the same accounting the externs above get, for the same reason.
   */
  protected val externVarsUsed = mutable.LinkedHashSet.empty[String]

  /** Every function name something has called, which is what decides whether a **library** function
   * is worth analyzing and emitting at all, in either half of it: the printing surface is the
   * largest thing that hangs off this, and a program that never prints should carry none of it.
   */
  protected val funcsUsed = new JournaledSet[String](journal)

  /** Instantiations whose body has not been analyzed yet. Queued rather than analyzed inline
   * so an instantiation discovered mid-function does not disturb the enclosing context.
   */
  protected val pending = mutable.Queue.empty[(String, FuncDecl, Map[String, Type])]

  /** The instantiations of a slice shape block made real at the **read-only** view, by mangled name
   * (`TraitLookup.viewedConst`).
   *
   * What reads it is the write diagnostic, and that is the whole of why it is recorded. A block
   * written `impl[T] Sort for []T` whose body writes through `self` is refused when a `[]const T`
   * receiver asks for it, and the refusal lands in the *library's* file on a line whose author did
   * nothing wrong — so the message has to say that this body is one of two, and which one.
   */
  protected val constSelfInsts = mutable.HashSet.empty[String]

  /** Which lowered functions are a **generic's** body at some choice of type, by mangled name.
   *
   * An instantiation's name says nothing about where the choice came from: `lib$twice.Loud` is
   * spelled from the declaration's module and the caller's type, and `Modules.moduleOf` reads only
   * the first half. So what a module promised about its own conduct cannot be asked of an
   * instantiation — the answer would hold the library to a type it never saw — and this is what
   * lets the promise be asked of the generic's **own** body instead (`reference/modules.md § A
   * generic answers for what it wrote, not for what its caller chose`).
   */
  protected val genericInsts = mutable.HashSet.empty[String]

  /** Every generic body as the definition-time pass of `reference/generics.md § Bounds` analyzed
   * it: each type parameter standing for itself, and each call through a bound naming the
   * **trait's** member rather than whatever an instantiation would substitute.
   *
   * That is the one form in which a generic's own conduct can be read. `s.put(msg)` in it is
   * `Sink.put`, which is no function the program links, while a call the body makes to something
   * concrete keeps the name it always had — so what the declaring module wrote and what its caller
   * chose are told apart by the names alone, before substitution makes them identical.
   */
  protected val abstractFuncs = mutable.ListBuffer.empty[TFunc]

  /** What a call **inside** one of those bodies to another generic named, against the declaration it
   * came from.
   *
   * A generic calling a generic instantiates it at whatever the outer one's parameters stand for, so
   * the call reads `lib$grow.T` — a name no program ever links, since the walk that made it throws
   * its instantiations away. Without this the call leads nowhere and a module could promise `no
   * alloc` and then reach an allocator through a one-line generic of its own, which is the promise
   * being worth nothing. With it the abstract body is what the name leads to, which is the same
   * answer the rest of this gives one step further out.
   */
  protected val abstractInsts = mutable.HashMap.empty[String, String]

  /** Every enum variant name maps to the enums declaring one of that name, so a bare `Circle(5)` or
   * `Empty` resolves without qualification.
   *
   * **It is a list because a variant belongs to its enum rather than to the module**
   * (`reference/types.md § Enums`), so two enums in one module may each name a variant `Failed` and
   * neither has to be renamed. What picks between them is `variantOwners.of`: the expected type
   * where there is one, and otherwise the single visible candidate — a bare name with two answers
   * and nothing to choose by is a diagnostic pointing at the qualified `Enum.Variant` spelling,
   * never a guess.
   *
   * Declaration order is kept, which is what makes an ambiguity message name the candidates in the
   * order the file declares them rather than in whatever order a hash produced.
   */
  protected val variantOwners = mutable.LinkedHashMap.empty[String, List[String]]

  /** The bodies of the closures met so far, lowered where they were written
   * (`reference/expressions.md § Closures`).
   *
   * A closure is analyzed inline rather than queued, because its result type is what its body
   * yields and nothing else can say what that is. So its function is finished at the moment the
   * literal is read, and waits here to be added to the program with the rest of them.
   */
  protected val closureFuncs = mutable.ListBuffer.empty[TFunc]

  // --- asking two types whether they agree ------------------------------------------------

  /** Whether a value of type `got` genuinely cannot stand where a `want` was asked for — an
   * argument against a parameter, a returned value against a declared result.
   *
   * Two types agree for reasons of their own. A type that could not be worked out agrees with
   * everything, in either direction: the mistake that produced it has been reported, and a second
   * complaint about what it fails to match is noise about a consequence rather than a cause. A
   * `never` agrees with everything in *one* direction only — it may stand anywhere, because control
   * does not reach the place the value would have been used, but nothing may stand for it.
   */
  protected def disagree(got: Type, want: Type): Boolean =
    Type.repr(got) != Type.repr(want) && got != Type.Unknown && want != Type.Unknown && got != Type.Never

  /** The one type two alternatives meet at — the branches of an `if`, the arms of a `match`, a
   * loop's `break` values and its `else` — or `None` when they have no common type.
   *
   * The only interesting case is `never`: an alternative that does not finish constrains nothing,
   * so it takes the other side's type. Everything else must already agree, since sysl has no
   * subtyping among concrete types to widen towards.
   */
  protected def join(a: Type, b: Type): Option[Type] =
    if a == b then Some(a)
    else if a == Type.Never then Some(b)
    else if b == Type.Never then Some(a)
    // Two transparent-compatible types — a subtype and its base, or two subtypes over one base —
    // meet at that base, since either may stand where the base is asked for.
    else if Type.repr(a) == Type.repr(b) then Some(Type.repr(a))
    // The two views of one slice, which are one type with a bit and not two types: the writable one
    // is accepted wherever the read-only one is wanted, so the pair has a meeting point and it is
    // the read-only one. This is the same rule `Literals.meetViews` applies to a pair of operands,
    // and it is here for the reason it is there — without it, which branch of the `if` was written
    // first decided whether the form had a type at all.
    else if Type.constView(a) == Type.constView(b) then Some(Type.constView(a))
    else None

  protected def show(t: Type): String = Type.show(t)

  /** The type parameters of a nominal type — a struct or an enum — by the name it was declared
   * under. Empty both for a non-generic type and for a name that declares no type at all, which is
   * what lets a caller ask "is this generic" without first knowing which kind it is.
   */
  protected def nominalTparams(base: String): List[String] =
    structDecls.get(base).map(_.tparams).orElse(enumDecls.get(base).map(_.tparams)).getOrElse(Nil)

  /** What a nominal type asks of its own parameters, by parameter name. Empty for a type that asks
   * nothing, which is every type that takes no parameters and most of those that do.
   */
  protected def nominalBounds(base: String): Map[String, List[BoundRef]] =
    structDecls.get(base).map(_.bounds).orElse(enumDecls.get(base).map(_.bounds)).getOrElse(Map.empty)

  /** Which of a nominal type's parameters stand for **values** (`reference/generics.md § A
   * parameter may stand for a value`), and at what type — the `N` of `struct Buf[const N: usize]`.
   * Empty for every type that declares none, which is most of them, and answered without the caller
   * knowing which kind of declaration it is asking about.
   */
  protected def nominalValues(base: String): Map[String, TypeRef] =
    structDecls.get(base).map(_.tvalues).orElse(enumDecls.get(base).map(_.tvalues)).getOrElse(Map.empty)
}
