package sh.sysl

/** The semantic pass: it resolves names, checks types, and turns the untyped `Program` into
 * a typed `TProgram` that codegen lowers directly. All diagnostics live here; codegen trusts
 * the tree it is handed.
 *
 * The work is split across traits mixed into this class, the way codegen is split across
 * `Emitter` and friends: `AnalyzerBase` holds the shared tables and name scopes, `TypeResolution`
 * resolves and instantiates types, `Literals` handles the scalar leaves, `Hoisting` registers
 * declarations, `StmtAnalysis` handles statements and blocks, `CallAnalysis` handles calls and
 * construction, `PatternAnalysis` handles `match`, `SpecialForms` holds the handful of call forms
 * the compiler resolves by name, `ProgramWalk` runs the passes in order, and `ExprAnalysis` is the
 * expression dispatch and places.
 *
 * `ProgramWalk` has four of its own underneath it, in the order the driver reaches them:
 * `ModuleFiles` (what a file contributes to its module, and which file the program starts in),
 * `ModuleStorage` (the `val`s, `var`s and `extern` variables a module lays down), `AbstractBodies`
 * (`reference/generics.md § Bounds`'s definition-time check of every generic body) and
 * `FunctionBodies` (what a body is checked against, and analyzing one inside another).
 *
 * What stays in this file is the class that mixes them together and the entry point that runs it.
 * The recursive entry points the traits call back into — `analyzeExpr`, `analyzePlace`,
 * `analyzeBlockBody` — are declared abstract in `AnalyzerBase` and implemented in whichever trait
 * owns them, so a trait may call a form it does not itself analyze.
 *
 * Declarations are hoisted, so functions, structs, and enums may be used before they appear
 * and may be mutually recursive. Each function (and the synthetic `main` around the top-level
 * statements) is its own naming context: a variable that shadows an outer one is renamed to a
 * unique register name, which keeps codegen's per-function SSA names distinct without the
 * analyzer having to understand LLVM.
 *
 * **Generics are monomorphized here.** A generic declaration is kept in its untyped form and
 * instantiated on demand: each distinct set of type arguments produces its own `Type.Struct` /
 * `Type.Enum` / `TFunc` under a mangled name, and codegen never sees a type parameter. Type
 * arguments are inferred from the argument types at a call or construction, and from the
 * *expected* type when the arguments alone do not determine them — which is what lets `None`
 * and `Ok(5)` take their type from the context they appear in.
 */
class Analyzer private[sysl] (
    protected val units: List[Program],
    protected val building: Set[String],
    protected val std: Stdlib,
    protected val target: Target,
    protected val provides: Set[String],
    protected val packages: Packages,
    protected val own: Option[Set[String]],
    protected val hasEntryPoint: Boolean,
) extends ProgramWalk with ExprAnalysis with DefinitionIndex {

  /** Every error the walk found, in source order and **unrendered** — see `Diagnostic`. */
  def errors: List[Diagnostic] = diagnostics

  /** Where the things this program names were declared — see `DefinitionIndex`. It reads the tables
   * the walk filled, so it is asked after the walk and not before.
   */
  def references(tree: TProgram): List[Reference] = referencesIn(tree)

  /** Whether this walk should record where each local reference resolved to. Off by default, since
   * only an editor wants it and every name in the library goes through the path that fills it.
   */
  def recordReferences(on: Boolean): Unit = recordingReferences = on
}

object Analyzer {

  /** Analyzes a program to a typed tree, or returns every error it found, rendered and in source
   * order.
   *
   * The walk itself never stops at the first mistake — each declaration, function body, and
   * statement is a recovery region — so what comes back on the left is the whole list. An error
   * escaping the regions entirely is still caught here, since a diagnostic that reaches the user
   * beats a stack trace.
   */
  def analyze(program: Program): Either[List[Diagnostic], TProgram] = analyze(List(program))

  /** Analyzes the files of one module together. They share a single scope, so a declaration in one
   * is visible to all of them with no ordering and no forward declaration (`reference/modules.md §
   * The module graph is acyclic`) — which falls out of hoisting, since the pass that registers
   * every signature already runs over the whole set before any body is checked.
   */
  /** `target` is here for the reason it is a parameter everywhere else: a few rules are the
   * *machine's* rather than the language's, and a diagnostic about one has to be raised where
   * diagnostics are raised. `reference/ffi.md § interrupt`'s calling conventions are the case —
   * whether `interrupt` exists at all, and what signature it demands, differ per processor.
   */
  /** `paths` is here for `target`'s reason one step further out: a `c const` is evaluated by the C
   * compiler, and a C compiler that has not been told where the headers are cannot evaluate one
   * (`SearchPaths`). It reaches nothing else in the analyzer.
   */
  /** `own` names the modules this compilation is **producing**, as against the ones handed to it —
   * a `--lib` source root, a fetched package, and the standard library, which is handed by every
   * compilation there is (`Compiler.ownModules`). `None` says a caller had nothing to name, and then
   * every module is the program's own.
   *
   * It is here for the reason `building` is: `units` is one flat list and nothing in a tree says
   * which road it arrived by, so a rule that has to tell a program's own file from a library's has to
   * be told. What reads it is the target half of `Capabilities` — the only rule in the walk whose
   * answer is the **machine's** and therefore the program author's rather than a library author's.
   */
  def analyze(units: List[Program], building: Set[String] = Set.empty,
              std: Stdlib = Stdlib.fromSource(Target.default), target: Target = Target.default,
              provides: Set[String] = Capability.core.toSet, packages: Packages = Packages.none,
              paths: SearchPaths = SearchPaths.none, own: Option[Set[String]] = None,
              entryPoint: Boolean = true)
      : Either[List[Diagnostic], TProgram] =
    CProbe.lower(units, target, paths) match
      case Left(d)       => Left(List(d))
      case Right(lowered) =>
        val walked = analyzing(lowered, building, std, target, provides, packages, own, entryPoint,
          recording = false)

        walked.index match
          case Some(i) if walked.isClean => Right(i.tree)
          case _                         => Left(walked.problems)

  /** The same walk, answering with the **definition index** beside the tree — what an editor asks
   * for and what nothing in a build has any use for (`DefinitionIndex`).
   *
   * It is a second entry point rather than a wider return type for the same reason `Compiler.checked`
   * is: every existing caller wants a tree and would have to unwrap one, and the index is read off
   * tables the analyzer instance holds, so it cannot be asked for after the fact.
   *
   * **It answers an `Indexing` rather than an `Either`, and the difference is the point of it.** A
   * file with a mistake in it is the ordinary state of a file in an editor, and a walk that ran to
   * the end and recorded that mistake has a complete tree and complete tables. Reporting only the
   * diagnostic threw away an index that was already built and paid for — so both come back, and it
   * is the caller that decides what a partial answer is worth. Only a walk that escaped before
   * producing a tree has nothing.
   */
  def indexed(units: List[Program], building: Set[String] = Set.empty,
              std: Stdlib = Stdlib.fromSource(Target.default), target: Target = Target.default,
              provides: Set[String] = Capability.core.toSet, packages: Packages = Packages.none,
              paths: SearchPaths = SearchPaths.none, own: Option[Set[String]] = None,
              entryPoint: Boolean = true)
      : Indexing =
    CProbe.lower(units, target, paths) match
      case Left(d)        => Indexing(None, List(d))
      case Right(lowered) =>
        analyzing(lowered, building, std, target, provides, packages, own, entryPoint,
          recording = true)

  /** The walk itself, over units whose `c const` blocks are already ordinary constants.
   *
   * **The lowering is a separate step in front of this rather than a pass inside it**, and that is
   * what keeps the rest of the compiler from knowing the feature exists: by the time a name is
   * resolved or a bound is folded, a `c const` is the `const` of `reference/modules.md § const — a value` and nothing else. It is
   * here — at the analyzer's door — rather than in the parser, because `SyslParser.parse` has seven
   * callers and this has one.
   */
  private def analyzing(units: List[Program], building: Set[String], std: Stdlib, target: Target,
                        provides: Set[String], packages: Packages, own: Option[Set[String]],
                        entryPoint: Boolean, recording: Boolean)
      : Indexing = {
    val analyzer = new Analyzer(units, building, std, target, provides, packages, own, entryPoint)

    analyzer.recordReferences(recording)

    val outcome =
      try Right(analyzer.analyze())
      catch
        case AnalyzerError(msg, pos, _) => Left(List(Diagnostic(msg, pos)))
        // A poisoned region carries no message of its own: it means an error was already
        // recorded, and those are what the caller is told about.
        case Poisoned() => Left(Nil)

    val found = analyzer.errors

    outcome match
      // The references are gathered only where they were asked for: `referencesIn` walks the whole
      // tree, and a build has no use for the answer. A tree with mistakes recorded against it still
      // yields an index — that is the case an editor is in nearly all the time.
      case Right(tree) =>
        Indexing(Some(Indexed(tree, if recording then analyzer.references(tree) else Nil)), found)
      case Left(escaped) =>
        val all = found ::: escaped

        // Reaching here with nothing to say would mean the analyzer gave up without recording
        // why, which is a bug in the analyzer rather than in the program it was handed.
        if all.isEmpty then Indexing(None, List(Diagnostic("the analyzer stopped without reporting why", None)))
        else Indexing(None, all)
  }
}

