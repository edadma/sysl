package sh.sysl

import scala.collection.mutable

/** Who may name what a `@tests` file declares (`reference/attributes.md § @tests — a file of scaffolding`).
 *
 * The header says two things and this is the second of them. The first — that every build but a test
 * build drops the file — is `Tests.strip`'s, and on its own it would be unsound: a program that
 * called a helper would compile here and fail at the link, with a message about a missing symbol
 * rather than about the line that named it.
 *
 * So the rule is stated over the **referring declaration** rather than over the file it sits in:
 *
 *   - a declaration in a `@tests` file may name another, since the two are dropped together;
 *   - a `@test` function may name one wherever it was written, because `reference/attributes.md § @tests — a file of scaffolding` puts a test
 *     beside what it tests and `Tests.strip` drops it in the same builds;
 *   - a hook may name one for the same reason it is dropped by the same builds
 *     (`reference/attributes.md § The hooks a module may write`) — a `@setup` exists to prepare what
 *     the module's tests are written against, so the scaffolding they may name is the scaffolding it
 *     may name;
 *   - anything else may not, and is told so where it wrote the name.
 *
 * **A closure is judged by the body it was written in, not by the name it was filed under.** It is
 * lowered to a function of its own under a name no reader wrote, so on its own terms it belongs to
 * no file and would be held to the third rule while sitting inside something the first two exempt —
 * a lambda in a test naming that test's own helper, reported as though a shipped function had named
 * it. `Closures.lowerClosure` answers that where the answer is still known, by putting the lowered
 * name into `testOnlyDecls` when the enclosing body is one a test build keeps. That makes the
 * exemption and the drop the same fact rather than two that have to agree.
 *
 * That the two halves agree is what makes the drop safe rather than lucky: every reference into a
 * test file comes from something dropped in exactly the builds the file is, so a tree that has been
 * stripped can hold no reference to anything that went with it.
 *
 * **Asked of the typed tree**, for the reason `Purity`'s question is: a name reaches a declaration
 * through a dozen paths in the analyzer — a call, an operator that lowered to one, a function's
 * address, a read of module storage — and a guard at each is a list a thirteenth path would quietly
 * not join. A reference is a *node*, so the nodes are the whole answer.
 */
trait TestScope extends AnalyzerBase {

  private val reported = mutable.Set.empty[Pos]

  /** Reports every reference into a `@tests` file from something that is not itself test-only.
   *
   * `main` is walked with the functions because a program's top-level statements are its entry point
   * (`reference/modules.md § Where a program starts`) and are dropped by no build at all — so a helper named there is the plainest case of
   * the mistake, and the one a reader is likeliest to make while moving code out of a test.
   */
  protected def checkTestScope(funcs: List[TFunc], main: List[TStmt], testOnly: Set[String],
                               scaffolding: Set[String]): Unit = {
    if testOnly.isEmpty then return

    reported.clear()

    for f <- funcs if !testOnly(f.name) && !scaffolding(f.name) do
      scan(f.body, testOnly, None)
      f.requires.foreach((c, _) => scan(c, testOnly, None))
      f.ensures.foreach((c, _) => scan(c, testOnly, None))
      f.variant.foreach(scan(_, testOnly, None))

    scan(main, testOnly, None)
  }

  /** What one node names, where that is a declaration a test file wrote, or nothing.
   *
   * The three are the ways such a declaration can be named at all: called, had its address taken, or
   * read as storage.
   *
   * **The two dispatching forms are absent, and neither is a hole.** A trait-object call names a
   * *slot* and which body answers it is settled while the program runs; an operator that a trait
   * supplies carries the method it lowers to as data riding on the node (`TDispatch`). Both reach a
   * function only through a method table, and every entry in one is put there by an `impl` — which a
   * `@tests` file may not write. So a declaration in such a file is never a trait method, and there
   * is no path to it but the three below.
   */
  private def named(e: TExpr, testOnly: Set[String]): Option[String] = e match
    case TCall(name, _, _, _) if reportable(name, testOnly)    => Some(name)
    case TFuncAddr(name, _, _) if reportable(name, testOnly)   => Some(name)
    case TGlobal(symbol, _, _) if reportable(symbol, testOnly) => Some(symbol)
    case _                                                     => None

  /** Whether naming this is a mistake worth telling somebody about.
   *
   * A **lowered** closure is in the set and is not, and the case that says why is a generic taking a
   * callable: `is_sorted_by(xs, (a, b) -> a < b)` instantiates the library's own function at the
   * closure's type, and the `lt(…)` inside that instantiation is a direct call on the closure's body.
   * The instantiation is an ordinary library function by then, so the walk arrives at it — and what it
   * would say is that `$closure4.call` may not be named here, which is a name the program does not
   * contain and the reader cannot go and look at.
   *
   * **Nothing is given up by staying quiet.** The rule exists so that a stripped tree holds no
   * reference to what went, and an instantiation keyed on a test's closure type can only have been
   * demanded by that test — so it goes when the test does, which is `Reachability.prune`'s answer
   * rather than this pass's. What is left for this pass is exactly the names a reader wrote.
   */
  private def reportable(name: String, testOnly: Set[String]): Boolean =
    testOnly(name) && !Closures.lowered(name)

  /** Walks a tree, reporting each reference into a test file and going no deeper into one.
   *
   * The descent is through the shape of the tree rather than a case per node, for the reason
   * `Purity`'s and `Reachability`'s are: a node added later is walked without anyone remembering to
   * come back here. `where` carries the innermost position down so that a synthesized node — an
   * operator that became a call, a `print` that became two — is reported at the line the reader
   * wrote rather than wherever the walk was last looking.
   */
  private def scan(x: Any, testOnly: Set[String], where: Option[Pos]): Unit = {
    val here = x match
      case p: Positioned if p.pos.isDefined => p.pos
      case _                                => where

    x match
      case _: Type => ()
      case e: TExpr if named(e, testOnly).isDefined =>
        report(here, named(e, testOnly).get)
      case xs: Iterable[?] => xs.foreach(scan(_, testOnly, here))
      case p: Product      => p.productIterator.foreach(scan(_, testOnly, here))
      case _               => ()
  }

  private def report(where: Option[Pos], key: String): Unit =
    if where.forall(reported.add) then
      recover(())(at(where)(err(
        s"'${Modules.show(key)}' is declared in a file that said '@tests', so it is there for the " +
          "module's tests and no build but 'sysl test' keeps it — only another such file, or a " +
          "'@test' function, may name it")))
}
