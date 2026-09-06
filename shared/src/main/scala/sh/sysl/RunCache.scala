package sh.sysl

import io.github.edadma.cross_platform.*

/** The executable `sysl run` built last time, kept so that running the same program twice costs the
 * second one nothing (card `0309`).
 *
 * **The measurement it was filed on.** `sysl run . -- hello.sl` in a 9,600-line module with three
 * git dependencies took **9.2 seconds for a program whose body is `print("hi")`**, and the same 9.2
 * seconds whatever the program did; the built binary ran the same hello-world in 0.004. So the whole
 * of that was compilation, repeated on every invocation with nothing changed between them — and it
 * hides inside what looks like the program's own cost. Four timings taken while benchmarking a
 * collector read 9.6–10.1 seconds and appeared to say the workloads were indistinguishable; they
 * were 9.2 seconds of rebuild plus 0.02–0.9 seconds of work, and the real ratio was about 45x.
 *
 * ==What the key is over, and why it is long==
 *
 * **Getting a cache key wrong is a stale-binary bug, which is worse than a slow build**, so this errs
 * toward rebuilding: everything that can reach the bytes is in the key, and anything that cannot be
 * cheaply established is a miss rather than a guess.
 *
 *   - the **compiler**, by version. Two releases never share an entry, exactly as they never share a
 *     standard-module entry, and for the same reason: an executable is compiled code, so a release
 *     that changes what a program lowers to while touching none of its source produces different
 *     bytes at an identical fingerprint.
 *   - the **target**, the **allocator** pair and the **optimization** flag, each of which changes
 *     what is emitted.
 *   - every **source file** the program is made of — its own, every `--lib` source root's, and the C
 *     each of those trees carries — by `LibraryArtifact.fingerprint`, which is over each file's place
 *     in its tree and its text.
 *   - every **artifact** named with `--lib`, by its bytes.
 *   - the **standard module** it compiles against, by whichever of the three it is: a named
 *     `--std-lib`, the library's own fingerprint, or the from-source build `--no-std-lib` asks for.
 *   - the **search paths** and the **link line**, which decide what the C compiler and the linker do
 *     with input this key has otherwise covered.
 *   - the **environment that reaches the toolchain** (`Toolchain.buildEnvironment`) — the extra
 *     clang flags, and the variables naming a cross toolchain. This is the only part of the key
 *     that is neither the command line nor the tree, and it was **missing until card `0415`**: a
 *     run under `SYSL_EXTRA_CFLAGS="-fsanitize=address"` over an unchanged tree replayed the
 *     uninstrumented binary the previous ordinary run had left in the slot and reported green.
 *
 * **What is deliberately NOT in it: the program's own arguments.** `sysl run p -- a b` and
 * `sysl run p -- c d` are one binary run twice, which is the whole point.
 *
 * ==What it does not cover, stated rather than hidden==
 *
 * A **development tree**, where the compiler changes under a constant `BuildInfo.version`. Nothing in
 * a cache can distinguish those without hashing the compiler itself, and it is the same hole the
 * standard-module cache has and names. `SYSL_NO_CACHE` is the escape hatch, and a `sysl build`
 * never consults this at all.
 *
 * An **`#include`d header** outside the trees this hashes, which a `c const` block or a vendored C
 * file may reach. The same is true of the standard-module cache, and the same answer applies.
 */
object RunCache {

  /** Set to anything non-empty to compile every time. It is for working **on the compiler**, where
   * the version in the key stands still while the bytes it produces do not.
   */
  private val Off = "SYSL_NO_CACHE"

  /** How many entries a program's cache holds before eviction is considered at all.
   *
   * The standard-module cache evicts nothing, which is right for it — one entry per library per
   * release, and a developer has a handful. This one takes an entry per *edit*, so a week's work on
   * one program leaves hundreds, and unbounded is the wrong answer at that rate.
   */
  private val Keep = 256

  /** How old an entry has to be before it is a candidate, in milliseconds — a week.
   *
   * **Age is what makes eviction safe beside a build that is running**, which is the half a bare
   * count gets wrong. Several compilations share one cache directory, and a hit is `isFile` and then
   * an `exec`: an eviction that took a *fresh* entry could take one another process was about to
   * run, or had already checked for. Nothing this old is in flight anywhere, so the two cannot meet.
   *
   * By last-modified rather than by last-read: a read that touched the file would make every run a
   * write, and what the bound protects is disk rather than correctness — the worst an eviction costs
   * is one rebuild.
   */
  private val Stale = 7L * 24 * 60 * 60 * 1000

  /** Where a built program is kept, or nothing where this machine has no cache directory — the same
   * condition the standard module's own path falls back on, and the same answer: behave as though
   * there were no cache.
   */
  private def slot(key: String): Option[String] =
    if disabled || envVar(Off).exists(_.nonEmpty) then None
    else root.map(c => s"$c/sysl/run/$key")

  /** The directory this cache lives under, which is the machine's unless a caller has said
   * otherwise — the same shape `Fetch.usingCache` has, and for the same two reasons: a suite that
   * counted entries in the developer's real cache would be measuring their morning, and one that
   * wrote into it would be spending their disk.
   */
  /** **Per thread rather than per process**, which `Fetch`'s equivalent is not and which this one
   * has to be: a suite that redirects the cache so it can count what lands in it runs beside other
   * suites driving the same compiler, and a process-wide override would collect their entries as
   * well as its own. A thread-local is exact — the redirect covers the calls the redirecting thread
   * makes and no others — and costs a compilation nothing, since the ordinary answer is `None`.
   */
  private val override_ = new ThreadLocal[Option[String]] {
    override def initialValue(): Option[String] = None
  }

  private val off = new ThreadLocal[Boolean] {
    override def initialValue(): Boolean = false
  }

  private def disabled: Boolean = off.get

  private def root: Option[String] = override_.get.orElse(cacheDirectory)

  private[sysl] def usingCache[T](path: String)(body: => T): T = {
    val saved = override_.get

    override_.set(Some(path))
    try body
    finally override_.set(saved)
  }

  /** The escape hatch, reachable from a test without an environment to set. */
  private[sysl] def disabledFor[T](body: => T): T = {
    val saved = off.get

    off.set(true)
    try body
    finally off.set(saved)
  }

  /** The program built for this key, where one is there and is still executable. */
  def hit(key: String): Option[String] = slot(key).filter(p => isFile(p) && isExecutable(p))

  /** Where to link, for a build that is about to happen: a path in the cache with the directory
   * already made, or nothing where there is no cache to write into.
   *
   * The build links **straight into the slot** rather than to a temporary that is copied afterwards.
   * A copy would have to reproduce the executable bit, which is a thing `cross_platform` does not
   * offer and a thing to get wrong once per platform; linking there gets it from the linker.
   */
  def reserve(key: String): Option[String] =
    slot(key).flatMap { path =>
      try
        Project.parentOf(path).foreach(Project.makeDirectories)
        evict(path, newest(path))
        Some(path)
      catch case _: Exception => None
    }

  /** Entries past [[Keep]] that are also older than [[Stale]], removed. A failure here is not a
   * failure of the build: the worst an unevicted cache costs is disk, and the worst a failed
   * eviction costs is nothing at all.
   *
   * `now` is read from the newest entry rather than from a clock, so that a machine whose clock has
   * moved evicts nothing rather than everything.
   */
  private def evict(fresh: String, now: Long): Unit =
    for dir <- Project.parentOf(fresh) do
      try
        val kept = listFiles(dir).toList.filterNot(_ == fresh)

        if kept.length >= Keep then
          for old <- kept if now - lastModified(old) > Stale do
            try deleteFile(old)
            catch case _: Exception => ()
      catch case _: Exception => ()

  /** The most recent entry's timestamp, which stands in for the clock. A directory with nothing in
   * it answers zero, which evicts nothing — the right answer for a cache being made.
   */
  private def newest(fresh: String): Long =
    Project.parentOf(fresh).toList
      .flatMap(dir => try listFiles(dir).toList catch case _: Exception => Nil)
      .map(lastModified)
      .maxOption
      .getOrElse(0L)

  /** The **test list** beside a cached test binary, which is the one thing `sysl test` needs that
   * the executable does not carry: what to call, what to report it as, whether it should trap and
   * what it should have printed on its way out.
   *
   * A sidecar rather than a second cache, so the two cannot come apart — the binary is written
   * first, and a hit needs both.
   *
   * The encoding is one line per test with `\u0000` between the fields, which is the one character
   * a `@test`'s display name cannot hold: it is read out of source, and a NUL does not survive the
   * lexer. A line that will not read is treated as no cache at all, which is a rebuild.
   */
  def tests(key: String): Option[String] = slot(key).map(_ + ".tests")

  def encode(ts: List[TTest]): String =
    ts.map(t => (List(t.func, t.display, t.shouldTrap.toString, t.expected.getOrElse(""),
                      if t.expected.isDefined then "1" else "0", t.file, t.line.toString) :::
                 HookKind.values.toList.flatMap(k => hookFields(t.hooks, k)))
             .mkString("\u0000")).mkString("\n")

  /** One hook as three fields, empty where the module declared none. Three flat fields rather than
   * one packed one, because the separator is already spoken for and a second one would be a second
   * character a name must not hold.
   */
  private def hookFields(hooks: THooks, kind: HookKind): List[String] =
    hooks.all.find(_.kind == kind) match
      case Some(h) => List(h.func, h.file, h.line.toString)
      case None    => List("", "", "")

  def decode(text: String): Option[List[TTest]] =
    val lines = text.linesIterator.filter(_.nonEmpty).toList
    val marks = 6 + HookKind.values.length * 3

    Option.when(lines.forall(_.count(_ == '\u0000') == marks))(
      lines.map { line =>
        // `-1` so that a trailing run of empty fields survives, which is every test in a module that
        // declared no hooks at all. Without it the fields simply are not there and the read fails,
        // which reads as a corrupt sidecar and is an ordinary suite.
        val f = line.split("\u0000", -1)
        val hooks = HookKind.values.toList.zipWithIndex.flatMap { (k, i) =>
          val at = 7 + i * 3

          Option.when(f(at).nonEmpty)(THook(k, f(at), f(at + 1), f(at + 2).toInt))
        }

        TTest(f(0), f(1), f(2) == "true", Option.when(f(4) == "1")(f(3)), f(5), f(6).toInt,
              THooks.of(hooks))
      },
    )

  /** The key itself: everything above, mixed in a fixed order.
   *
   * The order is fixed rather than sorted because a key is not compared with anything but itself —
   * what it has to be is *the same* for the same inputs, and the sorting that matters is inside
   * `LibraryArtifact.fingerprint`, which is over a set of files that may arrive in any order.
   */
  def key(parts: List[String]): String = LibraryArtifact.fingerprint(
    parts.zipWithIndex.map((p, i) => Source(i.toString, p)),
  )
}
