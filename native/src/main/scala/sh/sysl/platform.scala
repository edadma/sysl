package sh.sysl

import scala.scalanative.meta.LinktimeInfo
import scala.scalanative.posix.stdlib.{getenv, realpath, setenv}
import scala.scalanative.posix.unistd.execv
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

val platform = "native"

/** How long a path may be. `PATH_MAX` is 1024 on macOS and 4096 on Linux, and `realpath` writes up
 * to that many bytes into whatever it is given — so the larger of the two is the only safe size for
 * a buffer compiled once and run on either.
 */
private inline val PathMax = 4096

/** macOS answers this and nothing else does: there is no `/proc`, and the path a process was
 * launched from is something only the loader knows.
 *
 * Guarded by `LinktimeInfo.isMac`, which is resolved when the compiler is *linked* rather than when
 * it runs — so on a Linux build this declaration is unreachable and the symbol is never asked of the
 * linker. A runtime `if` would not do: the symbol would be undefined and the link would fail.
 */
@extern
private object dyld {
  def _NSGetExecutablePath(buf: CString, size: Ptr[CUnsignedInt]): CInt = extern
}

/** The machine this compiler is running on, as Scala Native reports it: `(processor, system)`, in
 * whatever words this platform uses for them. Turning the pair into a target is `Target.hostName`'s
 * job, and is shared so that the three platforms cannot disagree about what a machine is.
 *
 * A native build is compiled *for* the machine it runs on, so both halves are fixed when the
 * compiler itself is linked and are read off the triple it was built with.
 *
 * Observed on this machine: `("aarch64", "darwin")` — the processor is **not** spelled the way the
 * triple spells it (`arm64-apple-...`), and not the way Node spells it either, which is the whole
 * reason the three platforms hand over words rather than a target.
 */
def hostMachine: (String, String) = (LinktimeInfo.target.arch, LinktimeInfo.target.os)

/** Where this compiler's own binary is, with every symlink on the way resolved — which is how an
 * installed sysl finds the library it was installed beside (`Std.root`).
 *
 * **Resolving matters, and it is the whole reason this is not `argv[0]`.** Homebrew installs a
 * binary into `/opt/homebrew/Cellar/sysl/<version>/bin` and links `/opt/homebrew/bin/sysl` at it, so
 * the path a user's shell found is a symlink in a directory that holds no library. Following it
 * lands on the real prefix, which is where the files actually are.
 *
 * The native build is the one that matters here — it is the binary that ships — and it is the only
 * one of the three that can answer exactly. macOS asks the loader through `_NSGetExecutablePath`,
 * which Apple's own documentation says to pass through `realpath`; Linux reads the symlink the
 * kernel maintains at `/proc/self/exe`, which `realpath` resolves in one step. Anything else answers
 * `None` and the search falls through to the working directory, which is what a development tree
 * wants anyway.
 */
def executablePath: Option[String] = {
  val resolved = stackalloc[Byte](PathMax)

  val raw: CString =
    if LinktimeInfo.isMac then {
      val buf  = stackalloc[Byte](PathMax)
      val size = stackalloc[CUnsignedInt]()

      !size = PathMax.toUInt

      // A nonzero answer means the buffer was too small, and `size` comes back holding what would
      // have been needed. `PATH_MAX` is that size, so there is nothing to retry with.
      if dyld._NSGetExecutablePath(buf, size) == 0 then buf else null
    } else if LinktimeInfo.isLinux then c"/proc/self/exe"
    else null

  if raw == null || realpath(raw, resolved) == null then None else Some(fromCString(resolved))
}

/** The ceiling this compiler puts on its own GC when the caller has not chosen one — see
 * `ensureHeapCeiling`, the only place this is read.
 *
 * Picked by measurement (2026-09-07) against `slate`, the largest sysl program there is (95,757
 * lines across 197 files): capped runs at 2g, 4g, 8g, 16g and 32g all either crashed with
 * `Out of heap space` or grew to the cap and stalled without finishing; 36g was the smallest that
 * completed, in 92 s with a measured peak of 36.9 GB (40g and 48g completed at the same peak,
 * confirming 36g as the floor rather than a fluke). Twice the smallest completing cap is 72 GB,
 * rounded up to the nearest power of two. `~/dev/sysl-lang/sysl/CLAUDE.md`'s heap-ceiling sections
 * have the rest of the method.
 */
private[sysl] inline val DefaultMaxHeapSize = "128g"

/** What this compiler would set `GC_MAXIMUM_HEAP_SIZE` to, given what it already reads there — or
 * `None` where the caller has already chosen, which `ensureHeapCeiling` takes as "leave it alone."
 *
 * Held apart from `ensureHeapCeiling` so the decision is a value a test can ask for without paying
 * for a real `setenv` and `execv`: this is the whole of the policy, and everything below it is
 * mechanism.
 */
private[sysl] def heapCeilingDecision(current: Option[String]): Option[String] =
  Option.when(current.isEmpty)(DefaultMaxHeapSize)

/** Give the Immix collector a ceiling before it can grow past one, unless the caller already gave
 * it one of their own.
 *
 * Scala Native's Immix GC reads `GC_MAXIMUM_HEAP_SIZE` from the environment at GC init — before
 * `main` runs — and defaults to **unlimited** when it is absent (`Settings_MaxHeapSize` in the
 * runtime's own `gc/immix/Settings.c`, answering `UNLIMITED_HEAP_SIZE` from `Constants.h`). So an
 * installed compiler run on a large program grows until it meets physical memory rather than
 * collecting, which is what a hosted runner's `Out of heap space grow heap` is. There is no
 * link-time hook for this default: the setting is read directly from the environment by C code
 * this project does not own, and Scala Native's `NativeConfig` has nothing that reaches it.
 *
 * So the ceiling is set here, in the compiler's own entry point, before anything of size has been
 * allocated: `setenv` the default, then **re-exec this same binary with the same arguments**.
 * `execv` replaces the process image outright rather than forking a child, so stdin, stdout,
 * stderr and the eventual exit status all carry through unchanged — there is no parent left
 * to relay them. The child inherits the environment `setenv` just mutated, so its own GC sees
 * `GC_MAXIMUM_HEAP_SIZE` already set and `heapCeilingDecision` is what stops a second re-exec:
 * the guard and the fix are the same environment variable.
 *
 * A build that cannot resolve its own path — `executablePath` answers `None` off this platform's
 * mac/Linux pair, which is every target this compiler ships for — falls back to a PATH lookup of
 * its own command name, and running under neither leaves the process to start uncapped rather
 * than fail outright: growing without bound is the defect this exists to fix, not one worth
 * trading for a compiler that will not start at all.
 */
def ensureHeapCeiling(programArgs: Seq[String]): Unit =
  heapCeilingDecision(Option(getenv(c"GC_MAXIMUM_HEAP_SIZE")).map(p => fromCString(p))).foreach { ceiling =>
    Zone.acquire { implicit z =>
      setenv(c"GC_MAXIMUM_HEAP_SIZE", toCString(ceiling), 1)

      executablePath.orElse(findOnPath("sysl")).foreach { path =>
        val cArgs = (path +: programArgs).map(toCString)
        val argv  = stackalloc[CString](cArgs.length + 1)

        for i <- cArgs.indices do argv(i) = cArgs(i)
        argv(cArgs.length) = null

        execv(toCString(path), argv)
        // execv only returns on failure — fall through and run uncapped.
      }
    }
  }

/** Where `name` sits on the PATH, if it is there and can be run.
 *
 * This is what makes an external subcommand possible: `sysl doc` looks for `sysl-doc` and hands it
 * the rest of the line. `ProcessBuilder` would search the PATH itself, so the reason to ask
 * separately is the **diagnostic** — a command that is not there and a command that ran and failed
 * are different things to say to somebody, and going through the process API answers both with one
 * `IOException`.
 *
 * Executability is part of the question rather than an extra check. A directory named like the
 * command, or a file somebody forgot to `chmod +x`, is not a command — and finding one would stop
 * the search at something that cannot run while a real one sat further down the PATH.
 *
 * The `.exe` spelling is tried second so that Windows works without a separate code path and no
 * other platform pays for it: a file called `sysl-doc.exe` is not on anybody's Unix PATH.
 *
 * Written against `java.io` rather than `unistd` so that it is the same source as the JVM's, which
 * is the convention `runProgram` below already follows for the same reason: two implementations of
 * one question are two things to keep in step.
 */
def findOnPath(name: String): Option[String] =
  Option(System.getenv("PATH")).toList
    .flatMap(_.split(java.io.File.pathSeparatorChar).toList)
    .filter(_.nonEmpty)
    .flatMap(dir => List(new java.io.File(dir, name), new java.io.File(dir, s"$name.exe")))
    .find(f => f.isFile && f.canExecute)
    .map(_.getAbsolutePath)

/** A built program run as the driver's own foreground work — `Main`'s `run` command states the
 * contract this answers to.
 *
 * The input is **copied** from `Console.in` rather than the child being handed this process's own
 * descriptor, and two things follow that are both wanted: a caller that redirected its input has
 * redirected the program's, which is what makes any of this observable from a test; and the copy is
 * dynamically scoped, so it cannot be perturbed by another suite running beside this one the way a
 * `System.setIn` would be.
 */
def runProgram(command: Seq[String]): Int =
  try {
    val proc = new ProcessBuilder(command*).start()
    val feed = new Thread(() => feedInput(proc.getOutputStream))
    val errs = new Thread(() => pumpOutput(proc.getErrorStream, Console.err))

    // The reader is left where it is if the program never asked for what it was offered: it is
    // blocked on an input nobody is going to write to, and a daemon thread is not one to wait for.
    feed.setDaemon(true)
    feed.start()
    errs.start()
    pumpOutput(proc.getInputStream, Console.out)
    errs.join()
    proc.waitFor()
  } catch {
    case e: java.io.IOException => Console.err.println(e.getMessage); -1
  }

private def feedInput(to: java.io.OutputStream): Unit = {
  val chunk = new Array[Char](4096)

  try {
    var n = Console.in.read(chunk)

    while n != -1 do {
      to.write(new String(chunk, 0, n).getBytes("UTF-8"))
      to.flush()
      n = Console.in.read(chunk)
    }
  } catch {
    // The program stopped reading and closed its end, which is an ordinary thing for a program to
    // do — `head` does it — and is not something to report.
    case _: java.io.IOException => ()
  } finally
    try to.close()
    catch { case _: java.io.IOException => () }
}

/** Whatever the program has written so far, forwarded and **flushed**, for as long as it writes.
 *
 * Reading it all and printing the lot at the end is what `exec` does, and it is why the driver's own
 * messages arrived before a program's however the program had ordered them.
 */
private def pumpOutput(from: java.io.InputStream, to: java.io.PrintStream): Unit = {
  val chunk = new Array[Byte](8192)
  var n     = from.read(chunk)

  while n != -1 do {
    to.print(new String(chunk, 0, n, "UTF-8"))
    to.flush()
    n = from.read(chunk)
  }
}
