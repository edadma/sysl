package sh.sysl

val platform = "jvm"

/** The machine this compiler is running on, as the JVM reports it: `(processor, system)`, in
 * whatever words this platform uses for them. Turning the pair into a target is `Target.hostName`'s
 * job, and is shared so that the three platforms cannot disagree about what a machine is.
 *
 * Observed on this machine: `("aarch64", "Mac OS X")`.
 */
def hostMachine: (String, String) =
  (System.getProperty("os.arch", ""), System.getProperty("os.name", ""))

/** Where this compiler's own binary is — **`None` here, and honestly so.**
 *
 * The executable a JVM run has is `java`, sitting in whatever JDK the machine happens to use, and
 * the library is nowhere near it. Answering with the JDK's path would be worse than answering with
 * nothing: `Std.root` would search a directory that cannot hold the library and report it as one of
 * the places it looked.
 *
 * Nothing is lost by it. The JVM build is the development one — `sbt syslJVM/test`, and the driver
 * run from a checkout — where the working directory is inside the repository and the search finds
 * `library/` there. It is the *native* build that gets installed, and that one answers exactly.
 */
def executablePath: Option[String] = None

/** No-op here — see the native implementation, the only one that does anything.
 *
 * The JVM's own heap is capped by `.jvmopts`' `-Xmx8g`, unrelated to Scala Native's Immix GC and
 * already in force before this or any other Scala code runs, so there is no ceiling for this
 * platform to give itself.
 */
def ensureHeapCeiling(programArgs: Seq[String]): Unit = ()

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
