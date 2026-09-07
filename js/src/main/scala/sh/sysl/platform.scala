package sh.sysl

import scala.scalajs.js

val platform = "js"

/** The machine this compiler is running on, as Node reports it: `(processor, system)`, in whatever
 * words this platform uses for them. Turning the pair into a target is `Target.hostName`'s job, and
 * is shared so that the three platforms cannot disagree about what a machine is.
 *
 * A build with no `process` at all — a browser — has no machine to report, so it answers a pair of
 * empty strings and every invocation there has to name its target.
 *
 * Observed under Node 24 on this machine: `("arm64", "darwin")`.
 */
def hostMachine: (String, String) = {
  val process = js.Dynamic.global.selectDynamic("process")

  if js.isUndefined(process) then ("", "")
  else (process.arch.asInstanceOf[String], process.platform.asInstanceOf[String])
}

/** Where this compiler's own script is, as Node reports it — `argv[1]`, the file Node was told to
 * run, which for a compiler invoked as a command is the compiler.
 *
 * `argv[0]` is `node` itself and is no more use here than the JVM's `java` is. A browser has no
 * `process` and no script path, so it answers nothing and every invocation there falls through to
 * the working directory.
 *
 * **Unresolved, unlike the native answer.** Node's own `fs.realpathSync` is reachable only through a
 * module import, and the JS build is not one that gets installed beside a library — so what this
 * buys is the same shape as the other two rather than a path anybody depends on.
 */
def executablePath: Option[String] = {
  val process = js.Dynamic.global.selectDynamic("process")

  if js.isUndefined(process) then None
  else {
    val argv = process.argv.asInstanceOf[js.Array[String]]

    Option.when(argv.length > 1)(argv(1))
  }
}

/** Where `name` sits on the PATH, if it is there and can be run — **`None` here, deliberately.**
 *
 * This answers the external-subcommand question: `sysl doc` looks for `sysl-doc` and hands it the
 * rest of the line. The other two platforms search the PATH with `java.io`, which Scala.js does not
 * have; Node could be asked through `fs`, and the reason not to is the one this file already gives
 * for `runProgram` — **the JS build is not the one that runs programs.** It is the browser and
 * interpreter build, where there is no PATH to search and no process to start.
 *
 * Answering `None` is therefore honest rather than a stub: on this platform there is no external
 * subcommand to find, and `Main` says so in the same words it uses when a binary is genuinely
 * missing — which is the truth here.
 */
def findOnPath(name: String): Option[String] = None

/** No-op here — see the native implementation, the only one that does anything.
 *
 * Node's own garbage collector is V8's, not Scala Native's Immix, and is unaffected by
 * `GC_MAXIMUM_HEAP_SIZE`, so there is no ceiling for this platform to give itself.
 */
def ensureHeapCeiling(programArgs: Seq[String]): Unit = ()

/** A built program run as the driver's own foreground work — `Main`'s `run` command states the
 * contract this answers to.
 *
 * Node has the whole of it in one word: `stdio = "inherit"` hands the child this process's own three
 * descriptors, so its input is whatever Node's is and what it writes goes straight out. The other two
 * platforms copy instead, which buys a seam a test can drive; nothing here needs one, because the JS
 * build is not the one that runs programs.
 */
def runProgram(command: Seq[String]): Int = {
  val childProcess = js.Dynamic.global.require("child_process")
  val result       = childProcess.spawnSync(command.head, js.Array(command.tail*),
    js.Dynamic.literal(stdio = "inherit"))
  val status       = result.status

  if status == null || js.isUndefined(status) then -1 else status.asInstanceOf[Int]
}
