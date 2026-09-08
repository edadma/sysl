package sh.sysl

import io.github.edadma.cross_platform.*

import scopt.OParser

// The command line: what a `Config` holds, the parser that fills one in, and the three
// subcommands whose whole answer is text on stdout. Held apart from the driver because it is
// the half a reader consults to find out what sysl *accepts* rather than what it does.

/** The sysl command-line driver. It reads a module's source files, runs the pure front end and
 * codegen from the shared module, and drives an LLVM toolchain to link and run the result. Filesystem
 * and process access go through `cross_platform`, so the same driver ships as a native binary
 * and as a Node CLI (the JVM build is for a fast development loop).
 *
 * Each subcommand takes a **path**, which is a **project root** or a single file. A module is a
 * directory and its name is that directory's path relative to the root (`reference/modules.md`), so
 * naming a directory compiles the whole tree under it — one module per directory, each holding its
 * files to the name its location gives it — and naming a file compiles that file alone.
 *
 * Subcommands:
 *   - `sysl run <path>`            compile and execute
 *   - `sysl build <path> -o x`     compile to a native executable
 *   - `sysl build-lib <path> -o x` compile a library to a linkable artifact
 *   - `sysl emit-llvm <path>`      print the generated LLVM IR
 *   - `sysl weave <path>`          render a literate source as an HTML document
 *   - `sysl tangle <path>`         print the program a literate source holds
 *   - `sysl deps <path>`           print the resolved dependency graph
 *   - `sysl add <coordinate>`      add a dependency to this project's manifest
 *   - `sysl vendor <path>`         put what this project depends on into vendor/
 *   - `sysl targets`               list the machines sysl can build for
 *
 * **`--lib` takes either a source tree or an artifact**, and which one is read off the name: a
 * `.syslib` is decoded, anything else is walked as source. That is deliberate — how a library was
 * shipped is the shipper's business, and a program that depends on one should not have to write down
 * which it got. `build-lib` is what turns the first into the second, and the only difference
 * downstream is what the compilation *cost*: an artifact is a linear decode where source is a parse.
 *
 * **An artifact is an `ar` archive** (`LibraryArtifact`), so it reaches the linker as it stands and
 * only the members that resolve something are pulled in. Building one therefore needs an `llvm-ar`
 * as well as a `clang`; `--ar` names it where it is somewhere a search would not look.
 *
 * **The standard module's own source is read off disk**, from the library installed with this
 * compiler — `<prefix>/share/sysl/library` beside the binary, or `library/` in a checkout (`Std.root`).
 * There is no copy inside the executable: a library nobody can open is not one anybody can learn
 * from or edit, which is what every other toolchain concluded too.
 *
 * **`--std-lib` is the same thing for the standard module** as `--lib` is for any other, and every
 * program is compiled against it whether or not one is named. Built by `build-lib --std` and given
 * back here, it replaces the parse of that source: the signatures arrive decoded, and the half that
 * was already compiled is linked rather than emitted a second time into every program.
 *
 * **It need not be given, and it need not already exist.** `build-lib --std` with no `-o` writes to
 * `LibraryArtifact.stdDefault`, and a compilation with no `--std-lib` looks there — one path at both
 * ends. Where nothing usable is at that path the compiler **builds one**, from the library source,
 * and says so on stderr. The artifact is derived rather than authored: not committed, object code
 * for one machine, and computed entirely from the source beside the compiler, so being absent after
 * a clone or stale after a format change has one answer and it is not a question for whoever ran the
 * command. It sits in the user's cache under a fingerprint of the library it was built from, so
 * every project on a machine shares one and a compiler installed with a different library gets a
 * path of its own rather than a stale hit.
 *
 * **Which is not the same as substituting a library.** What a compiler must never do is answer *I
 * could not find the library you meant* by quietly using a different one — and a rebuild uses **this**
 * one, held to `Std.fingerprint` on the way back in. A `--std-lib` that was named and cannot be read
 * still stops the compilation, because there the reader asked for a particular artifact and is owed
 * the truth about it.
 *
 * **`--no-std-lib` is the one route to the library as source**, ignoring whatever artifact is on
 * disk. Compiling it rather than linking it is what makes bootstrap possible — there is no released
 * sysl to build the first artifact with — so it is reached deliberately rather than by a lookup
 * coming up empty. Taken silently it would be taken always, because then nobody would have any
 * reason to build an artifact at all.
 *
 * **`build-c` and `emit-header` are the exception and take the source unasked**, because what they
 * write is read by a C linker and a `.syslib` is not something one can be given. That is a property
 * of the consumer rather than a preference, which is why it is not left to a flag.
 *
 * **Everything after a bare `--` belongs to the program being run**, not to sysl: it is passed
 * straight through to the executable, which is what lets `sysl run prog.sysl -- -v file` reach a
 * `main(args: []string)` without sysl having to decide whether `-v` was meant for it. The split is
 * made before the options are parsed, which is why an argument that looks like one of sysl's own is
 * still the program's.
 *
 * `--explain-escapes` may be given to any of them: it reports, on stderr, every local array the
 * compiler moved to the heap and the view that forced it (`05`).
 *
 * `--target` names the machine to build for (`getting-started/cli.md § targets`). Given none, a
 * build is for the machine it is running on — and if that is one sysl has no entry for, it says so
 * and stops rather than guessing, because a wrong guess produces a module that looks right and is
 * not.
 *
 * `--optimize` names the level handed to clang, spelled as clang spells one after the `-O`, and it
 * reaches every object a build produces rather than only the link. The default is `1` rather than
 * nothing at all, which is what it used to be: `-O0` is a different instruction selector, it is the
 * mode a back end's own suite covers least, and a miscompile was found living there
 * (`Toolchain.defaultOptimization` has the case). A level clang does not have is clang's to report.
 */
case class Config(
    command: String = "",
    file: String = "",
    /** `sysl add`'s coordinate, as it was typed — `github.com/owner/repo`, or that with `@version`.
      *
      * Kept as the whole string rather than split here, because what a malformed one is worth is a
      * sentence explaining the shape, and argument parsing is not where that reads well.
      */
    spec: String = "",
    output: Option[String] = None,
    explainEscapes: Boolean = false,
    verbose: Boolean = false,
    target: Option[String] = None,
    libs: List[String] = Nil,
    std: Boolean = false,
    stdLib: Option[String] = None,
    noStdLib: Boolean = false,
    /** Where to look for a prebuilt standard module, when somewhere other than the default.
      *
      * **An `Option` so that the default is worked out when it is wanted rather than when a `Config`
      * is built.** The default path holds a fingerprint of the library, so naming it here would have
      * read the library's source during argument parsing — before the driver had a chance to report
      * not finding it, and from a place where the failure could only be an exception. A compiler
      * that cannot find its library has to say so on stderr like anything else (`Std.root`).
      */
    stdSearch: Option[String] = None,
    ar: Option[String] = None,
    /** `--cc` — the clang to build with, where a search would not find the right one.
      *
      * The companion of `--ar`, and it exists because three `Toolchain` diagnostics told the reader
      * to name one for months while nothing parsed the flag (card `0197`). It reaches every place
      * the compiler runs clang, the standard module's own rebuild included — a flag honoured in
      * `build` and dropped when the library is rebuilt underneath it would fail later than the flag
      * and blame the library.
      */
    cc: Option[String] = None,
    /** `--link-path` and `--include-path` — where on this machine to look for a library a `@link`
      * named, and for a header a carried `.c` includes or an `@include` names (`SearchPaths`). Lists
      * rather than single values because a build that needs one prefix usually needs the two it came
      * with, and the order given is the order searched.
      */
    linkPaths: List[String] = Nil,
    includePaths: List[String] = Nil,
    /** `--include-path <name>=<dir>` — the same flag naming which of a package's declared header
      * requirements the directory answers (`reference/packages.md § Capabilities`).
      *
      * Kept beside `includePaths` rather than instead of it: the directory goes to the C compiler
      * either way, and what the name adds is that a package which asked for it and got nothing is
      * refused by name instead of by clang.
      */
    namedIncludes: Map[String, String] = Map.empty,
    defines: List[String] = Nil,
    programArgs: List[String] = Nil,
    filter: Option[String] = None,
    failFast: Boolean = false,
    optimize: String = Toolchain.defaultOptimization,
    /** `build-c --header` — where the generated C header goes, when somewhere other than beside the
      * archive (`reference/ffi.md § @export`).
      */
    header: Option[String] = None,
    /** `prove --emit-whyml` — print the translation instead of running the prover (`reference/verification.md § sysl prove`). */
    emitWhyML: Boolean = false,
    /** `emit-ast --no-spans` — omit every node's source span, so a diff does not move when a line
      * does (`AstPrinter`).
      */
    noSpans: Boolean = false,
    /** `prove --overflow` — whether staying in an integer's range is a proof obligation. */
    overflow: String = "check",
) {

  /** The same config with header requirements an environment variable answered folded in, exactly as
   * `--include-path <name>=<dir>` would have.
   *
   * **Both fields, because they do different jobs**: `namedIncludes` is what a requirement is
   * checked against, and `includePaths` is what reaches the C compiler. Adding to only the first
   * gives a build that passes its own check and then fails inside clang.
   */
  def withNamedIncludes(found: List[(String, String)]): Config =
    if found.isEmpty then this
    else
      copy(includePaths = includePaths ::: found.map(_._2),
           namedIncludes = namedIncludes ++ found)
}

/** The option grammar, held apart from the entry point so that a test can ask what an argument list
 * parses to. The alternative is a suite that builds a `Config` by hand and so never finds out
 * whether the flag it is about is spelled the way the user has to spell it.
 */
private[sysl] val parser = {
  val builder = OParser.builder[Config]

  {
    import builder.*
    OParser.sequence(
      programName("sysl"),
      cmd("run")
        .action((_, c) => c.copy(command = "run"))
        .text("compile and run a sysl module, given its directory or a single file; " +
          "arguments after '--' go to the program")
        .children(arg[String]("<path>").required().action((f, c) => c.copy(file = f))),
      cmd("build")
        .action((_, c) => c.copy(command = "build"))
        .text("compile a sysl module to a native executable")
        .children(
          arg[String]("<path>").required().action((f, c) => c.copy(file = f)),
          opt[String]('o', "output").action((o, c) => c.copy(output = Some(o))).text("output executable path"),
        ),
      cmd("build-lib")
        .action((_, c) => c.copy(command = "build-lib"))
        .text("compile a sysl library to a linkable artifact, for '--lib'")
        .children(
          arg[String]("<path>").required().action((f, c) => c.copy(file = f)),
          opt[String]('o', "output").action((o, c) => c.copy(output = Some(o))).text("output artifact path"),
          opt[Unit]("std")
            .action((_, c) => c.copy(std = true))
            .text("this library is sysl's own standard module, which the compiler otherwise supplies"),
        ),
      cmd("build-c")
        .action((_, c) => c.copy(command = "build-c"))
        .text("compile a sysl module to a static archive and a C header, for an existing C " +
          "project to link against")
        .children(
          arg[String]("<path>").required().action((f, c) => c.copy(file = f)),
          opt[String]('o', "output").action((o, c) => c.copy(output = Some(o))).text("output archive path"),
          opt[String]("header")
            .action((h, c) => c.copy(header = Some(h)))
            .text("where to write the C header; defaults to the archive's path with a '.h' suffix"),
        ),
      cmd("emit-header")
        .action((_, c) => c.copy(command = "emit-header"))
        .text("print the C header for what a module exports")
        .children(arg[String]("<path>").required().action((f, c) => c.copy(file = f))),
      cmd("weave")
        .action((_, c) => c.copy(command = "weave"))
        .text("render a literate source as an HTML document, with its prose set, its program " +
          "highlighted and its mathematics typeset")
        .children(
          arg[String]("<path>").required().action((f, c) => c.copy(file = f)),
          opt[String]('o', "output").action((o, c) => c.copy(output = Some(o)))
            .text("where to write the document; a directory when the path holds several sources, " +
              "and standard output by default"),
        ),
      cmd("tangle")
        .action((_, c) => c.copy(command = "tangle"))
        .text("print the program a literate source holds, with the prose stripped")
        .children(
          arg[String]("<path>").required().action((f, c) => c.copy(file = f)),
          opt[String]('o', "output").action((o, c) => c.copy(output = Some(o)))
            .text("where to write the program; defaults to standard output"),
        ),
      cmd("test")
        .action((_, c) => c.copy(command = "test"))
        .text("run the '@test' functions of a sysl module")
        .children(
          arg[String]("<path>").required().action((f, c) => c.copy(file = f)),
          opt[String]("filter")
            .action((f, c) => c.copy(filter = Some(f)))
            .text("run only the tests whose name or module holds this text"),
          opt[Unit]("fail-fast")
            .action((_, c) => c.copy(failFast = true))
            .text("stop at the first test that fails"),
          opt[Unit]("std")
            .action((_, c) => c.copy(std = true))
            .text("this tree is sysl's own standard module, which the compiler otherwise supplies"),
        ),
      cmd("emit-llvm")
        .action((_, c) => c.copy(command = "emit-llvm"))
        .text("print the generated LLVM IR")
        .children(arg[String]("<path>").required().action((f, c) => c.copy(file = f))),
      cmd("emit-ast")
        .action((_, c) => c.copy(command = "emit-ast"))
        .text("print one file's untyped parse tree, as deterministic text — parse only, with no " +
          "analysis and no standard module, so it works on a file that would fail to compile")
        .children(
          arg[String]("<path>").required().action((f, c) => c.copy(file = f)),
          opt[Unit]("no-spans")
            .action((_, c) => c.copy(noSpans = true))
            .text("omit every node's source span, for a diff that does not move when a line does"),
        ),
      cmd("prove")
        .action((_, c) => c.copy(command = "prove"))
        .text("translate a module to WhyML and discharge its proof obligations with Why3 (17)")
        .children(
          arg[String]("<path>").required().action((f, c) => c.copy(file = f)),
          opt[Unit]("emit-whyml")
            .action((_, c) => c.copy(emitWhyML = true))
            .text("print the WhyML instead of proving it"),
          opt[String]("overflow")
            .action((o, c) => c.copy(overflow = o))
            .text("'check' (the default) makes staying in an integer's range a proof obligation; " +
              "'ignore' drops those obligations, for reasoning about the rest of a function first"),
        ),
      cmd("deps")
        .action((_, c) => c.copy(command = "deps"))
        .text("print the dependency graph this project resolves to, and who asked for each version")
        .children(arg[String]("<path>").required().action((f, c) => c.copy(file = f))),
      cmd("add")
        .action((_, c) => c.copy(command = "add"))
        .text("add a dependency to this project's package.hocon, at its newest version or a pinned one")
        .children(
          arg[String]("<coordinate>").required().action((s, c) => c.copy(spec = s))
            .text("'github.com/sysl-lang/sdl3', or 'github.com/sysl-lang/sdl3@0.3.1'"),
          arg[String]("<path>").optional().action((f, c) => c.copy(file = f))
            .text("the project to add it to; the working directory by default"),
        ),
      cmd("vendor")
        .action((_, c) => c.copy(command = "vendor"))
        .text("put every package this project depends on into vendor/, so that a build fetches nothing")
        .children(arg[String]("<path>").required().action((f, c) => c.copy(file = f))),
      cmd("targets")
        .action((_, c) => c.copy(command = "targets"))
        .text("list the machines sysl can build for"),
      // A heading, because everything below belongs to no command and the usage would otherwise
      // print it flush against the last one — where an option renders exactly as that command's own
      // children do, so `--target` read as an option of `targets`.
      note("\nOptions, which any command takes:"),
      // Flags rather than subcommands, because they are what somebody types before they know there
      // are subcommands — and each satisfies `checkConfig` below by naming a command of its own, so
      // `sysl --version` and `sysl --help` stand alone rather than being options to something else.
      opt[Unit]("version")
        .action((_, c) => c.copy(command = "version"))
        .text("print which build of sysl this is"),
      // Not scopt's own `help("help")`, though it exists and would render the same text. That one is
      // a *terminating* option: it reaches `OEffect.Terminate`, which the default setup answers with
      // `sys.exit`, so a test that asked what `--help` does would take the test runner down with it.
      // Naming a command instead keeps it on the same footing as every other one — driven through
      // `execute`, answerable in a test, and printing the usage that `OParser` generates anyway.
      opt[Unit]("help")
        .action((_, c) => c.copy(command = "help"))
        .text("print this usage text"),
      opt[Unit]('v', "verbose")
        .action((_, c) => c.copy(verbose = true))
        .text("report what the build decided: which standard module was used and why, the files " +
          "read, the search paths, and the clang and linker command lines"),
      opt[Unit]("explain-escapes")
        .action((_, c) => c.copy(explainEscapes = true))
        .text("report every local array promoted to the heap, and the view that forced it"),
      opt[String]("target")
        .action((t, c) => c.copy(target = Some(t)))
        .text("the machine to build for; defaults to this one. 'sysl targets' lists them"),
      opt[String]("lib")
        .unbounded()
        .action((l, c) => c.copy(libs = c.libs :+ l))
        .text("a library to compile against — a '.syslib' artifact or a source root; " +
          "may be given more than once"),
      opt[String]("std-lib")
        .action((l, c) => c.copy(stdLib = Some(l)))
        .text("a prebuilt standard module to compile against, from 'build-lib --std'; " +
          "one that cannot be read stops the compilation, being the one that was asked for"),
      opt[Unit]("no-std-lib")
        .action((_, c) => c.copy(noStdLib = true))
        .text("compile the standard module from its source rather than linking a prebuilt one, " +
          "ignoring whatever artifact is on disk"),
      opt[String]("ar")
        .action((a, c) => c.copy(ar = Some(a)))
        .text("the llvm-ar to build a library with; defaults to searching for one"),
      opt[String]("cc")
        .action((a, c) => c.copy(cc = Some(a)))
        .text("the clang to build with; defaults to searching for one"),
      opt[String]("link-path")
        .unbounded()
        .action((d, c) => c.copy(linkPaths = c.linkPaths :+ d))
        .text("a directory to look in for a library a 'link' directive named — for one a package " +
          "manager installed outside the toolchain's own prefix; may be given more than once"),
      opt[String]("include-path")
        .unbounded()
        .action { (d, c) =>
          SearchPaths.namedInclude(d) match
            case None => c.copy(includePaths = c.includePaths :+ d)
            case Some((name, dir)) =>
              c.copy(includePaths = c.includePaths :+ dir, namedIncludes = c.namedIncludes + (name -> dir))
        }
        .text("a directory to look in for a header the C beside a module includes, or one an " +
          "'@include' names for a 'c const' block; the other half of --link-path, and needed by the " +
          "same bindings; may be given more than once. Written '<name>=<dir>' it also answers the " +
          "header requirement a package declared under that name"),
      opt[String]('D', "define")
        .unbounded()
        .action((d, c) => c.copy(defines = c.defines :+ d))
        .text("a macro the C beside a module is compiled with, and a 'c const' block's headers are " +
          "read under, as 'NAME' or 'NAME=value' — what a host C project configures its own headers " +
          "with, and which finding the header does not supply; may be given more than once"),
      opt[String]('O', "optimize")
        .action((o, c) => c.copy(optimize = o))
        .text(s"the optimization level to hand clang, as it spells one after the '-O': " +
          s"defaults to ${Toolchain.defaultOptimization}, and '0' is the mode a miscompile was " +
          s"once found in. '-O2' is written the way clang writes it"),
      checkConfig(c => if c.command.isEmpty then failure("a subcommand is required") else success),
    )
  }
}

/** `-O2` split into `-O` and `2`, which is the one place sysl's spelling of an option and the
 * parser's disagree.
 *
 * A short option takes its value as the next argument, and clang's optimization flag is written
 * **joined** — `-O2`, `-Os` — because that is how it has been written since cc. A short form that
 * only worked detached would look like clang's flag without being it, which is a worse thing to
 * offer than no short form at all, so the argument is rewritten rather than the spelling given up.
 *
 * Only that one letter, and only where something follows it: a bare `-O` still takes the next
 * argument, and `--optimize` is untouched because it does not begin `-O`. Nothing here can reach a
 * program's own arguments, which the caller has already split off at the `--`.
 */
private def splitJoinedLevel(args: Seq[String]): Seq[String] =
  args.flatMap(a => if a.startsWith("-O") && a.length > 2 then Seq("-O", a.drop(2)) else Seq(a))

/** sysl's own arguments, parsed. Held apart from the entry point so that a test asks the question a
 * user's shell asks, rather than building a `Config` by hand and never finding out whether a flag is
 * spelled the way it has to be typed.
 */
private[sysl] def parseArgs(own: Seq[String]): Option[Config] =
  OParser.parse(parser, splitJoinedLevel(own), Config())

/** The subcommands sysl itself implements, **read off the parser rather than written down twice.**
 *
 * `Main` needs this to tell a subcommand it does not have from one it does: an unknown word is
 * somebody else's `sysl-<word>` and gets exec'd, and a known one must never be, or installing a
 * binary called `sysl-build` would quietly take over the compiler's own command.
 *
 * **A second list would drift**, and it would drift in the direction that breaks a built-in: a
 * command added to the parser and forgotten here stops being recognized and starts being looked for
 * on the PATH. So the names come from `OParser.usage`, which renders one `Command: <name> …` line
 * per command and is the only enumeration scopt exposes — its `OptionDefKind` is `private[scopt]`,
 * so the structure cannot be walked directly.
 *
 * **Reading the usage text is indirect and it fails loudly**, which is what makes it safe: if scopt
 * ever changes that rendering this answers with nothing, every built-in is looked for on the PATH,
 * and `SubcommandTests` says so on the first run rather than in somebody's shell a month later.
 */
private[sysl] def builtinCommands: Set[String] =
  OParser
    .usage(parser)
    .linesIterator
    .collect { case line if line.startsWith("Command: ") => line.drop("Command: ".length).takeWhile(!_.isWhitespace) }
    .toSet

/** Which build of sysl this is.
 *
 * On stdout rather than stderr, and alone on its line, because the first thing anyone does with a
 * version is read it out of a script — and a bug report that quotes it is the reason it exists at
 * all. The number comes from the build (`BuildInfo`), so a binary cannot claim a version it was not
 * cut at.
 */
private def printVersion(): Int = {
  stdout(s"sysl ${BuildInfo.version}\n")
  0
}

/** The usage text, for someone who asked for it.
 *
 * It was already reachable — an invocation naming no subcommand prints it, because `checkConfig`
 * refuses one — but only by *failing*, on stderr and with a non-zero status. `--help` is the first
 * thing anyone types at an unfamiliar command, and answering it with `Error: Unknown option --help`
 * is the worst first impression a compiler can make.
 *
 * Asked for, it is not an error: stdout, and a zero status, so `sysl --help | less` works and a
 * script that checks the status is not told something went wrong. That is the whole difference
 * between this and the failure path, which keeps stderr and its 2.
 *
 * `OParser.usage` renders it, so this is scopt's own text rather than a second copy to keep in step
 * with the parser above.
 */
private def printUsage(): Int = {
  stdout(OParser.usage(parser) + "\n")
  0
}

/** The registry, as a reader of `sysl targets` sees it: the name to write, the LLVM triple it
 * stands for, and — for one sysl knows and cannot build for — why not.
 */
private def listTargets(): Int = {
  val width = Target.all.map(_.name.length).max

  for t <- Target.all do
    val here  = if Target.host.contains(t) then "  (this machine)" else ""
    val limit = t.unsupported.fold("")(why => s"  ($why)")

    stdout(s"${t.name.padTo(width, ' ')}  ${t.triple}$here$limit\n")

  // Always the words this machine's own runtime used, recognized or not. On a machine sysl has no
  // entry for that is the whole of what a report needs, and there is nowhere else to read it.
  stdout(s"\nthis machine reports: ${Target.hostMachineShown}\n")

  0
}
