package sh.sysl

import io.github.edadma.cross_platform.*

// The subcommands that write something: the two literate ones, the archive and header a C
// project links, the proof obligations, and the library artifact. Each is reached from
// `execute` once the compilation it needs is in hand.

/** The literate sources of a tree, or the reason there is nothing to render.
 *
 * **The ordinary `.sysl` files are passed over rather than refused**, because a directory holding
 * both is the normal shape of a literate module — `library/sysl/regex` is five `.lsysl` files and its
 * tests are not — and a command that refused the tree would be unusable on the very trees it is for.
 * What *is* refused is a tree with no literate source at all: there the request cannot be granted
 * however it is read, and saying so beats writing an empty document.
 */
private def literateSources(cfg: Config, sources: List[Source], verb: String): Either[String, List[Source]] = {
  val literate = sources.filter(src => Literate.named(src.name))

  if literate.isEmpty then
    Left(s"${cfg.file} holds no '${Literate.Extension}' files — '$verb' reads the prose a " +
      "literate source is written around, and an ordinary sysl program has none")
  else Right(literate)
}

/** `weave`: each literate source of a tree rendered as its own HTML document (`Weave`).
 *
 * **One source is one document, rather than a tree being joined into one.** A woven document is a
 * leaf artifact somebody opens, so the unit that makes sense is the file that was written; a tree
 * rendered end to end would put five modules under one title and one heading numbering. When the
 * path holds several sources, `-o` therefore names a **directory**, and each document is written
 * into it under its own name.
 */
private def weave(cfg: Config, sources: List[Source]): Int =
  literateSources(cfg, sources, "weave") match
    case Left(err) => fail(err)
    case Right(literate) =>
      val rendered = literate.map(src => src -> Weave.render(src))

      rendered.collectFirst { case (_, Left(err)) => err } match
        case Some(err) => fail(err)
        case None =>
          val documents = rendered.collect { case (src, Right(html)) => src -> html }

          (cfg.output, documents) match
            case (None, (_, html) :: Nil) => stdout(html); 0

            // Several documents and nowhere to put them: they cannot be told apart on a stream, and
            // concatenating them would produce one file with several `<html>` elements in it.
            case (None, _) =>
              fail(s"${cfg.file} holds ${documents.length} literate sources — 'weave' writes one " +
                "document each, so give '-o' a directory to write them into")

            case (Some(path), (_, html) :: Nil) => write(path, html)

            case (Some(dir), many) =>
              try
                Project.makeDirectories(dir)

                many.foldLeft(0) { (code, entry) =>
                  val (src, html) = entry

                  if code != 0 then code else write(s"$dir/${Weave.documentName(src.name)}", html)
                }
              catch case e: Exception => fail(s"cannot write $dir: ${e.getMessage}")

/** `tangle`: the program a literate source holds, with the prose stripped (`Literate.tangle`).
 *
 * **This is what the compiler reads**, and that is the whole of why it is worth a command. When a
 * literate file misbehaves — a block indented that should not have been, a fence that swallowed a
 * function — the question is always what tangling produced, and until now there was no way to ask.
 * It also hands the program to anything that does not know the format: a tool, a paste, a bug
 * report.
 */
private def tangle(cfg: Config, sources: List[Source]): Int =
  literateSources(cfg, sources, "tangle") match
    case Left(err) => fail(err)
    case Right(literate) =>
      val tangled = literate.map(Literate.tangle)

      tangled.collectFirst { case Left(err) => err } match
        case Some(err) => fail(err)
        case None =>
          // A tree's programs are joined rather than written separately, because unlike a document
          // they are all the same kind of thing and a module's source is read in one piece.
          val text = tangled.collect { case Right(src) => src.text }.mkString("\n")

          cfg.output match
            case Some(path) => write(path, text)
            case None       => stdout(text); 0

/** A rendered thing written where it was asked for, or the reason it could not be. */
private def write(path: String, text: String): Int = {
  Project.parentOf(path).foreach(Project.makeDirectories)

  try
    writeFile(path, text)
    Console.err.println(s"wrote $path")
    0
  catch case e: Exception => fail(s"cannot write $path: ${e.getMessage}")
}

/** `sysl build-c` — the static archive and the C header a C project is handed (`reference/ffi.md §
 * @export`).
 *
 * **This is `build-lib`'s shape with a different destination.** The compilation is the ordinary one
 * rather than a library build, because what is wanted is a module lowered for *this* target with its
 * calls resolved, not a tree of declarations somebody else will compile. What differs from `build`
 * is the entry point, which `cLibrary` above suppressed, and the ending: an archive rather than a
 * link.
 *
 * **The standard module is compiled into the archive**, always, and this is the one thing `build-c`
 * decides differently from every other command. A `.syslib` is not something a C project can link:
 * an archive referring to code no reachable file contains fails at the C link naming `sysl$prints`,
 * which is a symbol its author has no way to place. So the archive stands alone or it is not an
 * artifact. See `stdChoice`.
 *
 * **What is NOT in the archive is what this build's own libraries supply** — `libm`, and whatever
 * `@link` named — and the report says so rather than leaving it to be discovered at the C project's
 * link. Those are libraries the author chose and can be given to a linker, which is exactly the
 * distinction the standard module fails.
 *
 * **`roots` is every tree the compilation walked, not the project's own**, and it is a parameter
 * for that reason. `reference/ffi.md § A library may carry C` gives a source root named with
 * `--lib` and a package a `dependencies` block brought in the same answer as the project itself:
 * their C is compiled and reaches the link. This command reads that table exactly as
 * `NativeSources` does for the commands that link, and the archive is where "the link line" lands
 * when the consumer is a C project — an object the archive left out is one the C author has no way
 * to supply and no way to hear about, since the sysl half compiled cleanly and only the C project's
 * linker ever notices.
 */
private def buildForC(cfg: Config, compiled: Compiled, target: Target, named: Option[String],
                      roots: List[String], paths: SearchPaths): Int = {
  // Before the compile rather than after, exactly as `build-lib` does it: the archiver is not needed
  // until the end, so discovering it late would make "there is no llvm-ar" a thing somebody waited
  // the whole build for.
  val ar = Toolchain.findAr(cfg.ar) match
    case Left(err)   => return fail(err)
    case Right(path) => path

  // One flat list, because an archive is one flat namespace. `nativeMember` names a member after the
  // path inside its own tree, so two trees can still land on one name — which `collisions` reports,
  // rather than `ar r` replacing by name and shipping an archive quietly missing half of what one of
  // them defined.
  val native = roots.flatMap(Project.cSources(_, Some(target.os)))

  LibraryArtifact.collisions(native) match
    case Some(err) => return fail(err)
    case None      => ()

  val out    = cfg.output.getOrElse(defaultOutput(cfg.file, named, ".a"))
  val header = cfg.header.getOrElse(s"$out.h")

  Project.parentOf(out).foreach(Project.makeDirectories)
  Project.parentOf(header).foreach(Project.makeDirectories)

  // Named members for `build-lib`'s reason: an archive holds the same names wherever it was built,
  // and `ar t` then shows a reader something they can make sense of.
  val staging = createTempDirectory("sysl-c-")
  val code    = s"$staging/${LibraryArtifact.codeMember}"
  val objects = native.map(s => s -> s"$staging/${LibraryArtifact.nativeMember(s)}")

  val outcome =
    for
      _ <- Toolchain.compileObject(compiled.ir, code, target, cfg.optimize, cfg.cc)
      _ <- objects.foldLeft[Either[String, Unit]](Right(()))((so_far, entry) =>
             so_far.flatMap(_ => Toolchain.compileC(entry._1.name, entry._2, target, cfg.optimize,
               paths, cfg.verbose)))
      _ <- Toolchain.archive(code :: objects.map(_._2), out, ar)
    yield ()

  (code :: objects.map(_._2) ::: List(staging)).foreach(Project.discard)

  outcome match
    case Left(err) => fail(err)
    case Right(_)  =>
      writeFile(header, CHeader.render(compiled.exports, named.getOrElse(Project.nameOf(cfg.file))))
      Console.err.println(s"wrote $out")
      Console.err.println(s"wrote $header")

      // A build that exported nothing produced an archive with no way in, and the author almost
      // certainly meant to mark something. It is a warning rather than a refusal because an archive
      // of pure C shims is a real thing to want, and because the header says the same on its face.
      if compiled.exports.isEmpty then
        Console.err.println("sysl: warning: nothing in this module is marked '@export', so the " +
          "archive has no C entry point")

      // The libraries this object still needs, which the C project's link line has to carry. Said
      // here because nothing downstream will say it: an unresolved sysl symbol at that link reads
      // as a missing definition rather than as a missing archive.
      if compiled.links.nonEmpty then
        Console.err.println(s"sysl: link this against: ${compiled.links.mkString(", ")}")

      0
}

/** `sysl prove` — the module as WhyML, and what Why3 made of it (`reference/verification.md § sysl
 * prove`).
 *
 * **A proof is not a build.** Nothing is emitted and nothing about `sysl build` changes, which is
 * `reference/verification.md`: a module that fails to prove still compiles and still runs, with
 * every check `16` and `17` describe. What the prover buys is finding out before the program runs
 * rather than at the trap.
 *
 * The exit status is Why3's, so a proof run is usable in a build script that wants to fail on an
 * undischarged goal.
 *
 * **`paths` is not a build's leftovers, and this took a `SearchPaths` at all only from card `0325`.**
 * A proof stops before lowering, which is not before analysis, and a `c const` block is evaluated by
 * running the C compiler over the file's `@include`s while the tree is analyzed. So a proof of a
 * project binding an installed library needs the include paths a build needs — and with no parameter
 * here to receive them, `--include-path` was ignored rather than insufficient, which is why the flag
 * that rescued `emit-llvm` did nothing for this.
 */
private def prove(cfg: Config, sources: List[Source], libraries: List[Program], target: Target,
                  std: Stdlib, provides: Set[String], paths: SearchPaths): Int = {
  val (typed, ownModules) =
    Compiler.typedWith(sources, libraries, target, Some(std), provides, paths) match
    case Left(err)  => return report(err)
    case Right(out) => out

  if cfg.overflow != "check" && cfg.overflow != "ignore" then
    return fail(s"--overflow is 'check' or 'ignore', and '${cfg.overflow}' is neither")

  val mlw = WhyML.generate(typed, moduleName(cfg.file), cfg.overflow == "check", ownModules) match
    case Left(err)  => return fail(err)
    case Right(out) => out

  if cfg.emitWhyML then { stdout(mlw); return 0 }

  Toolchain.why3Prove(mlw) match
    case Left(err) => fail(err)
    case Right((code, out)) =>
      stdout(out)
      if code == 0 then Console.err.println("every goal was discharged")
      code
}

/** `sysl emit-typed` — one module's typed tree, as deterministic text (`TypedAstPrinter`), or its
 * declaration tables under `--tables`.
 *
 * It stops at the same typed tree `prove` reads, and for the same reason: analysis has already
 * checked every rule that could fail, and nothing past this point — pruning, lowering, codegen — is
 * needed to answer whether two analyzers agree on what a module means. An analysis error is
 * reported exactly as any other compiling command reports one, with nothing on standard output.
 */
private def emitTyped(cfg: Config, sources: List[Source], libraries: List[Program], target: Target,
                      std: Stdlib, provides: Set[String], paths: SearchPaths): Int = {
  val (typed, _) =
    Compiler.typedWith(sources, libraries, target, Some(std), provides, paths) match
    case Left(err)  => return report(err)
    case Right(out) => out

  stdout(if cfg.tables then TypedAstPrinter.tables(typed) else TypedAstPrinter.print(typed, spans = !cfg.noSpans))
  0
}

/** A WhyML module's name, taken from the file the program was given. It is only a label — Why3 needs
 * one and sysl's module names hold dots a WhyML identifier may not.
 */
private def moduleName(path: String): String = {
  val base = path.split('/').last.takeWhile(_ != '.')

  if base.isEmpty then "Program" else base.filter(c => c.isLetterOrDigit || c == '_')
}

/** `sysl build-lib <path> -o <artifact>` — a library compiled once, into the two halves a program
 * links against (`LibraryArtifact`).
 *
 * The artifact is **for one machine**, exactly as an `.rlib` is, because half of it is object code.
 * The other half is the tree, which would travel anywhere: a generic has no compiled form until the
 * program that calls it fixes its type arguments, so it is monomorphized in that program instead.
 *
 * `--std` says the library being built is sysl's own standard module, which is the one thing an
 * ordinary compilation may not declare. It is written down rather than inferred from the module
 * names in the tree: guessing it would turn a clear refusal — *you cannot add to the module every
 * program is compiled against* — into an artifact that builds and then collides with the built-in
 * copy at whatever link tried to use it.
 *
 * **`--lib` names what this library is built ON, and it is the only way one reaches here.** A
 * package built on another — `sdl3-ttf` on `sdl3` — needs that one's declarations to compile at all,
 * and gets them as a source root or as a `.syslib`, exactly as a program does. What it never does is
 * *fetch*: `dependencies` is a coordinate to resolve over the network, and a command whose whole job
 * is to compile one tree into an artifact for one machine should not be the thing that goes looking.
 * So a package that declares dependencies and is handed no library is refused, with the flag that
 * would have answered it named — rather than compiled until the analyzer reports a module nobody
 * mentioned.
 */
private def buildLibrary(cfg: Config, sources: List[Source], target: Target, std: Stdlib,
                         project: PackageConfig, libraries: List[Source],
                         libraryTrees: List[Program], allocator: Allocator,
                         paths: SearchPaths): Int = {
  val named = project.name

  // Said before anything is compiled, and said as a *request* the driver cannot meet rather than as
  // a missing module: the reader wrote a `dependencies` block that every other command honours, and
  // what they are owed is which command they are running and which flag stands in for it here.
  if project.dependencies.nonEmpty && cfg.libs.isEmpty then
    val labels = project.dependencies.map(d => s"'${d.label}'").mkString(", ")

    return fail(s"build-lib compiles the tree it is given and fetches nothing, so this package's " +
      s"dependencies reach it through --lib rather than through 'dependencies' — supply $labels as " +
      "a source root or as a '.syslib' built from one")

  // Before the library is compiled rather than after. Compiling it is the slow part and the archiver
  // is not needed until the end, so discovering it late would make "there is no llvm-ar" a thing a
  // user waited for the whole build to be told.
  val ar = Toolchain.findAr(cfg.ar) match
    case Left(err)   => return fail(err)
    case Right(path) => path

  // The library's C files, which become members of the archive beside the object its own modules
  // compiled to (`reference/ffi.md § A library may carry C`). Checked for a name collision here
  // rather than after the build, for the same reason the archiver is: the compile is the slow part,
  // and a name that cannot be archived is known before any of it has run.
  val native = Project.cSources(cfg.file, Some(target.os))

  LibraryArtifact.collisions(native) match
    case Some(err) => return fail(err)
    case None      => ()

  LibraryArtifact.build(sources, target, if cfg.std then LibraryArtifact.std else Set.empty, Some(std),
                        native, paths,
                        libraries, libraryTrees, allocator) match
    case Left(err) => report(err)
    case Right((ir, meta)) =>
      // The standard module's default output is the place a compilation looks for it, so that
      // building it and finding it are not two things to keep in agreement. Any other library is
      // named after the root it was built from and written inside it, which is the same rule a
      // build's executable follows and for the same reason: the root is the one place that names
      // the library without depending on where the caller happened to be standing.
      // **The key names the library that was COMPILED, which is not always the one this machine
      // would resolve.** `Std.candidates` tries the installed library before the working directory,
      // so an installed sysl run in a checkout resolves the installed one while being handed the
      // checkout's — and naming the artifact with `Std.fingerprint` put one library's bytes under
      // the other's key. Invisible while the compiled tree is a superset of the resolved one, which
      // is what an ordinary afternoon's editing makes it; an undefined symbol, or silently the wrong
      // implementation, the day it is not.
      val mine = Option.when(cfg.std)(Std.fingerprintOf(cfg.file, target.os))

      val out =
        cfg.output.getOrElse(
          if cfg.std then cfg.stdSearch.getOrElse(LibraryArtifact.stdDefault(target, allocator, mine))
          else defaultOutput(cfg.file, named, LibraryArtifact.extension))

      // **And say so where the two disagree**, because the command still did what it was asked and
      // the artifact still will not be read. The fast loop in a worktree is exactly this case: the
      // type-check is the point of the run and it happened, while the file it wrote is keyed to a
      // library no compilation here resolves. Reported rather than refused — a refusal would stop
      // the type-check, which is the thing the command is mostly run for.
      for
        fp   <- mine
        root <- Std.root.toOption
        if fp != Std.fingerprintOf(root, target.os)
      do
        Console.err.println(
          s"note: this is not the library a compilation here resolves — that one is at $root, and " +
            s"this artifact is keyed to ${cfg.file}, so nothing will read it. Set SYSL_LIB to " +
            "this tree, or pass -o, if it was meant to be used rather than only checked.")

      Project.parentOf(out).foreach(Project.makeDirectories)

      // The members are named rather than left as whatever a temporary file was called, so an
      // artifact holds the same names wherever it was built and `ar t` shows a reader something they
      // can make sense of. A directory of our own is what makes naming them possible.
      val staging  = createTempDirectory("sysl-lib-")

      // The one place this command writes outside the artifact it was asked for, so it is the one
      // place a reader has to be told about: a build interrupted between here and the cleanup below
      // leaves the directory named here, and nothing else in the run would say where it went.
      if cfg.verbose then trace(s"members staged in $staging")

      val code     = s"$staging/${LibraryArtifact.codeMember}"
      val metadata = s"$staging/${LibraryArtifact.metadataMember}"
      val objects  = native.map(s => s -> s"$staging/${LibraryArtifact.nativeMember(s)}")

      val outcome =
        for
          _ <- Toolchain.compileObject(ir, code, target, cfg.optimize, cfg.cc)
          _ <- Toolchain.compileObject(LibraryArtifact.metadataIr(meta, target), metadata, target,
                                       cfg.optimize, cfg.cc)
          // Each C file becomes its own member, so the linker pulls in a shim the same way it pulls
          // in anything else: because something left its symbol undefined.
          _ <- objects.foldLeft[Either[String, Unit]](Right(()))((so_far, entry) =>
                 so_far.flatMap(_ => Toolchain.compileC(entry._1.name, entry._2, target, cfg.optimize,
                   paths)))
          _ <- Toolchain.archive(code :: metadata :: objects.map(_._2), out, ar)
        yield ()

      (code :: metadata :: objects.map(_._2) ::: List(staging)).foreach(Project.discard)

      outcome match
        case Left(err) => fail(err)
        case Right(_)  => Console.err.println(s"wrote $out"); 0
}
