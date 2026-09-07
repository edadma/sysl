package sh.sysl

import io.github.edadma.cross_platform.*

// The driver: the entry point, the one pass over a compilation that every subcommand shares,
// and the questions it has to settle before compiling — which target, which standard module,
// and whether this command line answered what the packages asked for.

@main def sysl(args: String*): Unit = {
  ensureHeapCeiling(args)
  processExit(drive(processArgs(args)))
}

/** The whole of a command line, from the first word to the status it leaves.
 *
 * Held apart from the entry point for the reason `execute` is: a test asks the question a user's
 * shell asks. `execute` is one step further in and takes a parsed `Config`, so it cannot see the
 * step this function exists for — **which command word was written**, and whether sysl has one.
 */
private[sysl] def drive(all: Seq[String]): Int = {
  val (own, forwarded) = all.span(_ != "--")

  // A word sysl has no command for is somebody else's command, if they have installed one. git's
  // convention: `git foo` runs `git-foo`, which is how every third-party git subcommand has ever
  // worked, and it is what lets a tool with nothing to do with the compiler read as part of it.
  //
  // **A built-in always wins**, because this is only reached for a name `builtinCommands` does not
  // hold — so a binary called `sysl-build` cannot displace the compiler's own `build`, and adding a
  // command to the parser takes its name back automatically.
  //
  // The test is a leading word rather than a failed parse: reaching for this when scopt refuses
  // would send `sysl build --nonsense` looking for `sysl-build`, which is a flag mistake in a
  // command that exists. A leading `-` is not a command either — `sysl --version` is an option on
  // the program, and there is no `sysl---version`.
  all.headOption match
    case Some(name) if !name.startsWith("-") && !builtinCommands(name) =>
      subcommand(name, all.tail)

    case _ =>
      parseArgs(own) match
        case Some(cfg) => execute(cfg.copy(programArgs = forwarded.drop(1).toList))
        case None      => 2
}

/** Run an external subcommand — `sysl <name>` as `sysl-<name>`, with the rest of the line.
 *
 * **Everything after the name goes through untouched, `--` included.** sysl does not know what the
 * other program's arguments mean and has no business splitting them: the `--` that separates the
 * compiler's own flags from a program's is a convention of *this* command line, and a subcommand is
 * entitled to its own.
 *
 * `runProgram` rather than `exec`, for the reason `run` gives at its own call: `exec` is for a tool
 * whose output the compiler goes on to read, and closes the child's input at once. A subcommand is
 * the user's program — its input is theirs and what it writes is for them to read as it is written.
 * Its exit status becomes sysl's, so a script driving `sysl doc` sees what it would have seen
 * driving `sysl-doc`.
 *
 * **The refusal names what was looked for**, which is the whole reason the PATH is searched here
 * rather than left to the process API. "There is no `sysl-doc` on your PATH" tells somebody they
 * have a tool to install; `unknown command 'doc'` tells them they made a typo, and only one of
 * those is usually true.
 */
private def subcommand(name: String, rest: Seq[String]): Int =
  findOnPath(s"sysl-$name") match
    case Some(path) => runProgram(path +: rest)
    case None =>
      fail(s"'$name' is not a sysl command, and there is no 'sysl-$name' on the PATH — " +
        s"sysl runs an unknown subcommand as 'sysl-<name>', the way git does, so a tool that " +
        s"provides one has to be installed and on the PATH. '${builtinCommands.toList.sorted.mkString("', '")}' " +
        s"are the commands sysl has of its own")

/** What a subcommand does, and the exit status it leaves. Visible to the package so a test can drive
 * the driver rather than re-implementing it — the error paths here are the ones a user meets, and
 * none of them is reachable from the compiler's own API.
 */
private[sysl] def execute(asked: Config): Int = {
  // **A program in a package's `examples/` compiles against that package**, with nothing on the
  // command line saying so (`owningPackage`). It is added as a source root rather than handled
  // anywhere special, so everything downstream — the manifest's own `dependencies`, the C it
  // carries, the search paths — is what a `--lib` already gets, and a caller who wrote `--lib`
  // themselves is unaffected because a root named twice is one root.
  // **Rebindable, because a header requirement may be answered by an environment variable the
  // package named and that answer is not known until the manifest has been read.** Folding it into
  // the config is what makes every path below — the requirement check, clang's command line, a
  // dependency's own C — see it exactly as it sees a `--include-path`.
  var cfg =
    owningPackage(asked.file) match
      case Some(pkg) if !asked.libs.contains(pkg) => asked.copy(libs = asked.libs :+ pkg)
      case _                                      => asked

  if cfg.command == "version" then return printVersion()
  if cfg.command == "help" then return printUsage()
  if cfg.command == "targets" then return listTargets()

  // **Above everything a compilation needs, and above `readPackageConfig`.** Adding a dependency is
  // an edit to the manifest, so it asks for no target, no standard module and no library — and it
  // must not go through the reader that would refuse a project whose manifest is already saying
  // something this compiler cannot build here. What it does read the manifest for, it reads itself,
  // and it reads the *result* back before writing (`Add.run`).
  if cfg.command == "add" then
    return Add.run(if cfg.file.isEmpty then "." else cfg.file, cfg.spec) match
      case Left(err)  => fail(err)
      case Right(say) => println(say); 0

  // Rendering is a **source-level** job and stops here, above everything a compilation needs. It
  // asks for no target, no standard module and no library, which is not a shortcut but the whole
  // reason the command is usable: a package's prose is worth reading on a machine that could not
  // build it, and a document that could only be produced by a successful build would be missing
  // exactly when somebody wanted it.
  //
  // **So it reads every per-OS directory** (`Project.Every`), which is the only answer available with
  // no target chosen and is the right one either way: two implementations of one function are two
  // things worth reading, and `tangle` writes the tree back out with its shape intact.
  if cfg.command == "weave" || cfg.command == "tangle" then
    val rendered =
      try Project.collect(cfg.file, Project.Every)
      catch
        case e: SelectionError => return fail(e.getMessage)
        case e: Exception      => return fail(s"cannot read ${cfg.file}: ${e.getMessage}")

    if rendered.isEmpty then return fail(s"${cfg.file} holds no sysl source files")

    return if cfg.command == "weave" then weave(cfg, rendered) else tangle(cfg, rendered)

  val project = readPackageConfig(cfg.file) match
    case Left(err) => return fail(err)
    case Right(p)  => p

  // A key this compiler does not understand is said out loud and then ignored (`PackageConfig`'s
  // `unknownKeys`). On stderr, so it never lands in a program's own output, and once per key.
  project.warnings.foreach(Console.err.println)

  // **Above the target, and above every other question a compilation settles.** A graph is a
  // property of the manifests rather than of the machine, so a project that cannot be built here can
  // still be inspected — which is the same argument `weave` makes above, and the reason this sits
  // beside `targets`' early return rather than among the commands that compile. One step lower than
  // that one only because this command takes a path.
  //
  // An artifact named with `--lib` is a compiled library and has no manifest to read, so only the
  // source roots are handed over; `dependencies` splits them the same way for the same reason.
  if cfg.command == "deps" then
    return showDeps(cfg, project, cfg.libs.filterNot(LibraryArtifact.isArtifact))

  // Beside `deps` and for its reason: what a project takes is a property of the manifests rather
  // than of the machine, so a project that cannot be *built* here can still have its packages
  // brought down — which is most of what vendoring is for.
  if cfg.command == "vendor" then
    return vendorAll(cfg, project, cfg.libs.filterNot(LibraryArtifact.isArtifact))

  val target = chooseTarget(cfg.target, project.defaultTarget) match
    case Left(err) => return fail(err)
    case Right(t)  => t

  // **Below the target, because which files a tree holds is a question the target answers** — a
  // module's Linux implementation and its macOS one are different files, and `reference/modules.md
  // § Platform selection` selects between them by the directory they sit in. This used to sit above
  // everything, which was free while a tree meant the same thing to every machine.
  //
  // A malformed one is reported as itself rather than as a tree that would not read. The two are
  // different mistakes: one is a name somebody typed wrong, which the message can name and explain,
  // and the other is a permission or a missing path.
  val sources =
    try Project.collect(cfg.file, Some(target.os))
    catch
      case e: SelectionError => return fail(e.getMessage)
      case e: Exception      => return fail(s"cannot read ${cfg.file}: ${e.getMessage}")

  if sources.isEmpty then return fail(s"${cfg.file} holds no sysl source files")

  // The files, in the order the walk found them, which is the order the compiler reads them in.
  if cfg.verbose then
    trace(s"${sources.length} source file(s) under ${cfg.file}")
    sources.foreach(src => trace(s"  read ${src.name}"))

  val provides = project.provides(target.name)

  // What the package says it cannot be built without, asked of the machine it is being built for.
  // This is the config's own half of `requires`, and it is answered here rather than in the analyzer
  // because it is a statement about the *project* — there is no source position to point at, and a
  // build that cannot mean anything should stop before it parses a line.
  val unmet = project.requires.filterNot(provides).toList.sorted

  if unmet.nonEmpty then
    return fail(s"this package requires '${unmet.head}', and '${target.name}' does not provide it")

  // Compiling the standard module against a prebuilt copy of itself is the one combination that
  // cannot mean anything: the declarations being compiled are the ones the artifact holds. Refused
  // rather than ignored, since ignoring it leaves a command line that reads as though it were used.
  //
  // Named by what the two flags say rather than by the command that used to be the only one able to
  // say it — `test` takes `--std` as well now, and a message answering it with `build-lib` would be
  // about a command the reader had not typed.
  if cfg.std && cfg.stdLib.isDefined then
    return fail("--std says this tree is the standard module and --std-lib names a prebuilt one to " +
      "compile against — a compilation cannot be both")

  // Naming an artifact and refusing all of them at once has no reading either way round, and the two
  // spellings are near enough that a typo produces exactly this line. Refused rather than resolved by
  // precedence, since whichever precedence were chosen would silently discard half of what was asked.
  if cfg.noStdLib && cfg.stdLib.isDefined then
    return fail("--no-std-lib and --std-lib ask for different standard modules")

  // Refused rather than ignored, because it is the shape of the request that is wrong: an archive a C
  // project links cannot refer to a `.syslib`, so there is nothing a named one could do here but be
  // silently discarded. Said now rather than at the C project's link, where the symptom is an
  // undefined `sysl$` symbol and the cause is a flag on a command that ran successfully.
  if cLibrary(cfg.command) && cfg.stdLib.isDefined then
    return fail(s"${cfg.command} compiles the standard module into what it writes, since a C link " +
      "line cannot carry a '.syslib' — so --std-lib has nothing to name here")

  // A library reaches a compilation two ways, and neither is a second kind of input — both end as
  // **more modules**. Given as source its files carry the directory segments they were found under,
  // exactly as the program's do. Given as an artifact it arrives already parsed. Which one a path
  // names is read off the name, so a program that depends on a library need not know how it shipped.
  //
  // **Split here rather than where the two halves are read**, because both the fetch and the allocator
  // below have to consult the source roots, and both are settled above the standard module. Splitting
  // is a pure function of what was on the command line — no file is opened by it — so the only thing
  // moving it costs is that the reader meets it earlier than the code that uses it most.
  val (artifacts, roots) = cfg.libs.partition(LibraryArtifact.isArtifact)

  // What this compilation depends on, fetched and version-selected (`reference/packages.md §
  // Dependencies`, `§ 5`). A project with no `dependencies` and no declaring source root resolves
  // to itself and this costs nothing, which is what keeps `sysl run hello.sysl` free of ceremony.
  //
  // **Above the standard module rather than below it, because the allocator is settled here** and
  // every artifact this compilation reads is built for one allocator — the standard module's included.
  // It is still below every flag check, so a command line that cannot mean anything is refused without
  // reaching the network.
  //
  // `build-lib` fetches nothing, which is the invariant the early return below preserves: a command
  // that compiles one tree into an artifact for one machine should not reach the network, so a
  // `dependencies` block is refused by `buildLibrary` rather than acted on here.
  val fetched =
    if cfg.command == "build-lib" then PackageSources.none
    else
      dependencies(cfg, project, roots, target.os) match
        case Left(err) => return fail(err)
        case Right(d)  => d

  // The pair of C functions this whole program allocates through (`reference/packages.md § One
  // heap, and the package that names it`). A package that brings its own heap says so, and saying
  // so settles it for the program — which is the only shape that can work, because there is one
  // heap and whoever holds the last reference to something is who frees it (`03`). Two packages
  // naming different pairs is refused here rather than at the link, where it would not be refused
  // at all: both symbols resolve, and the program simply gives one allocator's storage back to
  // another.
  //
  // The project's own declaration is folded in beside the fetched ones, so an application with its own
  // heap and no dependency that has one is answered by the same rule — and so is a **library** being
  // built into an artifact, whose own is the whole answer because it has no dependencies to consult.
  //
  // **And a `--lib` source root's, which is the third road and used to be the silent one.** § 13 makes
  // this a property of the *package* rather than of how the package was reached, so a package that
  // brings its own heap brings it whichever way a build names it. Reached by coordinate it was
  // adopted; handed over as the same directory it was ignored, and what shipped was one program with
  // two heaps and nothing said about it at any point.
  //
  // An **artifact** is not consulted here and is not an omission: its object half is already compiled
  // against a pair, so there is nothing to adopt, and `LibraryArtifact.read` refuses one that does not
  // match rather than quietly re-deciding. Source is the only form where adopting is a thing that can
  // be done at all.
  val fromRoots = libAllocators(roots) match
    case Left(err)   => return fail(err)
    case Right(pair) => pair

  val allocator = Allocator.choose(
    project.allocator.map("this project" -> _).toList ::: fetched.allocators ::: fromRoots) match
    case Left(err) => return fail(err)
    case Right(a)  => a

  // **A heap the machine has no libc to supply, said here rather than left to the linker.** A
  // package requiring one, built for a freestanding target by a command that links, has asked for
  // `malloc` and `free` from a machine that has neither — and what a reader got was one
  // `undefined symbol` line per allocation, from a toolchain that already knew the answer.
  //
  // **It cannot be folded into the `unmet` test above, and the reason is the point.**
  // `PackageConfig.provides` defaults every capability to *provided*: the prior is that a machine
  // can do everything, and a config records what a machine cannot do. So a freestanding target
  // answers "I have a heap" unless the project wrote `capabilities { heap = false }`, and `unmet`
  // is empty. `Target.inherentCapabilities` is the physical half that would have caught it, and
  // `heap` is deliberately not in it — whether an allocator exists is an engineering decision about
  // a machine that could have one either way, which is what keeps a Pico project building against
  // newlib's `malloc` without having to say so.
  //
  // **So the discriminator is not the target. It is who is doing the link.** `build-c` and
  // `build-lib` are excluded because an archive's allocator arrives from CMake or Gradle, which the
  // compiler cannot see and must not second-guess; every board project in the org is built that way.
  // Where sysl invokes the linker itself, there is nothing else left to supply the pair.
  //
  // **Naming an allocator is the way out, and naming libc's own pair is not.** A project that
  // declared `malloc`/`free` explicitly is in exactly the position of one that declared nothing —
  // the symbols still have to come from somewhere — so the test is the resulting pair rather than
  // whether a block was written.
  if links(cfg.command) && target.os == Os.Freestanding &&
    project.requires(Capability.Heap) && allocator == Allocator.c
  then
    return fail(s"this package requires a heap and '${target.name}' is freestanding, so nothing " +
      s"supplies '${allocator.alloc}' and '${allocator.free}' — the link would answer with an " +
      "undefined symbol for each. Name a heap of your own in " +
      s"${PackageConfig.FileName}'s 'allocator' block, or build for a target that has a libc")

  // Which standard module this compilation is compiled against — an error if there is none, the same
  // as any other missing library.
  val Stdlib.Resolved(std, coreSymbols, coreArchive) =
    Stdlib.resolve(stdChoice(cfg, target), target, allocator, cfg.cc) match
    case Left(err) => return fail(err)
    case Right(c)  => c

  // **Which standard module this compilation got, and by which route.** An artifact that was read
  // and one that was rejected and rebuilt reach here looking identical, and the difference is the
  // first thing anybody diagnosing a stale library wants: `Stdlib.resolve` announces a *rebuild* on
  // its own, so what is added here is the quiet case.
  if cfg.verbose then
    coreArchive match
      case Some(archive) => trace(s"standard module linked from $archive")
      case None          => trace("standard module compiled from source, not linked from an artifact")

  // **A machine with no C toolchain is refused before anything is compiled**, because every command
  // below this line that produces an artifact produces it by handing a triple to clang — and CRAFT
  // has no clang to hand one to. Its back end is an out-of-tree `llc` rather than a driver, and the
  // machine has no libc, no object format and no linker (`Target.buildsWithClang`).
  //
  // It is refused *here*, at the target, rather than at the point each subcommand reaches for a
  // driver: the reader asked for a build of a machine that cannot be built, which is one mistake
  // with one answer, and five separate failures deep in the toolchain would each describe the last
  // step rather than the first. The answer names what *does* work, since there is one.
  // `emit-llvm` is the whole of what this machine supports and `prove` never lowers at all, so both
  // go through — the list is what needs a *driver* rather than what needs a target.
  if !target.buildsWithClang && !Set("emit-llvm", "prove")(cfg.command) then
    return fail(target.noToolchain)

  // Running the result is what makes `run` and `test` different from `build`, and only this machine
  // can do that — so a cross target is refused here rather than built and then failed to execute.
  //
  // **`test` used to refuse it in `TestRunner`, one step too late.** `TestRunner.run` is *handed*
  // the objects a tree's C compiled to, so by the time it reached its own check the library's C had
  // already been compiled for the cross target — and a shim under a `__<os>__` directory only
  // compiles on a host that has that system's headers. On Linux the first cross target in the
  // registry is `aarch64-macos`, so `sysl test --target <cross>` reported that
  // `library/sysl/fs/__macos__/dirent.c` would not compile, in place of the refusal it was about to
  // make anyway. `TestRunner` keeps its own check, since it is reachable without this driver.
  if Set("run", "test")(cfg.command) && !Target.host.contains(target) then
    return fail(s"'${cfg.command}' ${if cfg.command == "run" then "executes" else "runs"} what it " +
      s"builds, and '${target.name}' is not this machine — use 'sysl build --target ${target.name}'")

  // Only the metadata is read here. The object half stays in the file and reaches the linker as the
  // archive it already is — there is nothing to unwrap, nothing to write to a temporary, and nothing
  // to clean up on the paths below that refuse the compilation.
  val unpacked =
    try artifacts.map(p => LibraryArtifact.metadataOf(p, readBytes(p)))
    catch case e: Exception => return fail(s"cannot read a library: ${e.getMessage}")

  unpacked.collectFirst { case Left(e) => e } match
    case Some(e) => return fail(e)
    case None    => ()

  val decoded =
    artifacts.zip(unpacked).collect { case (p, Right(meta)) =>
      LibraryArtifact.read(p, meta, target, allocator) }

  decoded.collectFirst { case Left(e) => e } match
    case Some(e) => return fail(e)
    case None    => ()

  val collected =
    try roots.map(root => root -> Project.collect(root, Some(target.os)))
    catch
      case e: SelectionError => return fail(e.getMessage)
      case e: Exception      => return fail(s"cannot read a library: ${e.getMessage}")

  collected.find(_._2.isEmpty) match
    case Some((root, _)) => return fail(s"$root holds no sysl source files")
    case None            => ()

  // Building a library stops here — there is no program to link it into. An artifact is **for a
  // machine**, exactly as an rlib is, because half of it is compiled object code; the generic half
  // travels as trees because there is nothing to compile until a caller fixes its type arguments.
  //
  // **Below the `--lib` resolution and above the fetch, which is the whole of what `build-lib` was
  // missing.** A library built on another library needs that library's declarations to compile at
  // all, and the two ways of naming one — a source root and a `.syslib` — are resolved just above.
  // What it does not do is *fetch*: a command that compiles one tree into an artifact for one
  // machine should not reach the network, so a `dependencies` block is refused here rather than
  // acted on, and `buildLibrary` says so in as many words.
  if cfg.command == "build-lib" then
    // Asked here rather than with the others below, because this return is above them and the reason
    // they are asked applies squarely: `buildLibrary` compiles this package's C into the artifact,
    // so a header it cannot find fails inside clang exactly as it would for a `build`.
    //
    // **Its own manifest and nothing else, which is narrower than every other command and is what
    // this command actually does.** `Project.cSources(cfg.file)` is the whole of the C compiled here
    // — a `--lib` source root's C is not, because that root's C belongs in that root's own artifact,
    // and a `dependencies` block is refused outright a few lines into `buildLibrary` rather than
    // fetched. Charging for either would be charging for a header this command will never open.
    cfg = cfg.withNamedIncludes(envHeaders(ownHeaderNeeds(project), cfg.namedIncludes.keySet))

    unmetHeaders(project, Nil, cfg.namedIncludes.keySet) match
      case Some(err) => return fail(err)
      case None      => ()

    // The same narrowing, for the same reason: this command compiles this package's C and no other,
    // so it is asked about the libraries *this* manifest names and nothing a root or a dependency
    // declared. A `.pc` answer reaches the C compiler here exactly as it does for a `build`.
    val libPaths = probeLibs(
      project.pkgConfig.toList.sortBy(_._1).map((mod, why) => LibNeed("this project", mod, why)),
      cfg.namedIncludes.keySet, target, cfg.verbose) match
      case Left(err)     => return fail(err)
      case Right(answer) => answer

    return buildLibrary(cfg, sources, target, std, project,
                        collected.flatMap(_._2), decoded.collect { case Right(r) => r._1 }.flatten,
                        allocator,
                        SearchPaths(cfg.linkPaths, cfg.includePaths, cfg.defines,
                                    libPaths.probed, libPaths.probedLibs,
                                    carriedOf(cfg.file, project, target.os) match
                                      case Left(err)     => return fail(err)
                                      case Right(answer) => answer,
                                    cfg.cc))

  val librarySources = collected.flatMap(_._2) ::: fetched.sources
  val packages       = fetched.packages
  val read           = decoded.collect { case Right(r) => r }
  val libraryTrees   = read.flatMap(_._1)

  // What the libraries already compiled, so this module declares those rather than defining them a
  // second time. Their bodies arrive from the archives at link time. The standard module's are in
  // here on the same footing as a named library's: what a prebuilt std buys a program is exactly
  // that its share of the library stops being emitted into every one.
  val precompiled = read.flatMap(_._2).toSet ++ coreSymbols

  // What the linker is handed: the artifacts themselves. A `.syslib` **is** an archive, so it goes on
  // the link line as it stands and the linker takes only the members that resolve something.
  val archives = artifacts ::: coreArchive.toList

  // The C beside the sysl, of every tree this compilation walked — the project's own, each `--lib`
  // source root's, and each package's (`reference/ffi.md § A library may carry C`,
  // `NativeSources`). An artifact is not among them: a `.syslib` carries its C already compiled, as
  // archive members.
  //
  // Where this machine keeps what the toolchain was not told the
  // location of (`SearchPaths`). One value rather than two lists threaded separately, because the
  // two halves are one setting: a binding to a library outside the default prefix needs its headers
  // to compile and its archive to link, and a build given only one of them fails at whichever step
  // comes first. What this machine answered about the installed libraries the packages named
  // (`reference/packages.md § Capabilities`). Asked under the same guard as the header requirements
  // below and for the same reason — a command that reads no C opens none of these — and asked *here*
  // because the answer is part of the paths every C compilation and the link are given.
  //
  // **The guard used to be `links || cLibrary`, and the two halves it bundles are not one question.**
  // The *link* half is right to stop there: `emit-llvm` and `prove` never reach a linker. The
  // *include* half is not, because a `c const` block is evaluated during analysis, which they both
  // do — so `sysl emit-llvm` failed with `'uv.h' file not found` on a project `sysl build` compiled
  // (card `0325`). Probing for a command that will not link costs a `pkg-config` run whose ldflags
  // go unread, which is the right price for the two agreeing.
  val probed =
    if analyzesC(cfg.command) then
      val fromLibs = libPkgNeeds(roots) match
        case Left(err)   => return fail(err)
        case Right(need) => need

      val own = project.pkgConfig.toList.sortBy(_._1)
        .map((mod, why) => LibNeed("this project", mod, why))

      probeLibs(own ::: fetched.libs ::: fromLibs, cfg.namedIncludes.keySet, target, cfg.verbose) match
        case Left(err)     => return fail(err)
        case Right(answer) => answer
    else SearchPaths()

  // What each package said its **own** carried C is compiled with (`reference/packages.md § No
  // build scripts, ever`), keyed by the path that C will be compiled from. Three roads reach it and
  // all three are here: the project's own manifest, each `--lib` source root's, and each fetched
  // package's — the same three the header requirements are gathered from, for the same reason.
  val carried = (for
    own      <- carriedOf(cfg.file, project, target.os)
    fromLibs <- libDefines(roots, target.os)
  yield own ++ fromLibs ++ fetched.defines) match
    case Left(err)     => return fail(err)
    case Right(answer) => answer

  val paths = SearchPaths(cfg.linkPaths, cfg.includePaths, cfg.defines,
                          probed.probed, probed.probedLibs, carried, cfg.cc)

  if cfg.verbose then
    for lib <- cfg.libs do trace(s"library: $lib")
    for dir <- cfg.linkPaths do trace(s"link path: $dir")
    for dir <- cfg.includePaths do trace(s"include path: $dir")
    for d <- cfg.defines do trace(s"define: $d")

    // Said even when it is libc's, because "which allocator is this program using" is a question with
    // an answer whatever the answer is, and one that is silent until a package changes it reads as a
    // setting that does not exist.
    val who = (project.allocator.map("this project" -> _).toList ::: fetched.allocators ::: fromRoots)
      .collectFirst { case (name, a) if a == allocator => name }

    trace(s"allocator: ${allocator.alloc} / ${allocator.free}" +
      who.fold(" (the C default)")(n => s" (named by $n)"))

  // What the packages said their C has to be able to find, asked of what this command line supplied
  // (`reference/packages.md § Capabilities`). Answered here rather than left to clang because a
  // header that is not there fails inside a compiler that has never heard of sysl: what comes back
  // is `'lwip/tcp.h' file not found`, which names neither the package that wanted it nor the flag
  // that would have supplied it.
  //
  // Asked only where C is going to be compiled. The requirement exists so that a tree's C compiles,
  // so a command that compiles none has nothing unmet — and refusing `emit-llvm` or `prove` over a
  // path they would never open would be charging for something they do not do. `build-lib` compiles
  // C too and is asked at its own return above, for its own manifest only.
  //
  // **Deliberately narrower than `analyzesC` above, and the two are different questions.** A
  // declared requirement is about the tree's *carried* C: a project whose only C is a shim needs
  // those headers to build and does not need them to print its IR. What `emit-llvm` and `prove` were
  // missing is the probe's answer and `--include-path`, which they now have; a `c const` in a
  // declaring project meets clang's message rather than this one, which is the price of not refusing
  // a command over a file it never opens.
  //
  // A `--lib` **source root** is asked too, and is the one road that used to fall through. A package
  // reached through `dependencies` is checked because its manifest came back with the graph, and one
  // reached as a `.syslib` needs no header at all — its `c const` was lowered when the artifact was
  // built. Handed the same package as a directory, the driver read its `.sysl` and nothing else, so
  // the requirement it had written down went unasked and clang answered instead.
  if links(cfg.command) || cLibrary(cfg.command) then
    val fromLibs = libHeaderNeeds(roots) match
      case Left(err)    => return fail(err)
      case Right(needs) => needs

    // **The dependencies' needs are in here too, not only this project's.** A program depending on
    // `pico2` never writes that requirement down itself, so resolving only the project's own would
    // leave the one case this exists for unanswered.
    cfg = cfg.withNamedIncludes(
      envHeaders(ownHeaderNeeds(project) ::: fetched.needs ::: fromLibs, cfg.namedIncludes.keySet))

    unmetHeaders(project, fetched.needs ::: fromLibs, cfg.namedIncludes.keySet) match
      case Some(err) => return fail(err)
      case None      => ()

  // **The standard library's own tree, and only where it was compiled from source.** The library is
  // a library (`reference/modules.md § Separate compilation`) and may carry C exactly as any other
  // tree may (`reference/ffi.md § A library may carry C`) — `sysl.fs` reaches `struct dirent`
  // through a shim under a `__<os>__` directory, which is the only way to reach a layout that
  // differs by platform and the reason the directories exist (`reference/modules.md § Platform
  // selection`).
  //
  // **Two compilations have no standard library to add, and both would add it twice.** Where one
  // arrived as an **artifact** its shims are already archive members. And under `--std` the tree
  // being compiled *is* the standard module (`reference/modules.md § Separate compilation`), so
  // `cfg.file` is already this very directory — `sysl test library --std` is the case, and what it
  // produced was two `sysl.fs.dirent.o` and a duplicate symbol at the link.
  // **A `.c` nothing will compile, named at the moment it is walked past.** A directory holding C
  // and no sysl is not a module, so its files belong to no compilation — which is deliberate, and
  // which said nothing at all until card `0323`: what a reader got was no `compile:` line and then a
  // link error naming a symbol, indistinguishable from the compiler having ignored a file it should
  // have taken. This is that skip in one sentence, with the one-file remedy in it.
  //
  // Asked of the reader's own trees only. A dependency's stray `.c` is its author's to fix and a
  // consumer can do nothing with the warning, and the standard library has none.
  if analyzesC(cfg.command) then
    for
      root <- cfg.file :: roots
      dir  <- Project.skippedC(root, Some(target.os))
    do
      Console.err.println(s"sysl: warning: '$dir' holds C and no sysl, so it is not a module and " +
        "its '.c' files are compiled by nothing — a directory becomes one by holding the '.sysl' " +
        "that declares those 'extern's")

  val stdTree =
    Option.when(coreArchive.isEmpty && !cfg.std)(Std.root.toOption).flatten.toList

  // **A function rather than a value, because *when* the C is compiled is a decision.** It used to be
  // computed here, above both the test branch and the compilation below — so a program the analyzer
  // was going to refuse paid for a C compile first, and where that C could not be compiled at all the
  // error from it arrived *instead of* the diagnostic. On Linux, `sysl build --target aarch64-macos`
  // said `library/sysl/fs/__macos__/dirent.c` would not compile — true, since a macOS shim needs
  // macOS headers, and nothing to do with the program in front of it.
  //
  // So each command asks for it where it needs it: a test build below its own branch, and an
  // ordinary build below the analysis. What a reader gets from a program that will not compile is
  // now the reason it will not compile.
  def nativeSources(): Either[String, NativeSources.Built] =
    if links(cfg.command) then
      NativeSources.build(NativeSources.of(cfg.file :: roots ::: fetched.roots ::: stdTree, target.os),
        target, cfg.optimize, paths, cfg.verbose)
    else Right(NativeSources.none)

  // **What `sysl run` built last time**, keyed over everything that can reach the bytes (`RunCache`).
  //
  // The check is here rather than beside the link, and that is the whole of what it buys: the
  // compilation below is the expensive half, so a cache consulted after it would have saved the
  // linker's second and paid the analyzer's nine. Everything the key is over is settled by this
  // point — the sources, the libraries, the standard module, the target, the allocator and the
  // search paths — which is why this is the first line at which it *can* be asked.
  //
  // `run` alone. A `build` writes a binary somebody named and is expected to have built it; a
  // `build-c` and a `build-lib` write artifacts for somebody else's toolchain, and neither is
  // something a reader would want quietly skipped.
  val runKey =
    Option.when(cfg.command == "run" || cfg.command == "test")(RunCache.key(List(
      cfg.command,
      BuildInfo.version,
      target.name,
      s"${allocator.alloc}/${allocator.free}",
      cfg.optimize,
      // The standard module's own identity: the archive's bytes where one was read, and the
      // library's fingerprint where it was compiled from source. `Std.fingerprint` is not enough on
      // its own, because `--std-lib` may name an artifact built from a different tree entirely.
      coreArchive.map(a => s"$a:${fingerprintOfFile(a)}")
        .getOrElse(s"source:${Std.root.map(Std.fingerprintOf(_, target.os)).getOrElse("none")}"),
      // Every file the program is made of, and the C beside it. `fingerprint` is over each file's
      // place in its tree and its text, sorted, so the order these arrive in cannot matter.
      LibraryArtifact.fingerprint(sources ::: librarySources),
      LibraryArtifact.fingerprint(NativeSources.of(cfg.file :: roots ::: fetched.roots ::: stdTree,
                                                   target.os).flatten),
      // An artifact named with `--lib` has no source to hash, so it is hashed as bytes.
      artifacts.map(a => s"$a:${fingerprintOfFile(a)}").mkString("\u0000"),
      // **The environment that reaches the toolchain** (`Toolchain.buildEnvironment`, card `0415`).
      // Everything else in this key is settled by the command line and the source tree; this is the
      // half that is not, and it was missing — so an `SYSL_EXTRA_CFLAGS="-fsanitize=address"` run
      // over an unchanged tree was handed back the *uninstrumented* binary the previous ordinary
      // run had left in the slot, and reported green having looked at nothing.
      Toolchain.buildEnvironment.mkString(" "),
      paths.toString,
      archives.mkString("\u0000"),
    )))

  // `for ... yield` rather than `for ... do`, and the difference is not style: a `do` desugars to a
  // `foreach`, so the `return` inside it would be a *non-local* return out of a closure — which
  // Scala 3 no longer supports. Yielding the hit and matching on it puts the return back in this
  // method's own body, where it means what it says.
  if cfg.command == "run" then
    (for key <- runKey; built <- RunCache.hit(key) yield built) match
      case Some(built) =>
        if cfg.verbose then Console.err.println(s"cached: $built")

        return runProgram(built :: cfg.programArgs)
      case None => ()

  // A test build is its own compilation and branches before the one below, rather than sharing it:
  // it keeps the `@test` functions every other build drops, and it lowers a different entry point
  // (`Tests`). Everything up to here — the libraries, the standard module, the target — is the same,
  // which is why the branch is here and not at the top.
  if cfg.command == "test" then
    // A cached test build needs **two** things — the binary and what to call in it — so the hit is
    // both or neither. The filter is applied to the list afterwards, exactly as it is to a freshly
    // compiled one, which is what keeps `--filter` out of the key.
    val cached =
      for
        key   <- runKey
        built <- RunCache.hit(key)
        list  <- RunCache.tests(key).filter(isFile).flatMap(p => RunCache.decode(readFile(p)))
      yield (built, list)

    cached match
      case Some((built, list)) =>
        if cfg.verbose then Console.err.println(s"cached: $built")

        return TestRunner.rerun(cfg, built, list)
      case None => ()

    val native = nativeSources() match
      case Left(err)    => return fail(err)
      case Right(built) => built

    val status =
      TestRunner.run(cfg, sources, libraryTrees, target, precompiled, std, archives,
        native.objects, paths, allocator, librarySources, runKey, fetched.devModules)

    native.scratch.foreach(Project.discard)
    return status

  // Proving stops at the typed tree and never lowers, so it branches before the compilation below
  // (`reference/verification.md § sysl prove`). It reads the tree before pruning and before
  // `@ghost` erasure, because the predicates a specification is written in are exactly what the
  // lowering drops.
  if cfg.command == "prove" then
    return prove(cfg, librarySources ::: sources, libraryTrees, target, std, provides, paths)

  // One compilation, whatever the subcommand does with it. The notes come back beside the IR
  // rather than being printed from inside the compiler, which has no business writing to a console.
  val compiled =
    Compiler.compiledWith(sources, libraryTrees, target, precompiled, Some(std),
      provides, packages, entryPoint = !cLibrary(cfg.command), paths, allocator,
      librarySources) match
    case Left(err) => return report(err)
    case Right(result) =>
      if cfg.explainEscapes then
        if result.notes.isEmpty then Console.err.println("no arrays were promoted to the heap")
        else result.notes.foreach(Console.err.println)

      // Warnings are printed on **every** successful compilation, unlike the notes above, which are
      // an answer to a question `--explain-escapes` asked. A warning is not something a reader
      // opted into: it is the compiler saying the program compiles and is probably not what was
      // meant, and one nobody sees is one that has done nothing (card `0372`).
      result.warnings.foreach(w => Console.err.println(w.rendered))
      result

  // Below the compilation, which is the whole point of it being a function — see `nativeSources`.
  val native = nativeSources() match
    case Left(err)    => return fail(err)
    case Right(built) => built

  val status = cfg.command match
    case "emit-llvm" =>
      stdout(compiled.ir); 0

    case "emit-header" =>
      stdout(CHeader.render(compiled.exports, project.name.getOrElse(Project.nameOf(cfg.file)))); 0

    case "build-c" =>
      // The standard library's tree goes in with the rest, on the same condition the link line uses
      // it: `build-c` compiles the standard module into what it writes, so a shim of the library's
      // own that the archive left out is a symbol the C project's linker reports and its author
      // cannot supply.
      buildForC(cfg, compiled, target, project.name, cfg.file :: roots ::: fetched.roots ::: stdTree,
                paths)

    case "build" =>
      val exe = cfg.output.getOrElse(defaultOutput(cfg.file, project.name))

      // **The output path being a directory is caught here rather than by the linker.** A project
      // named for its own module directory is the obvious layout and reaches this without trying:
      // the executable takes the package's name, the module directory has the same one, and `ld`
      // answers `open() failed, errno=21 (Is a directory)` — an errno, naming neither the package
      // nor the directory, from a tool the reader did not invoke. The condition is known long
      // before clang is, and which of the two names to change depends on where this one came from.
      if isDirectory(exe) then
        fail(
          if cfg.output.isDefined then
            s"'-o $exe' names a directory, and an executable cannot be written over one"
          else
            s"this project builds an executable named '${Project.basename(exe)}', and '$exe' is a " +
              "directory — the two cannot both have that name. Rename the package or the directory, " +
              "or say where the binary goes with '-o <path>'")
      else
        Toolchain.build(compiled.ir, exe, target, archives, cfg.optimize, compiled.links, native.objects,
          paths, cfg.verbose) match
          case Left(err) => fail(err)
          case Right(_)  => Console.err.println(s"wrote $exe"); 0

    case "run" =>
      // Linked **straight into the cache slot** where there is one, rather than to a temporary that
      // is copied afterwards: a copy would have to reproduce the executable bit, which is a thing to
      // get wrong once per platform, and linking there gets it from the linker.
      val keeping = runKey.flatMap(RunCache.reserve)
      val exe     = keeping.getOrElse(createTempFile("sysl-", ""))

      Toolchain.build(compiled.ir, exe, target, archives, cfg.optimize, compiled.links, native.objects,
        paths, cfg.verbose) match
        case Left(err) => Project.discard(exe); fail(err)
        case Right(_) =>
          // `runProgram` and not `exec`, and the difference is the whole of what this command is
          // for. `exec` runs a **tool** — `clang`, `git`, `llvm-ar` — whose output is a value the
          // compiler goes on to inspect, so it closes the child's input and hands back two strings
          // once the child has finished. A program the user asked to run is the opposite on every
          // count: its input is the user's, and what it writes is for the user to read as it is
          // written. Running one through `exec` handed it end-of-input at once, so a `wc` read
          // nothing from the pipe it was given and a console exited after its banner.
          val status = runProgram(exe :: cfg.programArgs)

          // Kept where it is the cache's, discarded where it is a temporary of this run.
          if keeping.isEmpty then Project.discard(exe)
          status

    case other =>
      fail(s"unknown command '$other'")

  // The objects were a temporary of this build, whichever way it went — a link that failed leaves
  // them as surely as one that worked.
  native.scratch.foreach(Project.discard)
  status
}

/** One file's bytes as a fingerprint, for the two inputs to a run key that are compiled artifacts
 * rather than source: a `.syslib` named with `--lib`, and the standard module's archive.
 *
 * Read as text rather than as bytes, which is safe here for the one reason it usually is not: what
 * comes back is fed to a hash and never written out again, so a decode that maps two byte sequences
 * to one string would cost a false *hit* — and the encoding is fixed, so two runs of one file map to
 * one string whatever that string is. A file that will not read at all is a miss, which is a rebuild.
 */
private def fingerprintOfFile(path: String): String =
  try LibraryArtifact.fingerprint(List(Source(path, readFile(path))))
  catch case _: Exception => "unreadable"

/** Whether a subcommand ends at the linker, which is what decides whether a tree's C is worth
 * compiling. `emit-llvm` and `prove` do not, and `build-lib` never reaches here — it archives its
 * own C rather than linking it.
 */
private def links(command: String): Boolean = command == "build" || command == "run" || command == "test"

/** Whether a subcommand **reads** C, which is what decides whether the headers have to be found.
 *
 * It is every command that gets this far, and saying so as a list rather than as `true` is the
 * point: the ones that do not — `deps`, `weave`, `tangle`, `version` — have already returned above,
 * and `build-lib` asks its own question at its own return. A later subcommand that analyzes sysl
 * source belongs here, and one that does not will not reach the guard anyway.
 *
 * **This is `links` split in two, and conflating them was card `0325`.** A `c const` block is
 * evaluated by running the C compiler over the file's `@include`s during analysis (`CProbe`), so a
 * command that never links still needs somewhere to find `uv.h`. Asking "does this link?" where the
 * question is "does this read a header?" made `sysl emit-llvm` fail on a project `sysl build`
 * compiled — on the one command somebody reaches for to *diagnose* a build.
 */
private def analyzesC(command: String): Boolean =
  links(command) || cLibrary(command) || command == "emit-llvm" || command == "prove"

/** The allocator each `--lib` **source root** declares, named by the root as the reader wrote it.
 *
 * `reference/packages.md § One heap, and the package that names it` makes the allocator a property
 * of the *package*: one that brings its own heap settles the question for the whole program,
 * because there is one heap and whoever holds the last reference is who frees it. That is a claim
 * about the package rather than about the road it arrived by, so a package reached as a directory
 * answers exactly as the same package reached by coordinate.
 *
 * **Until this existed the two roads disagreed in silence**, which is the worst way for them to
 * disagree: a coordinate adopted the pair and a source root ignored it, so one program allocated
 * through two heaps and nothing said so at any point. Only the `-v` line differed, and the failure it
 * predicts is a `free` of storage the other allocator owns.
 *
 * **An artifact is deliberately not here.** Its object half is already compiled against a pair, so
 * there is nothing to adopt — `LibraryArtifact.read` refuses one that disagrees with the program
 * rather than re-deciding, and that refusal is a physical constraint rather than a policy this could
 * have shared.
 *
 * A root with no manifest has nothing to declare, exactly as for the header requirements beside this,
 * and one whose manifest will not parse stops the build rather than being skipped.
 */
private def libAllocators(roots: List[String]): Either[String, List[(String, Allocator)]] =
  roots.foldLeft[Either[String, List[(String, Allocator)]]](Right(Nil)) { (acc, root) =>
    for
      seen   <- acc
      config <- readPackageConfig(root)
    yield seen ::: config.allocator.map(root -> _).toList
  }

/** What the `--lib` **source roots** declare their own carried C is compiled with
 * (`reference/packages.md § No build scripts, ever`).
 *
 * Read off the same manifest as `libHeaderNeeds` and for the same reason: a package handed over as a
 * directory is the same package it is by coordinate, so what its C is compiled with cannot depend on
 * how it got here. A binding being developed is reached this way — `sysl test .` in the package's
 * own tree — so this is the road the author uses before anybody else has the package at all, and a
 * `defines` block that worked only once fetched would fail exactly where it is being written.
 *
 * **Unlike the allocator beside it, this is safe to take from a `--lib` root.** An allocator taken
 * silently is the mixed heap `reference/packages.md § One heap, and the package that names it`
 * exists to prevent, because it decides something for the whole program; a macro here reaches one
 * translation unit in the tree that declared it, and a root that is not a package has no `defines`
 * block to be read.
 */
private def libDefines(roots: List[String], os: Os): Either[String, Map[String, List[String]]] =
  roots.foldLeft[Either[String, Map[String, List[String]]]](Right(Map.empty)) { (acc, root) =>
    for
      seen   <- acc
      config <- readPackageConfig(root)
      mine   <- carriedOf(root, config, os)
    yield seen ++ mine
  }

/** One tree's declared macros, keyed by the path its C was found at.
 *
 * **The walk is only done for a tree that declared something**, which is why this is a function
 * rather than a step: reading a package's C to resolve an empty block would be work every build
 * pays for a feature almost none of them use.
 */
private def carriedOf(root: String, config: PackageConfig, os: Os)
    : Either[String, Map[String, List[String]]] =
  if config.defines.isEmpty then Right(Map.empty)
  else config.carriedDefines(Project.cSources(root, Some(os)).map(_.name))

/** What the `--lib` **source roots** declare they need headers for (`reference/packages.md §
 * Capabilities`).
 *
 * **This is the one road a declared requirement used to fall through.** The other two are already
 * answered and neither needed anything: a package reached through `dependencies` arrives with its
 * manifest as part of the graph, and one reached as a `.syslib` never needs a header at all, because
 * `LibraryArtifact` lowers a `c const` to its measured value before writing. Given the very same
 * package as a **directory**, though, the driver collected its `.sysl` files and opened nothing else
 * — so the sentence its author wrote for exactly this moment was never read, and clang's
 * `'cairo.h' file not found` answered in its place, naming neither the package nor the flag.
 *
 * **Only `requires { headers }` is taken from the manifest, and that is deliberate.** `--lib` names
 * a *source root*, which need not be a package at all, and a root that is not one has nothing to
 * say here. Reading the rest of what a manifest can declare is a different question with a real
 * cost — an allocator taken silently from a `--lib` root is the mixed heap `reference/packages.md §
 * One heap, and the package that names it` exists to prevent — so it is left alone rather than
 * guessed at.
 *
 * **The root is named as the reader wrote it**, which is a path here where it is a coordinate for a
 * dependency. Both are the thing the person reading the message typed and can go and look at; the
 * package's own declared name is neither.
 *
 * A manifest that will not parse stops the build rather than being skipped. That matches what a
 * dependency's does, and the alternative is silently dropping a requirement on the one path this
 * whole check exists to close.
 */
private def libHeaderNeeds(roots: List[String]): Either[String, List[HeaderNeed]] =
  roots.foldLeft[Either[String, List[HeaderNeed]]](Right(Nil)) { (acc, root) =>
    for
      seen   <- acc
      config <- readPackageConfig(root)
    yield seen ::: config.headers.toList.sortBy(_._1).map((name, h) => HeaderNeed(root, name, h.why, h.env))
  }

/** What the `--lib` **source roots** declare they need an installed library for
 * (`reference/packages.md § Capabilities`).
 *
 * Read off the same manifest as `libHeaderNeeds`, on the same road and for the same reason: a package
 * handed over as a directory is the same package it is by coordinate, so what it needs of this
 * machine does not depend on how it got here.
 */
private def libPkgNeeds(roots: List[String]): Either[String, List[LibNeed]] =
  roots.foldLeft[Either[String, List[LibNeed]]](Right(Nil)) { (acc, root) =>
    for
      seen   <- acc
      config <- readPackageConfig(root)
    yield seen ::: config.pkgConfig.toList.sortBy(_._1).map((mod, why) => LibNeed(root, mod, why))
  }

/** What this machine says about the libraries the packages named, or the one line the build stops
 * on (`reference/packages.md § Capabilities`, `PkgConfig`).
 *
 * ==Asked only for the host==
 *
 * `pkg-config` answers for the machine it runs on. A cross build's headers and archives are the
 * *target's*, and there is no sense in which this machine's `/opt/homebrew` belongs in a compilation
 * for a Cortex-M — so a target that is not this machine probes nothing and the declaration falls back
 * to being answered by the flags, which is where a cross build's paths were always going to come
 * from. Silence here would be the worst outcome available: a freestanding program that compiled
 * against the host's headers would link, and be wrong somewhere nobody can see.
 *
 * ==A supplied name is an answer, and stops the probe==
 *
 * `--include-path <name>=<dir>` satisfies this exactly as it satisfies a header requirement, and it
 * takes precedence over anything a probe would have found. That is what keeps a hermetic build, a
 * hand-built prefix and a machine with a broken `.pc` from being hostage to what happens to be
 * installed — and it is the reason this can be added at all without any build that works today
 * changing what it does.
 *
 * ==One at a time, and the two failures are told apart==
 *
 * The first unanswered requirement stops the build, as `unmetHeaders` does. The sentence differs by
 * *why* it could not be answered, because the two send the reader to different places: a machine with
 * no `pkg-config` is one `brew install pkgconf` away and has nothing to do with the library, where a
 * `pkg-config` that does not know the module means the library itself is not installed.
 */
private def probeLibs(needs: List[LibNeed], supplied: Set[String], target: Target,
                      verbose: Boolean): Either[String, SearchPaths] = {
  val wanted = needs.filterNot(n => supplied.contains(n.module))

  if wanted.isEmpty then Right(SearchPaths())
  else if !Target.host.contains(target) then
    Left(s"${wanted.head.who} needs the '${wanted.head.module}' library and this is a build for " +
      s"'${target.name}' rather than for this machine, so there is nothing to ask where it is — " +
      s"${wanted.head.why}. Say where it is with '--include-path ${wanted.head.module}=<dir>' and " +
      "'--link-path <dir>'")
  else
    wanted.foldLeft[Either[String, SearchPaths]](Right(SearchPaths())) { (acc, need) =>
      for
        so_far <- acc
        answer <- PkgConfig.query(need.module).left.map { why =>
                    s"${need.who} needs the '${need.module}' library and $why — ${need.why}. " +
                      (if PkgConfig.available then
                         s"Install it, or say where it is with '--include-path ${need.module}=<dir>' " +
                           "and '--link-path <dir>'"
                       else
                         "Install pkg-config and sysl will ask it where the library is — 'brew " +
                           "install pkgconf', or your system's package of that name — or say where " +
                           s"it is with '--include-path ${need.module}=<dir>' and '--link-path <dir>'")
                  }
      yield
        if verbose then
          trace(s"pkg-config ${need.module}: ${(answer.cflags ::: answer.ldflags).mkString(" ")}")

        so_far.copy(probed = so_far.probed ::: answer.cflags,
                    probedLibs = so_far.probedLibs ::: answer.ldflags)
    }
}

/** This project's own header requirements, in the order they were declared.
 *
 * Named rather than inlined because two things ask for them and they must agree: the refusal, and
 * the environment-variable resolution that runs just before it. A resolution over a different list
 * from the check would answer a requirement nothing was going to ask about, or miss one that was.
 */
private def ownHeaderNeeds(project: PackageConfig): List[HeaderNeed] =
  project.headers.toList.sortBy(_._1).map((name, h) => HeaderNeed("this project", name, h.why, h.env))

/** The header requirements nothing on this command line answered, as the one line a build stops on
 * (`reference/packages.md § Capabilities`).
 *
 * ==Why the message is this long==
 *
 * Every other requirement in the file is answered by something the reader already has: a capability
 * is provided by the target or it is not, and there is nothing to go and do. This one is answered by
 * a path on a machine the package has never seen, so the reader is being asked to find something —
 * and a refusal that only names what is missing leaves them to work out *what* it is, *where* it
 * lives and *how* to say so. All three are known here: the package wrote the first two down, and the
 * third is the flag this very check reads.
 *
 * The package's own prose is quoted rather than paraphrased. It is the only part of this nothing in
 * the compiler could have written, and it is the part that says which library is meant.
 *
 * ==One at a time, in the order they were declared==
 *
 * The first unmet requirement stops the build, as `requires` already does for a capability. A
 * consumer satisfying them one flag at a time is the same walk either way, and a list of four would
 * be four things to look up before anything can be tried.
 */
private def unmetHeaders(project: PackageConfig, fromPackages: List[HeaderNeed],
                         supplied: Set[String]): Option[String] = {
  val need = ownHeaderNeeds(project) ::: fromPackages

  need.find(n => !supplied.contains(n.name)).map { n =>
    // **Where the package named a variable, that is the thing to say first.** Setting it is done
    // once and answers every build in that tree; the flag is what somebody types every time. A
    // reader who has arrived here has neither, so they are owed both and in that order.
    val how = n.env match
      case Some(v) =>
        s"Set $v, which is where this package's own ecosystem looks for it, or say where they are " +
          s"with '--include-path ${n.name}=<dir>'"
      case None =>
        s"Say where they are with '--include-path ${n.name}=<dir>'"

    s"${n.who} needs the '${n.name}' headers and nothing supplied them — ${n.why}. $how"
  }
}

/** The header requirements answered by an environment variable the package named, as the include
 * paths a `--include-path` would have added.
 *
 * ==Why this is not reading the consumer's shell==
 *
 * `reference/packages.md § No build scripts, ever` refuses a build that behaves differently because
 * of the environment it was started in, and this does not do that. **A variable is consulted only
 * where nothing on the command line named that requirement**, and where it is unset the build stops
 * with the refusal it stopped with before — so the environment can turn a failure into a success and
 * can never turn one success into a different one. An explicit flag wins, always, which is what
 * makes a one-off override possible.
 *
 * ==An empty variable is unset==
 *
 * `PICO_SDK_PATH=` in a shell profile is somebody clearing it, not somebody naming the root
 * directory. Treating it as a path would hand clang an empty `-I` and fail somewhere with no
 * connection to the variable.
 */
private def envHeaders(needs: List[HeaderNeed], supplied: Set[String],
                       lookup: String => Option[String] = envVar): List[(String, String)] =
  needs
    .filterNot(n => supplied.contains(n.name))
    .flatMap(n => n.env.flatMap(lookup).map(_.trim).filter(_.nonEmpty).map(n.name -> _))
    .distinctBy(_._1)

/** Whether a subcommand is producing something a **C project** links, which is what decides that
 * the module is emitted with no entry point (`reference/ffi.md § @export`).
 *
 * Both commands here are one compilation with two things read off it, which is why they share this
 * rather than each answering for itself: `emit-header` prints what `build-c` writes beside the
 * archive, and a header describing a different compilation from the archive it sits next to is the
 * one failure a C project cannot diagnose.
 */
private def cLibrary(command: String): Boolean = command == "build-c" || command == "emit-header"

/** The standard module this compilation gets, or why it has none.
 *
 * Two places, and they are governed by different rules. **A named one is taken as it is**: someone
 * who wrote `--std-lib` down is owed an error when what they named is not there or will not read,
 * rather than a different standard module built underneath them. That is the rule `Toolchain.findAr`
 * applies to a named archiver, and for the same reason.
 *
 * **The one at the default path is a cache, and is rebuilt when it is not usable.** See
 * `Stdlib.resolve`.
 *
 * `--no-std-lib` is the one way to the library **as source**, with no artifact in between. It is
 * what bootstrap needs — there is no released sysl to build the first artifact with — and what the
 * compiler's own unit tests take, running as they do in a tree where nothing has been built.
 * Reaching it is a deliberate act, and that is worth keeping distinct from the rebuild below: this
 * one compiles the library's source into the program, where the rebuild produces the artifact and
 * links it. Both read the same files off disk (`Std.root`); what differs is what they do with them.
 *
 * **`build-lib --std` is exempt, and has to be.** It is the command that produces the artifact, so
 * consulting one would be a deadlock with nothing to break it.
 */
/** Which standard module this command line asks for.
 *
 * The whole of what the driver contributes: the finding, the rebuilding and the checking are
 * `Stdlib.resolve`'s, because they are not properties of a command line — a test suite and a
 * documentation harness need the same answer to the same question, and this is the only place that
 * knows about `Config`.
 *
 * `build-lib --std` takes the source, and has to: it is the command that *produces* the artifact, so
 * consulting one would be a deadlock with nothing to break it.
 *
 * **`build-c` takes the source too, and for a reason that is about its consumer rather than about
 * this compilation.** What it writes is linked by a C project, and an artifact is only an artifact if
 * that link succeeds — a `.syslib` cannot go on a C link line, so an archive referring to one is
 * unusable by the only tool that was ever going to read it. Folding the library in is what
 * `--no-std-lib` asks for everywhere else; here there is nothing else to ask for.
 */
private def stdChoice(cfg: Config, target: Target): Stdlib.Choice =
  // **A machine with no C toolchain takes the source, and has no choice about it.** An artifact is
  // an *archive of objects* — the cache path compiles the library's IR with clang and archives it —
  // so on a target with no object format, no archiver and no clang there is nothing an artifact
  // could be. Compiling the library's source into the program is the same road bootstrap takes, for
  // the same reason: it is the one that needs no toolchain at all.
  if !target.buildsWithClang then Stdlib.Choice.FromSource
  else if cfg.noStdLib || cfg.std || cLibrary(cfg.command) then Stdlib.Choice.FromSource
  else
    cfg.stdLib match
      case Some(named) => Stdlib.Choice.Artifact(named)
      case None        => Stdlib.Choice.Default(cfg.stdSearch)


/** Which machine this invocation is for: the one it names, the one the project config names, or this
 * one. A machine sysl has no entry for is reported rather than guessed at — the guess would be a
 * module that looks right and is built for something else.
 *
 * `--target` beats the config, which beats the host. That is the order every other tool uses and the
 * only one that lets a project with a default still be cross-built from the command line without
 * editing a file.
 */
private def chooseTarget(named: Option[String], configured: Option[String]): Either[String, Target] =
  named.orElse(configured) match
    case Some(name) => Target.named(name)
    case None =>
      Target.host.toRight(
        "this machine is not one sysl knows, so a build has to name its target with --target " +
          "('sysl targets' lists them)")

/** Where a build writes when the caller named no output.
 *
 * A **file** project is named by a path carrying an extension, so dropping it leaves a name that
 * cannot be the thing being built: `foo.sysl` becomes `foo`, beside the caller.
 *
 * A **directory** project has no extension to drop, and the name left over *is* the directory —
 * so the linker was handed a path it could not open, and `sysl build .` failed for every project
 * there has ever been while `sysl build test` failed for every project sitting in the working
 * directory. A directory has somewhere obvious to put the thing built out of it, which is inside
 * itself, so that is where it goes.
 *
 * **The point is that the answer stops depending on where the build was started.** `sysl build .`,
 * `sysl build test` and `sysl build ../test` are three ways of naming one project, and all three now
 * write `test/test` rather than three different paths, one of which was the project.
 *
 * **`named` is `package.name` where the project wrote one**, and it answers the question of what a
 * directory project *is*: today a directory is a project because it happens to hold `.sysl` files,
 * and nothing gives one an identity of its own. Requiring a `package.hocon` would give every project
 * one and cost every project the ceremony — 15 directories in this repo have none, and neither does
 * a scratch directory anybody makes in thirty seconds. So a bare directory goes on building and is
 * named for itself, and a project that wants to be called something says so.
 *
 * A **file** project is deliberately left out of this. Its name comes from a path the caller typed,
 * and a config sitting beside it quietly moving `foo.sysl`'s executable would be a worse surprise
 * than the thing this exists to fix.
 */
private def defaultOutput(file: String, named: Option[String], suffix: String = ""): String =
  if isDirectory(file) then s"$file/${named.getOrElse(Project.nameOf(file))}$suffix"
  else defaultOutputName(file) + suffix

/** The name a **file** project's output takes: its own, with the extension dropped. */
private def defaultOutputName(file: String): String = {
  val name = Project.basename(file)
  val dot  = name.lastIndexOf('.')
  val base = if dot > 0 then name.substring(0, dot) else name
  if base.isEmpty then "a.out" else base
}

/** What `--verbose` says, on stderr so that it never lands in what a build was for.
 *
 * It is prefixed rather than bare because these lines arrive in the middle of whatever else the
 * terminal is doing, and a reader needs to know which of the programs in front of them is talking.
 */
private[sysl] def trace(what: String): Unit = Console.err.println(s"sysl: $what")

/** A driver failure — something that went wrong around the compiler rather than inside it. */
private def fail(msg: String): Int = {
  Console.err.println(s"sysl: error: $msg")
  1
}

/** A diagnostic from the compiler, which already renders itself with its location and a caret
 * under the offending column, so it is printed exactly as it came.
 */
private def report(diagnostic: String): Int = {
  Console.err.println(diagnostic)
  1
}
