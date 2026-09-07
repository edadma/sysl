import xerial.sbt.Sonatype.sonatypeCentralHost

ThisBuild / licenses               := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))
ThisBuild / versionScheme          := Some("semver-spec")
ThisBuild / evictionErrorLevel     := Level.Warn
ThisBuild / scalaVersion           := "3.8.4"
ThisBuild / organization           := "sh.sysl"
ThisBuild / organizationName       := "sysl-lang"
ThisBuild / organizationHomepage   := Some(url("https://github.com/sysl-lang"))
ThisBuild / version                := "0.0.108"
ThisBuild / sonatypeCredentialHost := sonatypeCentralHost

ThisBuild / publishConfiguration := publishConfiguration.value.withOverwrite(true).withChecksums(Vector.empty)
ThisBuild / resolvers += Resolver.mavenLocal
ThisBuild / resolvers += Resolver.sonatypeCentralSnapshots
ThisBuild / resolvers += Resolver.sonatypeCentralRepo("releases")

ThisBuild / sonatypeProfileName := "sh.sysl"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/sysl-lang/sysl"),
    "scm:git@github.com:sysl-lang/sysl.git",
  ),
)
ThisBuild / developers := List(
  Developer(
    id = "edadma",
    name = "Edward A. Maxedon, Sr.",
    email = "edadma@gmail.com",
    url = url("https://github.com/edadma"),
  ),
)

ThisBuild / homepage := Some(url("https://github.com/sysl-lang/sysl"))
ThisBuild / description := "A modern, ref-counted, OS-level systems language — easier than Rust."

// Where a publish goes, and there are two answers because Central is slow.
//
// Central is the real one: it is what anybody depending on sysl resolves from, and a release is not
// a release until it is there. But it can take hours to propagate, and `~/dev/sysl.sh` — which
// compiles every fenced block on every page against a *released* compiler — cannot write a line of
// documentation for a release until it can resolve one. GitHub Packages answers in minutes, so the
// same artifacts go there too and the site can be written while Central catches up.
//
// Gated on an environment variable rather than a separate task, matching `SYSL_RELEASE`: one
// `publishTo`, switched, so there is no second configuration to drift.
//
//     sbt publishSigned                      # Central, 1 of 2 — stages locally, uploads nothing
//     sbt sonatypeBundleRelease              # Central, 2 of 2 — the upload, and the release
//     SYSL_PUBLISH_GITHUB=1 sbt publish      # GitHub Packages, the head start
//
// `publishSigned` alone publishes nothing: it writes the signed artifacts under
// `target/sonatype-staging/` and prints a `published … to <local path>` line for each, then exits
// successfully in about a second. `sonatypeBundleRelease` is what uploads the bundle and polls until
// the deployment reports `PUBLISHED`.
//
// **This does not shorten the release and must never be allowed to stand in for it.** A GitHub
// Packages copy makes a downstream build succeed whether or not the Central upload worked, which is
// the same masking that rules out `publishLocal` — so the Central check against `maven-metadata.xml`
// stays exactly as mandatory as it was.
ThisBuild / publishTo := {
  if (sys.env.contains("SYSL_PUBLISH_GITHUB"))
    Some("GitHub Packages" at "https://maven.pkg.github.com/sysl-lang/sysl")
  else
    sonatypePublishToBundle.value
}

// GitHub Packages authenticates every request, including reads of a public package, so both halves
// of this need a token. Kept out of the build for the obvious reason.
//
// Two places to find one, because the two machines that run this keep it differently. A workstation
// has a file; **CI has `GITHUB_TOKEN` and cannot have a file**, which is the case that matters —
// the whole point of publishing here is the window where Central has not propagated, and that is
// exactly when a build with no credentials would fail to resolve.
ThisBuild / credentials ++= githubCredentials

// The file wins where it exists, so a workstation that has set one up behaves as it did before this
// was added, and `GITHUB_TOKEN` cannot quietly take over from a token somebody chose on purpose.
lazy val githubCredentials: Seq[Credentials] = {
  val f = Path.userHome / ".sbt" / "github-credentials"

  if (f.exists)
    Seq(Credentials(f))
  else
    sys.env.get("GITHUB_TOKEN").toSeq.map { token =>
      // Any username authenticates against a valid token; CI's own actor is the honest one to send.
      Credentials(
        "GitHub Package Registry",
        "maven.pkg.github.com",
        sys.env.getOrElse("GITHUB_ACTOR", "edadma"),
        token,
      )
    }
}

// The version, carried into the compiler so that `sysl --version` can answer with it.
//
// Generated rather than hand-kept beside `ThisBuild / version`, because two places holding one fact
// drift, and this one drifts *silently* — a binary that reports the
// version before last is worse than one that reports none, since the whole use of the flag is telling
// which build somebody has when they report something.
lazy val embedVersion = Def.task {
  val utf8 = java.nio.charset.StandardCharsets.UTF_8
  val out  = (Compile / sourceManaged).value / "sh" / "sysl" / "BuildInfo.scala"
  val text =
    s"""package sh.sysl
       |
       |/** Generated from `version` by `build.sbt` -- do not edit. */
       |private[sysl] object BuildInfo {
       |  val version: String = "${version.value}"
       |}
       |""".stripMargin

  if (!out.exists || IO.read(out, utf8) != text) IO.write(out, text, utf8)
  Seq(out)
}

// The TextMate grammar `weave` highlights sysl with, compiled into the binary rather than read off
// disk beside `library/`.
//
// **It lives here because `GrammarTests` does**, and that test is the only thing standing between the
// grammar and the language drifting apart -- it reconciles the grammar against `SyslLexical`, which
// is in this tree. The grammar was in `sysl.sh` until 0082, where the test could see it but not the
// lexer it is a claim about; the site now fetches it the same way its CI already fetches `library/`.
//
// Compiled in rather than staged because it is 9 KB and because a `weave` that cannot find its
// grammar is a failure mode worth not having. The library is read off disk for the opposite reason:
// it is large, and people edit it.
lazy val embedGrammar = Def.task {
  val utf8    = java.nio.charset.StandardCharsets.UTF_8
  val out     = (Compile / sourceManaged).value / "sh" / "sysl" / "Grammar.scala"
  val grammar = IO.read((ThisBuild / baseDirectory).value / "grammars" / "sysl.tmLanguage.json", utf8)

  // Escaped into an ordinary string literal rather than set in triple quotes: the grammar is full of
  // regex backslashes, and a `"""` block would hand them to the reader intact only until the day one
  // of its patterns ends in a quote.
  val escaped = grammar.flatMap {
    case '\\' => "\\\\"
    case '"'  => "\\\""
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case c    => c.toString
  }

  val text =
    s"""package sh.sysl
       |
       |/** Generated from `grammars/sysl.tmLanguage.json` by `build.sbt` -- do not edit. */
       |private[sysl] object Grammar {
       |  val sysl: String = "$escaped"
       |}
       |""".stripMargin

  if (!out.exists || IO.read(out, utf8) != text) IO.write(out, text, utf8)
  Seq(out)
}

lazy val sysl = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(
    name := "sysl",
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        // Dead code is only dead once something says so. Without this the build is silent about an
        // unused import, local, private member, or parameter, and a warning sweep before a release
        // has nothing to find. `-Wvalue-discard` and `-Wnonunit-statement` are deliberately *not*
        // here: between them they flag every non-final ScalaTest assertion, ~285 of them, which
        // would bury the handful of warnings that mean something.
        "-Wunused:all",
        "-language:postfixOps",
        "-language:implicitConversions",
        "-language:existentials",
        "-language:dynamics",
      ),
    Compile / sourceGenerators += embedVersion.taskValue,
    Compile / sourceGenerators += embedGrammar.taskValue,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.20" % "test",
    libraryDependencies ++= Seq(
      "com.github.scopt"         %%% "scopt"                    % "4.1.0",
      "org.scala-lang.modules"   %%% "scala-parser-combinators" % "2.4.0",
      // Off-side-rule lexer base — what emits the indent and dedent tokens the layout rules in
      // `SyslLexical` are built on.
      "io.github.edadma"         %%% "indentation"              % "0.0.10",
      // Cross-platform I/O boundary: the compiler cross-publishes to JVM, JS and Native, so every
      // file and environment read goes through these rather than through `java.io`.
      "io.github.edadma"         %%% "path"                     % "0.0.8",
      "io.github.edadma"         %%% "cross_platform"           % "0.1.9",
      // The project config's format — `package.hocon`, which `PackageConfig` reads.
      "io.github.edadma"         %%% "hocon"                    % "0.1.2",
      // What `weave` renders a literate source with. The prose of a '.lsysl' file is Markdown
      // already, and `indentedCodeLanguage` is what carries the one thing the format gives up: a
      // program marked by an indent and nothing else reaches the highlighter knowing its language.
      "io.github.edadma"         %%% "markdown"                 % "0.4.7",
      // Turns the TextMate grammar into the spans the woven document's stylesheet colours, so a
      // reader needs no JavaScript for the code.
      "io.github.edadma"         %%% "highlighter"              % "0.0.11",
//      "com.lihaoyi" %%% "pprint" % "0.9.6" % "test",
    ),
    publishMavenStyle      := true,
    Test / publishArtifact := false,
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
    // Hands ScalaTest a thread pool, which is what a suite mixing in `ParallelTestExecution`
    // needs before it can run its tests at the same time. Without it the mixin is not an error
    // and not a warning — it simply has nowhere to distribute to and runs everything in order,
    // which is how the documentation suite came to spend six minutes doing four hundred
    // compilations on one core while seventeen sat idle. That suite has moved to the site's own
    // repository and this setting stays, because the suites left here mix in the same thing.
    //
    // JVM-only on purpose: the JS and Native runners are single-threaded, so the two suites that
    // ask for parallelism there get the sequential behaviour they had before rather than an
    // argument their runner has no use for.
    // The thread count is explicit because sbt refuses a bare `-P` — it will not infer one on
    // ScalaTest's behalf. What these threads do is wait on `clang` and on the linker, so the
    // count is the machine's rather than a fraction of it: the work is in the child processes.
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-P18"),
  )
  .nativeSettings(
//    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.7.0",
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
    // The binary that gets installed is built in release mode; the one built while developing is
    // not. Scala Native defaults to debug, and that default is the right one here — a link is
    // seconds rather than minutes, and nothing about this project is iterated on from Native
    // anyway (the JVM build is the development loop). But an installed compiler is run by people
    // who did not build it, and shipping the unoptimized link would make sysl look slow for a
    // reason that has nothing to do with sysl.
    //
    // Gated on the environment rather than made the default, so that asking for the slow, careful
    // link is a deliberate act performed when cutting a release: `SYSL_RELEASE=1 sbt
    // syslNative/nativeLink`.
    nativeConfig ~= { c =>
      if (sys.env.get("SYSL_RELEASE").contains("1"))
        c.withMode(scala.scalanative.build.Mode.releaseFast)
          .withLTO(scala.scalanative.build.LTO.thin)
      else c
    },
  )
  .jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    //  scalaJSLinkerConfig ~= { _.withModuleSplitStyle(ModuleSplitStyle.SmallestModules) },
    scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    //    Test / scalaJSUseMainModuleInitializer := true,
    //    Test / scalaJSUseTestModuleInitializer := false,
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    scalaJSUseMainModuleInitializer        := true,
//    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.7.0",
  )

// ===== sysl-doc: the API reference generator, as its own binary =====
//
// A separate binary rather than a `doc` subcommand, which is what card 0257's git-style dispatch was
// built for: `sysl doc` execs `sysl-doc` off the PATH. Every major toolchain ships its doc generator
// this way — scaladoc beside scalac, rustdoc beside rustc, javadoc beside javac — because the doc
// tool's dependency profile has nothing to do with the compiler's. This one links a static site
// generator, a templating engine, an asset pipeline and a web server; putting that inside the
// compiler is the artifact nobody wants.
//
// **So the compiler must NOT grow a `doc` subcommand.** A built-in wins the dispatch and this binary
// would never be reached. What lives in the compiler is `sh.sysl.doc`, the generator itself, because
// it needs the AST and carries no dependency of its own; what lives here is the command line and the
// site build.
//
// JVM and Native only. There is no use for a JS build of a command-line tool, and juicer's JS target
// has not linked since well before 0.3.0 — depending on it would import that problem for nothing.
lazy val syslDoc = crossProject(JVMPlatform, NativePlatform)
  .in(file("doc"))
  .dependsOn(sysl)
  .settings(
    name := "sysl-doc",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Wunused:all",
      "-language:postfixOps",
      "-language:implicitConversions",
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.2.20" % "test",
      // The site generator as a library — the SiteBuild pipeline without juicer's own option parser.
      // Split out and published for this consumer; see juicer 0.4.0.
      //
      // **0.4.1 IS A FLOOR RATHER THAN A CURRENT VERSION.** `MarkdownWriter` writes `slugStyle:
      // github` into every generated page's frontmatter, and a per-page `slugStyle` is exactly what
      // 0.4.1 added — 0.4.0 ignores the key and slugs with the site's default, which puts every
      // anchor on every generated page at the top of the right page. The pin sat at 0.4.0 for a
      // release with the frontmatter already being written, so `sysl doc --site` rendered dead links
      // and nothing said so. `SlugConformanceTests` is what notices now, and it only notices because
      // it renders through this jar.
      "io.github.edadma" %%% "juicer-core" % "0.4.2",
    ),
    // Not published. It is a binary somebody installs, exactly as the compiler is, and a coordinate
    // for it would be one nothing should depend on.
    publish / skip      := true,
    publishLocal / skip := true,
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
  )
  .nativeSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
    // Same gate as the compiler's, and for the same reason: the installed binary is built in release
    // mode and the one built while developing is not.
    nativeConfig ~= { c =>
      if (sys.env.get("SYSL_RELEASE").contains("1"))
        c.withMode(scala.scalanative.build.Mode.releaseFast)
          .withLTO(scala.scalanative.build.LTO.thin)
      else c
    },
  )

lazy val root = project
  .in(file("."))
  .aggregate(sysl.js, sysl.jvm, sysl.native, syslDoc.jvm, syslDoc.native)
  .settings(
    name                := "sysl",
    publish / skip      := true,
    publishLocal / skip := true,
  )
