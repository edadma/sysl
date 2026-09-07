package sh.sysl

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

/** The default `GC_MAXIMUM_HEAP_SIZE` this compiler gives its own GC — see `ensureHeapCeiling` in
 * `platform.scala`, right beside this file, the only place any of this matters.
 *
 * **Native-only by directory, not by an `assume` inside a shared spec.** `heapCeilingDecision` and
 * `DefaultMaxHeapSize` are declared in this platform's own `platform.scala` and are never asked for
 * on JVM or JS — there is no ceiling for either of them to give itself, as their own `platform.scala`
 * says — and the subprocess half below reaches for `java.io.File` and `ProcessBuilder` against a
 * real filesystem and a real child process, neither of which Scala.js's `javalib` provides. A shared
 * spec gated by `assume(platform == "native", ...)` would still have to *compile* on JS, which these
 * two calls cannot; a native-only source set is what `SubcommandTests`' commentary already points at
 * ("no JVM can [change its own PATH] ... so the seam is tested from both sides instead") without
 * quite reaching for.
 */
class HeapCeilingTests extends AnyFreeSpec with Matchers {

  "the decision of what ceiling to set" - {
    "chooses the default when the caller left the ceiling unset" in {
      heapCeilingDecision(None) shouldBe Some(DefaultMaxHeapSize)
    }

    "leaves a caller's own ceiling alone, whatever it says" in {
      heapCeilingDecision(Some("2g")) shouldBe None
      heapCeilingDecision(Some("512m")) shouldBe None
    }

    "leaves it alone even where the caller happened to choose the same number" in {
      // Not a distinction `getenv` could make in the first place — a value is a value, whatever
      // wrote it — but worth pinning, since "equal to the default" is the one case a sloppier
      // comparison (by identity, say) could get backwards.
      heapCeilingDecision(Some(DefaultMaxHeapSize)) shouldBe None
    }
  }

  "the real mechanism, exercised through the linked binary" - {
    // `execv` replaces the process image in place, so there is no parent left afterward to ask
    // "what did the child's environment end up being" — the only place that answer is still
    // observable is a *grandchild*, spawned once the re-exec has already happened. `sysl`'s own
    // external-subcommand dispatch (`SubcommandTests`) is exactly that seam: an unrecognized word
    // is looked for on the PATH and run with this process's environment, which by then is whatever
    // `ensureHeapCeiling` decided. A shell script standing in for `sysl-probe` reads it back without
    // needing a single line of sysl compiled — which matters here specifically, since compiling
    // anything at all now has its own, much larger, memory floor (see the CLAUDE.md heap-ceiling
    // sections) that a unit test has no business depending on.
    def linkedBinary: Option[File] = {
      val target   = new File("native/target")
      val scalaDir = Option(target.listFiles())
        .flatMap(_.find(f => f.isDirectory && f.getName.startsWith("scala-")))

      scalaDir.map(new File(_, "sysl")).filter(_.isFile)
    }

    def probeOutput(binary: File, clearCeiling: Boolean, ceiling: Option[String]): String = {
      val probeDir = java.nio.file.Files.createTempDirectory("sysl-heap-probe").toFile
      val script   = new File(probeDir, "sysl-probe")

      java.nio.file.Files.writeString(script.toPath, "#!/bin/sh\necho \"$GC_MAXIMUM_HEAP_SIZE\"\n")
      script.setExecutable(true)

      val builder = new ProcessBuilder(binary.getAbsolutePath, "probe")
      val env     = builder.environment()

      env.put("PATH", probeDir.getAbsolutePath + File.pathSeparator + env.get("PATH"))
      if clearCeiling then env.remove("GC_MAXIMUM_HEAP_SIZE")
      ceiling.foreach(env.put("GC_MAXIMUM_HEAP_SIZE", _))

      val proc = builder.start()
      val out  = new String(proc.getInputStream.readAllBytes(), "UTF-8").trim

      proc.waitFor()
      out
    }

    "the shipped binary sets its own default when the caller gave it none" in {
      val binary = linkedBinary

      assume(binary.isDefined,
        "requires a linked native binary — run 'sbt syslNative/nativeLink' first")

      probeOutput(binary.get, clearCeiling = true, ceiling = None) shouldBe DefaultMaxHeapSize
    }

    "a ceiling the caller already set reaches the child unchanged" in {
      val binary = linkedBinary

      assume(binary.isDefined,
        "requires a linked native binary — run 'sbt syslNative/nativeLink' first")

      probeOutput(binary.get, clearCeiling = false, ceiling = Some("222m")) shouldBe "222m"
    }
  }
}
