package xyz.hyperreal.sysl

import java.io.{File, FileOutputStream, PrintStream}

import scala.scalanative.native
import scala.scalanative.native._

object Main extends App {

  case class Options(
      parser: Boolean = false,
      gen: Boolean = false,
      opt: (String, String) = null,
      run: Boolean = false,
      files: List[File] = Nil,
      arch: String = null,
      out: File = null
  )

  val optRegex = "([0-3sz])(?:,([1-3]))?" r

  private val optionsParser = new scopt.OptionParser[Options]("sysl") {
    head("SysL Compiler", "v0.1.0")
    arg[File]("<source file>...")
      .unbounded()
      .action((x, c) => c.copy(files = c.files :+ x))
      .validate(
        x =>
          if (!x.exists)
            failure(s"not found: $x")
          else if (x.isFile && x.canRead)
            success
          else
            failure(s"unreadable: $x"))
      .text("source file(s) to compile (- refers to standard input)")
    opt[String]('a', "arch")
      .optional()
      .valueName("<arch>[,<cpuname>]")
      .action((x, c) => c.copy(arch = x))
      .text("target architecture")
    opt[Unit]('g', "gen")
      .action((_, c) => c.copy(gen = true))
      .text("run code generator")
    help("help").text("print this usage text").abbr("h")
    opt[Unit]('p', "parse")
      .action((_, c) => c.copy(parser = true))
      .text("run parser")
    opt[Unit]('r', "run")
      .action((_, c) => c.copy(run = true))
      .text("run bitcode interpreter")
    opt[String]('O', "opt")
      .optional()
      .valueName("<level>")
      .validate(
        x =>
          if (optRegex.pattern.matcher(x).matches)
            success
          else
            failure(s"Option --opt argument should match ([0-3sz])(?:,([1-3]))?"))
      .action((x, c) => c.copy(opt = x match { case optRegex(a, b) => (a, b) }))
      .text("optional optimization level")
    opt[File]('o', "out")
      .optional()
      .valueName("<output file>")
      .action((x, c) => c.copy(out = x))
      .validate(
        x =>
          if (!x.exists || x.canWrite)
            success
          else
            failure(s"Option --out: can't write to $x"))
      .text("optional output file")
    version("version")
      .text("print the version")
      .abbr("v")
  }

  optionsParser.parse(args, Options()) match {
    case Some(options) =>
      if (options.parser) {
        val asl = parse(options.files).head

        if (options.out ne null)
          write(asl.toString, options.out)
        else
          println(asl)
      } else if (options.gen) {
        val code = generate(options.files, options.opt)

        if (options.out ne null)
          write(code, options.out)
        else
          println(code)
      } else if (options.run) {
        interp(options.files)
      } else if (options.out ne null)
        executable(options.out, options.files, options.opt)
      else
        executable(new File("executable"), options.files, options.opt)
    case None => sys.exit(1)
  }

  def source(f: File) =
    if (f.toString == "-")
      io.Source.stdin
    else
      io.Source.fromFile(f)

  def parse(files: List[File]) = {
    def parse(in: File) = {
      val parser = new SyslParser

      parser.parseFromSource(source(files.head), parser.source)
    }

    files map parse
  }

  def generate(files: List[File], opt: (String, String)) = {
    val code = CodeGenerator(parse(files))

    if (opt eq null)
      code
    else {
      val f = temp(code, ".ll").toString
      val r = File.createTempFile("sysl-opt-", ".ll")

      system(s"opt -S --O${opt._1} $f >$r")

      val s   = io.Source.fromFile(r)
      val res = s.mkString

      s.close
      res
    }
  }

  def temp(s: String, suffix: String) = {
    val f = File.createTempFile("sysl-", suffix)

    write(s, f)
    f
  }

  def system(cmd: String): Unit =
    Zone { implicit z =>
      val s = toCString(cmd)

      libc.system(s) match {
        case c if c != 0 => sys.error(s"command failed with exit code $c: $cmd")
        case _           =>
      }
    }

  def interp(files: List[File]): Unit = {
    system(s"lli ${temp(generate(files, ("z", null)), ".ll")}")
  }

  def executable(out: File, files: List[File], opt: (String, String)): Unit = {
    val ll   = temp(generate(files, opt), ".ll").toString
    val name = ll.substring(0, ll.length - 3)

    system(s"llc -O=${if (opt != null && opt._2 != null) opt._2 else "1"} --filetype=obj $ll")
    system(s"gcc -no-pie ${name ++ ".o"} -o $out")
  }

  def write(s: String, f: File): Unit = {
    val st = new PrintStream(new FileOutputStream(f))

    st.print(s)
    st.close()
  }

  @native.extern
  object libc {
    def system(command: native.Ptr[CChar]): native.CInt = native.extern
  }

//  def time(block: => Unit) = {
//    val start = System.currentTimeMillis
//
//    block
//    println(f"\nCompleted in ${(System.currentTimeMillis - start) / 1000.0}%.3fs")
//  }
}
