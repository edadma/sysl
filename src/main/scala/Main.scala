package xyz.hyperreal.sysl

import java.io.{File, FileOutputStream, PrintStream}

object Main extends App {

  case class Options(
      parser: Boolean = false,
      gen: Boolean = false,
      run: Boolean = false,
      files: List[File] = Nil,
      out: File = null
  )

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
      .text("source file(s) to compile")
    opt[Unit]('g', "gen")
      .action((_, c) => c.copy(gen = true))
      .text("run code generator")
    help("help").text("print this usage text").abbr("h")
    opt[Unit]('p', "parser")
      .action((_, c) => c.copy(parser = true))
      .text("run parser")
    opt[Unit]('r', "run")
      .action((_, c) => c.copy(run = true))
      .text("run bitcode interpreter")
    opt[File]('o', "out")
      .optional()
      .valueName("<executable file>")
      .action((x, c) => c.copy(out = x))
      .validate(
        x =>
          if (!x.exists || x.canWrite)
            success
          else
            failure(s"Option --out: can't write to $x"))
      .text("optional executable output file")
    version("version")
      .text("print the version")
      .abbr("v")
  }

  optionsParser.parse(args, Options()) match {
    case Some(options) =>
      if ((options.parser | options.gen | options.run) && options.out != null) {
        optionsParser.showUsageAsError
        optionsParser.reportError("Option --out is only for compiler output")
        sys.exit(1)
      } else if (options.parser) {
        println(parse(options.files))
      } else if (options.gen) {
        println(generate(options.files))
      } else if (options.run) {
        interp(options.files)
      } else if (options.out != null)
        executable(options.out, options.files)
      else
        executable(new File("executable"), options.files)
    case None => sys.exit(1)
  }

  def parse(files: List[File]) = {
    def parse(in: File) = {
      val parser = new SyslParser

      parser.parseFromSource(io.Source.fromFile(files.head), parser.source)
    }

    files map parse
  }

  def generate(files: List[File]) = {
    CodeGenerator(parse(files))
  }

  def interp(files: List[File]) = {}

  def executable(out: File, files: List[File]) = {
    val s = new PrintStream(new FileOutputStream(out))

    s.print(generate(files))
    s.close
  }

//  def time(block: => Unit) = {
//    val start = System.currentTimeMillis
//
//    block
//    println(f"\nCompleted in ${(System.currentTimeMillis - start) / 1000.0}%.3fs")
//  }
}
