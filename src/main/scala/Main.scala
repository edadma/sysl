package xyz.hyperreal.sysl

object Main extends App {

  case class Options(
      source: Boolean = false,
      lexer: Boolean = false,
      parser: Boolean = false,
      interp: Boolean = false,
      gen: Boolean = false,
      run: Boolean = false
  )

  private val optionsParser = new scopt.OptionParser[Options]("rosettacodeCompiler") {
    head("Rosetta Code Compiler", "v0.1")
    opt[Unit]('g', "gen")
      .action((_, c) => c.copy(gen = true))
      .text("run code generator (only unless -s is used)")
    help("help").text("print this usage text").abbr("h")
    opt[Unit]('i', "interp")
      .action((_, c) => c.copy(interp = true))
      .text("run interpreter (only unless -s is used)")
    opt[Unit]('l', "lexer")
      .action((_, c) => c.copy(lexer = true))
      .text("run lexer only")
    opt[Unit]('p', "parser")
      .action((_, c) => c.copy(parser = true))
      .text("run parser (only unless -s is used)")
    opt[Unit]('r', "run")
      .action((_, c) => c.copy(run = true))
      .text("run using virtual machine (only unless -s is used)")
    opt[Unit]('s', "source")
      .action((_, c) => c.copy(source = true))
      .text("run all prior steps")
    version("version").text("print the version").abbr("v")
  }

  optionsParser.parse(args, Options()) match {
    case Some(options) =>
//      if (options.lexer)
//        LexicalAnalyzer.apply.fromStdin
//      else if (options.parser && options.source)
//        SyntaxAnalyzer.apply.fromString(capture(LexicalAnalyzer.apply.fromStdin))
//      else if (options.parser)
//        SyntaxAnalyzer.apply.fromStdin
//      else if (options.gen && options.source)
//        CodeGenerator.fromString(capture(SyntaxAnalyzer.apply.fromString(capture(LexicalAnalyzer.apply.fromStdin))))
//      else if (options.gen)
//        CodeGenerator.fromStdin
//      else if (options.run && options.source)
//        time(
//          VirtualMachine
//            .fromString(capture(CodeGenerator.fromString(
//              capture(SyntaxAnalyzer.apply.fromString(capture(LexicalAnalyzer.apply.fromStdin))))))
//            .run)
//      else if (options.run)
//        time(VirtualMachine.fromStdin.run)
//      else if (options.interp && options.source || !options.interp && !options.source)
//        time(
//          ASTInterpreter.fromString(capture(SyntaxAnalyzer.apply.fromString(capture(LexicalAnalyzer.apply.fromStdin)))))
//      else if (options.interp)
//        time(ASTInterpreter.fromStdin)
//      else if (options.source) {
//        optionsParser.showUsageAsError
//        optionsParser.reportError("-s should be used with one of the compiler stage options")
//      }
    case None => sys.exit(1)
  }

  def time(block: => Unit) = {
    val start = System.currentTimeMillis

    block
    println(f"\nCompleted in ${(System.currentTimeMillis - start) / 1000.0}%.3fs")
  }
}
