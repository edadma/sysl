package xyz.hyperreal.sysl

object CodeGenerator {

  def apply(as: List[SourceAST]) = {
    val out = new StringBuilder

    def line(s: String) = {
      out ++= s
      out += '\n'
    }

    def indent(s: String) = {
      out ++= "  "
      line(s)
    }

    compileSource(as.head)

    def compileSource(src: SourceAST) =
      src.stmts foreach compileTopLevelStatement

    def compileTopLevelStatement(stmt: StatementAST): Unit =
      stmt match {
        case DeclarationBlockAST(decls) => decls foreach compileTopLevelStatement
        case DefAST(dpos, name, FunctionPieceAST(ppos, parms, arb, parts, where)) =>
          compileFunction(name, parms, arb, parts, where)
      }

    def compileFunction(name: String,
                        parms: List[PatternAST],
                        arb: Boolean,
                        parts: List[FunctionPart],
                        where: List[DeclarationStatementAST]) = {
      val counter = new Counter

      def operation(s: String) = indent(s"%${counter.next} = $s")

      def compileStatement(stmt: StatementAST): Unit =
        stmt match {
          case t: DeclarationStatementAST =>
          case t: ExpressionAST           => compileExpression(t)
        }

      def compileExpression(expr: ExpressionAST): Int = {
        expr match {
          case BlockExpressionAST(stmts)    => stmts foreach compileStatement
          case LiteralExpressionAST(v: Int) => operation(v.toString)
        }

        counter.current
      }

      line("define i32 @" ++ name ++ "() {")
      compileExpression(parts.head.body)
      indent("ret i32 0")
      line("}")
    }
  }

}

class Counter {

  var count = 0

  def next = {
    count += 1
    count
  }

  def current =
    if (count == 0)
      sys.error("no last count")
    else
      count - 1

}
