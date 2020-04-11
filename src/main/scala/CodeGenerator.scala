package xyz.hyperreal.sysl

object CodeGenerator {

  def apply(as: List[SourceAST]) = {
    val out = new StringBuilder

    def line(s: String, indent: Int = 0) = {
      out ++= "  " * indent
      out ++= s
      out += '\n'
    }

    compileSource(as.head)

    def compileSource(src: SourceAST) = {
      src.stmts map compileStatement
    }

    def compileStatement(stmt: StatementAST): Unit =
      stmt match {
        case t: ExpressionAST           =>
        case DeclarationBlockAST(decls) => decls foreach compileStatement
        case DefAST(dpos, name, FunctionPieceAST(ppos, parms, arb, parts, where)) =>
          line("define i32 @" ++ name ++ "() {")
          compileExpression(parts.head.body, 0)
          line("ret i32 0", 1)
          line("}")
      }

    def compileExpression(expr: ExpressionAST, start: Int): Int =
      expr match {
        case BlockExpressionAST(stmts) =>
      }
  }

}
