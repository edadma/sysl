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

    def compileSource(src: SourceAST) = {
      line("""@.number_str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1""")
      line("declare i32 @printf(i8*, ...)")
      src.stmts foreach compileTopLevelStatement
    }

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

      def operation(s: String) = {
        indent(s"%${counter.next} = $s")
        counter.current
      }

      def compileStatement(stmt: StatementAST): Unit =
        stmt match {
          case t: DeclarationStatementAST =>
          case t: ExpressionAST           => compileExpression(t)
        }

      def compileExpression(expr: ExpressionAST): Int = {
        expr match {
          case BlockExpressionAST(stmts) => stmts foreach compileStatement
          case LiteralExpressionAST(v: Int) =>
            indent(s"store i32 $v, i32* %${operation("alloca i32, align 4")}, align 4")
            operation(s"load i32, i32* %${counter.current}, align 4")
          case VariableExpressionAST(pos, "print") =>
            indent(
              s"store i32 (i8*, ...)* @printf, i32 (i8*, ...)** %${operation("alloca i32 (i8*, ...)*, align 8")}, align 8")
            operation(s"load i32 (i8*, ...)*, i32 (i8*, ...)** %${counter.current}, align 8")
          case ApplyExpressionAST(fpos, f @ VariableExpressionAST(_, "print"), apos, List((_, arg)), tailrecursive) =>
            operation(
              s"call i32 (i8*, ...) %${compileExpression(f)}(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.number_str, i64 0, i64 0), i32 %${compileExpression(arg)})")
        }

        counter.current
      }

      line("define i32 @" ++ name ++ "() {")
      compileExpression(parts.head.body)
      indent("ret i32 0")
      line("}")
    }

    compileSource(as.head)
    out.toString
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
      count

}
