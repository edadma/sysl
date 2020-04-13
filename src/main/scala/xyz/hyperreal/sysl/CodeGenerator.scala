package xyz.hyperreal.sysl

import scala.annotation.tailrec
import scala.util.parsing.input.Position

object CodeGenerator {

  def apply(as: List[SourceAST]): String = {
    val out = new StringBuilder

    def line(s: String): Unit = {
      out ++= s
      out += '\n'
    }

    def indent(s: String): Unit = {
      out ++= "  "
      line(s)
    }

    def position(pos: Position): Unit = {
      indent(s"; ${pos.line}:${pos.column}")
      indent(s"; ${pos.longString.replace("\n", "\n  ; ")}")
    }

    def compileSource(src: SourceAST): Unit = {
      line("""@.number_str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1""")
      line("declare i32 @printf(i8*, ...)")
      src.stmts foreach compileTopLevelStatement
    }

    def compileTopLevelStatement(stmt: StatementAST): Unit =
      stmt match {
        case DeclarationBlockAST(decls) => decls foreach compileTopLevelStatement
        case DefAST(_, name, FunctionPieceAST(_, parms, arb, parts, where)) =>
          compileFunction(name, parms, arb, parts, where)
      }

    def compileFunction(name: String,
                        parms: List[PatternAST],
                        arb: Boolean,
                        parts: List[FunctionPart],
                        where: List[DeclarationStatementAST]): Unit = {
      val valueCounter = new Counter
      val blockCounter = new Counter
      val parmset =
        parms map {
          case VariablePatternAST(pos, name) => name
          case p                             => sys.error(s"pattern type not implemented yet: $p")
        } toSet

//      def getParm(p: String) =
//        parms indexWhere {
//          case VariablePatternAST(pos, name) => name == p
//          case p                             => sys.error(s"pattern type not implemented yet: $p")
//        }

      def operation(s: String) = {
        indent(s"%${valueCounter.next} = $s")
        valueCounter.current
      }

      def labelName = s"l${blockCounter.next}"

      def label = {
        val name = labelName

        line(s"$name:")
        name
      }

      def compileStatement(stmt: StatementAST): Unit =
        stmt match {
          case _: DeclarationStatementAST =>
          case t: ExpressionAST           => compileExpression(t)
        }

      def compileExpression(expr: ExpressionAST): Int = {
        expr match {
          case ConditionalExpressionAST(cond, els) =>
            val donelabel = labelName

            @tailrec
            def gencond(cs: Seq[(Position, ExpressionAST, ExpressionAST)]): Unit = {
              val (pos, condexpr, trueexpr) = cs.head

              val truelabel  = labelName
              val falselabel = labelName

              position(pos)

              val condvalue = compileExpression(condexpr)

              indent(s"br i1 %$condvalue, label %$truelabel, label %$falselabel")
              line(s"$truelabel:")
              compileExpression(trueexpr)
              indent(s"br label %$donelabel")
              line(s"$falselabel:")

              if (cs.tail nonEmpty)
                gencond(cs.tail)
            }

            gencond(cond)
            els foreach compileExpression
            indent(s"br label %$donelabel")
            line(s"$donelabel:")
          case WhileExpressionAST(lab, cond, body, els) =>
            val begin  = label
            val end    = labelName
            val inside = labelName

            indent(s"br i1 %${compileExpression(cond)}, label %$inside, label %$end")
            line(s"$inside:")
            body foreach compileExpression
            indent(s"br label $begin")
          case UnaryExpressionAST("+", pos, expr) => compileExpression(expr)
          case UnaryExpressionAST("-", pos, expr) =>
            operation(s"sub i32 0, %${compileExpression(expr)}")
          case BinaryExpressionAST(lpos, left, "+", rpos, right) =>
            operation(s"add i32 %${compileExpression(left)}, %${compileExpression(right)}")
          case BinaryExpressionAST(lpos, left, "-", rpos, right) =>
            operation(s"sub i32 %${compileExpression(left)}, %${compileExpression(right)}")
          case BinaryExpressionAST(lpos, left, "*", rpos, right) =>
            operation(s"mul i32 %${compileExpression(left)}, %${compileExpression(right)}")
          case BinaryExpressionAST(lpos, left, "/", rpos, right) =>
            operation(s"sdiv i32 %${compileExpression(left)}, %${compileExpression(right)}")
          case ComparisonExpressionAST(lpos, left, List(("==", rpos, right))) =>
            operation(s"icmp eq i32 %${compileExpression(left)}, %${compileExpression(right)}")
          case ComparisonExpressionAST(lpos, left, List(("!=", rpos, right))) =>
            operation(s"icmp ne i32 %${compileExpression(left)}, %${compileExpression(right)}")
          case ComparisonExpressionAST(lpos, left, List((">", rpos, right))) =>
            operation(s"icmp sgt i32 %${compileExpression(left)}, %${compileExpression(right)}")
          case ComparisonExpressionAST(lpos, left, List((">=", rpos, right))) =>
            operation(s"icmp sge i32 %${compileExpression(left)}, %${compileExpression(right)}")
          case ComparisonExpressionAST(lpos, left, List(("<", rpos, right))) =>
            operation(s"icmp slt i32 %${compileExpression(left)}, %${compileExpression(right)}")
          case ComparisonExpressionAST(lpos, left, List(("<=", rpos, right))) =>
            operation(s"icmp sle i32 %${compileExpression(left)}, %${compileExpression(right)}")
          case BlockExpressionAST(stmts) => stmts foreach compileStatement
          case LiteralExpressionAST(v: Int) =>
            indent(s"store i32 $v, i32* %${operation("alloca i32, align 4")}, align 4")
            operation(s"load i32, i32* %${valueCounter.current}, align 4")
          case VariableExpressionAST(pos, "print") =>
            indent(
              s"store i32 (i8*, ...)* @printf, i32 (i8*, ...)** %${operation("alloca i32 (i8*, ...)*, align 8")}, align 8")
            operation(s"load i32 (i8*, ...)*, i32 (i8*, ...)** %${valueCounter.current}, align 8")
          case VariableExpressionAST(pos, name) =>
//            getParm(name) match {
//              case -1 => problem(pos, s"parameter '$name' not found")
//            }
            if (parmset(name)) {
              indent(s"store i32 %$name, i32* %${operation("alloca i32, align 4")}, align 4")
              operation(s"load i32, i32* %${valueCounter.current}, align 4")
            } else {
              indent(
                s"store i32 (i32, i32)* @$name, i32 (i32, i32)** %${operation("alloca i32 (i32, i32)*, align 8")}, align 8")
              operation(s"load i32 (i32, i32)*, i32 (i32, i32)** %${valueCounter.current}, align 8")
            }
          case ApplyExpressionAST(fpos, f @ VariableExpressionAST(_, "print"), apos, List((_, arg)), tailrecursive) =>
            operation(
              s"call i32 (i8*, ...) %${compileExpression(f)}(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.number_str, i64 0, i64 0), i32 %${compileExpression(arg)})")
          case ApplyExpressionAST(fpos, f, apos, args, tailrecursive) =>
            val func = compileExpression(f)
            val argvals =
              for ((_, a) <- args)
                yield s"i32 %${compileExpression(a)}"

            operation(s"call i32 (${Iterator.fill(args.length)("i32") mkString ", "}) %$func(${argvals mkString ", "})")
        }

        valueCounter.current
      }

      val parmdef = parms map {
        case VariablePatternAST(pos, name) => s"i32 %$name"
      } mkString ", "

      line("define i32 @" ++ name ++ s"($parmdef) {")
      line("entry:")
      indent(s"ret i32 %${compileExpression(parts.head.body)}")
      line("}")
    }

    compileSource(as.head)
    out.toString
  }

}

class Counter {

  private var count = 0

  def next: Int = {
    count += 1
    count - 1
  }

  def current: Int =
    if (count == 0)
      sys.error("no last count")
    else
      count - 1

}
