package xyz.hyperreal.sysl

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.parsing.input.Position

object CodeGenerator {

  def apply(as: List[SourceAST]): String = {
    val globalVars = new mutable.HashMap[String, VarDef]
    val out        = new StringBuilder

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

    def typeFromString(typ: String) =
      typ match {
        case "Int"    => IntType
        case "Long"   => LongType
        case "Double" => DoubleType
        case "Char"   => CharType
      }

    def compileTopLevelStatement(stmt: StatementAST): Unit =
      stmt match {
        case DeclarationBlockAST(decls) => decls foreach compileTopLevelStatement
        case DefAST(_, name, FunctionPieceAST(_, parms, arb, parts, where)) =>
          compileFunction(name, parms, arb, parts, where)
        case VarAST(pos, name, None, init) => // todo: variable type
          globalVars get name match {
            case Some(_) => problem(pos, s"duplicate variable definition")
            case None =>
              val const =
                init match {
                  case None              => Constant(0, IntType)
                  case Some((pos, expr)) => eval(pos, expr)
                }
              globalVars(name) = VarDef(const.typ, const)
              line(s"@$name = global ${const.typ} ${const.value}")
          }
      }

    def literal(v: Any) =
      v match {
        case a: Int    => Constant(a, IntType)
        case a: Char   => Constant(a, CharType)
        case a: Double => Constant(a, DoubleType)
      }

    def eval(pos: Position, expr: ExpressionAST): Constant = {
      def numeric(expr: ExpressionAST) =
        eval(pos, expr) match {
          case r @ Constant(_, _: NumericType) => r
          case _                               => problem(pos, "none numeric type")
        }

      expr match {
        case LiteralExpressionAST(v: Int)     => literal(v)
        case UnaryExpressionAST("+", _, expr) => numeric(expr)
        case UnaryExpressionAST("-", _, expr) =>
          numeric(expr) match {
            case Constant(value: Int, typ)    => Constant(-value, typ)
            case Constant(value: Long, typ)   => Constant(-value, typ)
            case Constant(value: Char, _)     => Constant(-value, IntType)
            case Constant(value: Double, typ) => Constant(-value, typ)
          }
        case UnaryExpressionAST(op, _, _) => problem(pos, s"invalid unary operator: '$op'")
        case BinaryExpressionAST(lpos, left, op, rpos, right) =>
          val l = numeric(left)
          val r = numeric(right)
          val (l1, r1) =
            (l, r) match {
              case _ if l.typ == r.typ                              => (l, r)
              case (Constant(_: Double, tl), Constant(vr: Long, _)) => (l, Constant(vr.toDouble, tl))
              case (Constant(vl: Long, _), Constant(_: Double, tr)) => (Constant(vl.toDouble, tr), r)
              case (Constant(_: Long, tl), Constant(vr: Int, _))    => (l, Constant(vr.toLong, tl))
              case (Constant(vl: Int, _), Constant(_: Long, tr))    => (Constant(vl.toLong, tr), r)
              case (Constant(_: Double, tl), Constant(vr: Int, _))  => (l, Constant(vr.toDouble, tl))
              case (Constant(vl: Int, _), Constant(_: Double, tr))  => (Constant(vl.toDouble, tr), r)
              case (Constant(_: Int, tl), Constant(vr: Char, _))    => (l, Constant(vr.toInt, tl))
              case (Constant(vl: Char, _), Constant(_: Int, tr))    => (Constant(vl.toInt, tr), r)
            }

          (l1, op, r1) match {
            case (Constant(a: Int, t), "+", Constant(b: Int, _))       => Constant(a + b, t)
            case (Constant(a: Int, t), "-", Constant(b: Int, _))       => Constant(a - b, t)
            case (Constant(a: Int, t), "*", Constant(b: Int, _))       => Constant(a * b, t)
            case (Constant(a: Int, t), "/", Constant(b: Int, _))       => Constant(a / b, t)
            case (Constant(a: Int, t), "%", Constant(b: Int, _))       => Constant(a % b, t)
            case (Constant(a: Long, t), "+", Constant(b: Long, _))     => Constant(a + b, t)
            case (Constant(a: Long, t), "-", Constant(b: Long, _))     => Constant(a - b, t)
            case (Constant(a: Long, t), "*", Constant(b: Long, _))     => Constant(a * b, t)
            case (Constant(a: Long, t), "/", Constant(b: Long, _))     => Constant(a / b, t)
            case (Constant(a: Long, t), "%", Constant(b: Long, _))     => Constant(a % b, t)
            case (Constant(a: Double, t), "+", Constant(b: Double, _)) => Constant(a + b, t)
            case (Constant(a: Double, t), "-", Constant(b: Double, _)) => Constant(a - b, t)
            case (Constant(a: Double, t), "*", Constant(b: Double, _)) => Constant(a * b, t)
            case (Constant(a: Double, t), "/", Constant(b: Double, _)) => Constant(a / b, t)
          }
        case _ => problem(pos, s"initializer not compile-time constant")
      }
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
          case p: PatternAST                 => problem(p.pos, s"pattern type not implemented yet: $p")
        } toSet

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
          case UnaryExpressionAST("-", pos, expr) => operation(s"sub i32 0, %${compileExpression(expr)}")
          case BinaryExpressionAST(lpos, left, op, rpos, right) =>
            val inst =
              op match {
                case "+"   => "add"
                case "-"   => "sub"
                case "*"   => "mul"
                case "/"   => "sdiv"
                case "mod" => "srem"
                case "=="  => "icmp eq"
                case "!="  => "icmp ne"
                case "<"   => "icmp slt"
                case "<="  => "icmp sle"
                case ">"   => "icmp sgt"
                case ">="  => "icmp sge"
              }

            operation(s"$inst i32 %${compileExpression(left)}, %${compileExpression(right)}")
          case BlockExpressionAST(stmts) => stmts foreach compileStatement
          case LiteralExpressionAST(v: Any) =>
            val Constant(value, typ) = literal(v)

            indent(s"store $typ $value, i32* %${operation(s"alloca $typ")}")
            operation(s"load $typ, $typ* %${valueCounter.current}")
          case VariableExpressionAST(pos, "print") =>
            indent(
              s"store i32 (i8*, ...)* @printf, i32 (i8*, ...)** %${operation("alloca i32 (i8*, ...)*, align 8")}, align 8")
            operation(s"load i32 (i8*, ...)*, i32 (i8*, ...)** %${valueCounter.current}, align 8")
          case VariableExpressionAST(pos, name) =>
            globalVars get name match {
              case Some(VarDef(typ, _)) =>
                operation(s"load $typ, $typ* @$name")
              case None =>
                if (parmset(name)) {
                  indent(s"store i32 %$name, i32* %${operation("alloca i32, align 4")}, align 4")
                  operation(s"load i32, i32* %${valueCounter.current}, align 4")
                } else {
                  indent(
                    s"store i32 (i32, i32)* @$name, i32 (i32, i32)** %${operation("alloca i32 (i32, i32)*, align 8")}, align 8")
                  operation(s"load i32 (i32, i32)*, i32 (i32, i32)** %${valueCounter.current}, align 8")
                }
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

      val parmdef = parms map { case VariablePatternAST(_, name) => s"i32 %$name" } mkString ", "

      line("define i32 @" ++ name ++ s"($parmdef) {")
      line("entry:")
      indent(s"ret i32 %${compileExpression(parts.head.body)}")
      line("}")
    }

    compileSource(as.head)
    out.toString
  }

  abstract class Def { val typ: Type }
  case class VarDef(typ: Type, init: Constant) extends Def

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

}
