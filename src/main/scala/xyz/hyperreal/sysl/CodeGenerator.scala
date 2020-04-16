package xyz.hyperreal.sysl

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.parsing.input.Position

object CodeGenerator {

  def apply(as: List[SourceAST]): String = {
    val globalDefs = new mutable.HashMap[String, Def]
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
      line("""@.int.format = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1""")
      line("""@.double.format = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1""")
      line("declare i32 @printf(i8*, ...)")
      src.stmts foreach compileTopLevelStatementPass1
      src.stmts foreach compileTopLevelStatementPass2
    }

    def typeFromString(typ: String) =
      typ match {
        case "Int"    => IntType
        case "Long"   => LongType
        case "Double" => DoubleType
        case "Char"   => CharType
      }

    def compileTopLevelStatementPass1(stmt: StatementAST): Unit =
      stmt match {
        case DeclarationBlockAST(decls) => decls foreach compileTopLevelStatementPass1
        case DefAST(_, name, FunctionPieceAST(_, parms, arb, parts, where)) =>
          globalDefs(name) = FunctionDef(FunctionType(IntType, parms map (_ => IntType), arb))
        case VarAST(pos, name, None, init) => // todo: variable type
          globalDefs get name match {
            case Some(_) => problem(pos, s"duplicate definition for '$name''")
            case None =>
              val (const, typ) =
                init match {
                  case None              => (0, IntType)
                  case Some((pos, expr)) => eval(pos, expr)
                }
              globalDefs(name) = VarDef(typ, const)
          }
      }

    def compileTopLevelStatementPass2(stmt: StatementAST): Unit =
      stmt match {
        case DeclarationBlockAST(decls) => decls foreach compileTopLevelStatementPass2
        case DefAST(_, name, FunctionPieceAST(_, parms, arb, parts, where)) =>
          compileFunction(name, parms, arb, parts, where)
        case VarAST(pos, name, None, init) =>
          val VarDef(typ, const) = globalDefs(name).asInstanceOf[VarDef]

          line(s"@$name = global $typ $const")
      }

    def literal(v: Any) =
      v match {
        case null       => (0, PointerType(VoidType))
        case a: Int     => (a, IntType)
        case a: Long    => (a, LongType)
        case a: Char    => (a, CharType)
        case a: Double  => (a, DoubleType)
        case a: Boolean => (a, BoolType)
      }

    def eval(pos: Position, expr: ExpressionAST): (Any, Type) = {
      def numeric(expr: ExpressionAST) =
        eval(pos, expr) match {
          case r @ (_, _: NumericType) => r
          case _                       => problem(pos, "none numeric type")
        }

      expr match {
        case LiteralExpressionAST(v: Int)     => literal(v)
        case UnaryExpressionAST("+", _, expr) => numeric(expr)
        case UnaryExpressionAST("-", _, expr) =>
          numeric(expr) match {
            case (value: Int, typ)    => (-value, typ)
            case (value: Long, typ)   => (-value, typ)
            case (value: Char, _)     => (-value, IntType)
            case (value: Double, typ) => (-value, typ)
          }
        case UnaryExpressionAST(op, _, _) => problem(pos, s"invalid unary operator: '$op'")
        case BinaryExpressionAST(lpos, left, op, rpos, right) =>
          val l = numeric(left)
          val r = numeric(right)
          val (l1, r1) =
            (l, r) match {
              case _ if l._2 == r._2                => (l, r)
              case ((_: Double, tl), (vr: Long, _)) => (l, (vr.toDouble, tl))
              case ((vl: Long, _), (_: Double, tr)) => ((vl.toDouble, tr), r)
              case ((_: Long, tl), (vr: Int, _))    => (l, (vr.toLong, tl))
              case ((vl: Int, _), (_: Long, tr))    => ((vl.toLong, tr), r)
              case ((_: Double, tl), (vr: Int, _))  => (l, (vr.toDouble, tl))
              case ((vl: Int, _), (_: Double, tr))  => ((vl.toDouble, tr), r)
              case ((_: Int, tl), (vr: Char, _))    => (l, (vr.toInt, tl))
              case ((vl: Char, _), (_: Int, tr))    => ((vl.toInt, tr), r)
            }

          (l1, op, r1) match {
            case ((a: Int, t), "+", (b: Int, _))       => (a + b, t)
            case ((a: Int, t), "-", (b: Int, _))       => (a - b, t)
            case ((a: Int, t), "*", (b: Int, _))       => (a * b, t)
            case ((a: Int, t), "/", (b: Int, _))       => (a / b, t)
            case ((a: Int, t), "%", (b: Int, _))       => (a % b, t)
            case ((a: Long, t), "+", (b: Long, _))     => (a + b, t)
            case ((a: Long, t), "-", (b: Long, _))     => (a - b, t)
            case ((a: Long, t), "*", (b: Long, _))     => (a * b, t)
            case ((a: Long, t), "/", (b: Long, _))     => (a / b, t)
            case ((a: Long, t), "%", (b: Long, _))     => (a % b, t)
            case ((a: Double, t), "+", (b: Double, _)) => (a + b, t)
            case ((a: Double, t), "-", (b: Double, _)) => (a - b, t)
            case ((a: Double, t), "*", (b: Double, _)) => (a * b, t)
            case ((a: Double, t), "/", (b: Double, _)) => (a / b, t)
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

      def compileStatement(stmt: StatementAST) =
        stmt match {
          case _: DeclarationStatementAST => VoidType
          case t: ExpressionAST           => compileExpression(t)._2
        }

      def compileExpression(expr: ExpressionAST): (Int, Type) = {
        val typ =
          expr match {
            case ConditionalExpressionAST(cond, els) =>
              val donelabel = labelName

              @tailrec
              def gencond(cs: Seq[(Position, ExpressionAST, ExpressionAST)]): Unit = {
                val (pos, condexpr, trueexpr) = cs.head

                val truelabel  = labelName
                val falselabel = labelName

                position(pos)

                val (condvalue, condtype) = compileExpression(condexpr)

                if (condtype != BoolType)
                  problem(pos, s"expected expression of type Bool, found ${condtype.name}")

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
              VoidType // todo: get type correctly by looking all posibilities
            case WhileExpressionAST(lab, cond, body, els) =>
              val begin  = label
              val end    = labelName
              val inside = labelName

              indent(s"br i1 %${compileExpression(cond)._1}, label %$inside, label %$end")
              line(s"$inside:")
              body foreach compileExpression
              indent(s"br label $begin")
              VoidType // todo: get type correctly by looking all while body
            case UnaryExpressionAST("+", pos, expr) => compileExpression(expr)._2
            case UnaryExpressionAST("-", pos, expr) =>
              val (operand, typ) = compileExpression(expr)

              operation(s"sub $typ 0, %$operand")
              typ
            case BinaryExpressionAST(lpos, left, op, rpos, right) =>
              val (l, tl) = compileExpression(left)
              val (r, tr) = compileExpression(right)
              val (rt, ol, or) =
                (tl, tr) match {
                  case _ if tl == tr                             => (tl, l, r)
                  case (DoubleType, LongType)                    => (DoubleType, l, operation(s"sitofp i64 %$r to double"))
                  case (LongType, DoubleType)                    => (DoubleType, operation(s"sitofp i64 %$l to double"), r)
                  case (LongType, IntType)                       => (LongType, l, operation(s"sext i32 %$r to i64"))
                  case (IntType, LongType)                       => (LongType, operation(s"sext i32 %$l to i64"), r)
                  case (DoubleType, IntType)                     => (DoubleType, l, operation(s"sitofp i32 %$r to double"))
                  case (IntType, DoubleType)                     => (DoubleType, operation(s"sitofp i32 %$l to double"), r)
                  case (IntType, CharType) | (CharType, IntType) => (IntType, l, r)
                }

              val inst =
                (rt, op) match {
                  case (_: IntegerType, "+")   => "add"
                  case (_: FloatType, "+")     => "fadd"
                  case (_: IntegerType, "-")   => "sub"
                  case (_: FloatType, "-")     => "fsub"
                  case (_: IntegerType, "*")   => "mul"
                  case (_: FloatType, "*")     => "fmul"
                  case (_: IntegerType, "/")   => "sdiv"
                  case (_: FloatType, "/")     => "fdiv"
                  case (_: IntegerType, "mod") => "srem"
                  case (_: FloatType, "mod")   => "frem"
                  case (_: IntegerType, "==")  => "icmp eq" // todo: unsigned comparisons?
                  case (_: FloatType, "==")    => "fcmp ueq"
                  case (_: IntegerType, "!=")  => "icmp ne"
                  case (_: FloatType, "!=")    => "fcmp une"
                  case (_: IntegerType, "<")   => "icmp slt"
                  case (_: FloatType, "<")     => "fcmp ult"
                  case (_: IntegerType, "<=")  => "icmp sle"
                  case (_: FloatType, "<=")    => "fcmp ule"
                  case (_: IntegerType, ">")   => "icmp sgt"
                  case (_: FloatType, ">")     => "fcmp ugt"
                  case (_: IntegerType, ">=")  => "icmp sge"
                  case (_: FloatType, ">=")    => "fcmp uge"
                }

              operation(s"$inst $rt %$ol, %$or")
              rt
            case BlockExpressionAST(stmts) =>
              stmts.init foreach compileStatement
              compileStatement(stmts.last)
            case LiteralExpressionAST(v: Any) =>
              val (value, typ) = literal(v)

              indent(s"store $typ $value, $typ* %${operation(s"alloca $typ")}")
              operation(s"load $typ, $typ* %${valueCounter.current}")
              typ
            case VariableExpressionAST(pos, name) =>
              globalDefs get name match {
                case Some(VarDef(typ, _)) =>
                  operation(s"load $typ, $typ* @$name")
                  typ
                case Some(FunctionDef(func @ FunctionType(ret, parms, arb))) => //FunctionDef
                  indent(s"store $ret (${parms mkString ","})* @$name, $ret (${parms mkString ","})** %${operation(
                    s"alloca $ret(${parms mkString ","})*, align 8")}, align 8")
                  operation(
                    s"load $ret(${parms mkString ","})*, $ret(${parms mkString ","})** %${valueCounter.current}, align 8")
                  FunctionType(IntType, List(IntType, IntType))
                case None =>
                  if (parmset(name)) {
                    indent(s"store i32 %$name, i32* %${operation("alloca i32, align 4")}, align 4")
                    operation(s"load i32, i32* %${valueCounter.current}, align 4")
                    IntType // todo: parameters
                  } else {
                    problem(pos, s"unknown identifier: $name")
                  }
              }
            case ApplyExpressionAST(fpos, VariableExpressionAST(_, "print"), apos, List((_, arg)), tailrecursive) =>
              val (a, at) = compileExpression(arg)

              indent(
                s"store i32 (i8*, ...)* @printf, i32 (i8*, ...)** %${operation("alloca i32 (i8*, ...)*, align 8")}, align 8")
              operation(s"load i32 (i8*, ...)*, i32 (i8*, ...)** %${valueCounter.current}, align 8")

              at match {
                case IntType =>
                  operation(
                    s"call i32 (i8*, ...) %${valueCounter.current}(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.int.format, i64 0, i64 0), i32 %$a)")
                case DoubleType =>
                  operation(
                    s"call i32 (i8*, ...) %${valueCounter.current}(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.double.format, i64 0, i64 0), double %$a)")
              }

              VoidType
            case ApplyExpressionAST(fpos, f, apos, args, tailrecursive) =>
              val (func, ftyp) = compileExpression(f)
              val rtyp =
                ftyp match {
                  case FunctionType(ret, parms, arb) => ret
                  case a                             => problem(fpos, s"expected function type: $a")
                }
              val argvals =
                for ((p, a) <- args)
                  yield {
                    val (e, t) = compileExpression(a)

                    s"$t %$e"
                  }

              operation(
                s"call $rtyp (${Iterator.fill(args.length)("i32") mkString ", "}) %$func(${argvals mkString ", "})")
              rtyp
          }

        (valueCounter.current, typ)
      }

      val parmdef = parms map { case VariablePatternAST(_, name) => s"i32 %$name" } mkString ", "

      line("define i32 @" ++ name ++ s"($parmdef) {")
      line("entry:")
      indent(s"ret i32 %${compileExpression(parts.head.body)._1}")
      line("}")
    }

    compileSource(as.head)
    out.toString
  }

  abstract class Def { val typ: Type }
  case class VarDef(typ: Type, const: Any)  extends Def
  case class FunctionDef(typ: FunctionType) extends Def

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
