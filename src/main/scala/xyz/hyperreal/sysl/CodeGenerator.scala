package xyz.hyperreal.sysl

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.parsing.input.Position

object CodeGenerator {

  def apply(as: List[SourceAST]): String = {
    val globalDefs = new mutable.HashMap[String, Def]
    val out        = new StringBuilder
    val stringMap  = new mutable.HashMap[String, Int]

    def line(s: String): Unit = {
      out ++= s
      out += '\n'
    }

    def indent(s: String): Unit = {
      out ++= "  "
      line(s)
    }

    def cast(rtyp: Type, value: String, typ: Type) = s"bitcast ($typ $value to $rtyp)"

    def position(pos: Position): Unit = {
      indent(s"; ${pos.line}:${pos.column}")
      indent(s"; ${pos.longString.replace("\n", "\n  ; ")}")
    }

    def compileSource(src: SourceAST): Unit = {
      line("""@.int.format = private unnamed_addr constant [3 x i8] c"%d\00", align 1""")
      line("""@.char.format = private unnamed_addr constant [3 x i8] c"%c\00", align 1""")
      line("""@.double.format = private unnamed_addr constant [3 x i8] c"%f\00", align 1""")
      line("""@.str.format = private unnamed_addr constant [3 x i8] c"%s\00", align 1""")
      line("""@.nl.format = private unnamed_addr constant [2 x i8] c"\0A\00", align 1""")
      line("""@.comma.format = private unnamed_addr constant [3 x i8] c", \00", align 1""")
      line("""@.bool.format = private unnamed_addr constant [9 x i8] c"bool(%d)\00", align 1""")
      line("""@.null.format = private unnamed_addr constant [5 x i8] c"null\00", align 1""")
      line("declare i32 @printf(i8*, ...)")
      src.stmts foreach compileTopLevelStatementPass1
      src.stmts foreach compileTopLevelStatementPass2
    }

//    def typeFromString(typ: String): Type =
//      typ match {
//        case "int"    => IntType
//        case "long"   => LongType
//        case "double" => DoubleType
//        case "char"   => CharType
//        case "unit"   => UnitType
//      }

    def datatype(pos: Position, datatypeAST: DatatypeAST): Type =
      datatypeAST match {
        case IntTypeAST          => IntType
        case BCharTypeAST        => BCharType
        case CharTypeAST         => CharType
        case LongTypeAST         => LongType
        case UnitTypeAST         => UnitType
        case DoubleTypeAST       => DoubleType
        case IdentTypeAST(name)  => problem(pos, s"unknown type '$name'")
        case PointerTypeAST(typ) => PointerType(datatype(pos, typ))
      }

    def strings(ast: AST): Unit =
      ast match {
        case DerefExpressionAST(_, _, expr)   => strings(expr)
        case PreExpressionAST(op, pos, expr)  => strings(expr)
        case PostExpressionAST(op, pos, expr) => strings(expr)
        case LiteralExpressionAST(s: String) if !stringMap.contains(s) =>
          line(s"""@.str.${stringMap.size} = private unnamed_addr constant [${s.length + 1} x i8] c"$s\00", align 1""")
          stringMap(s) = stringMap.size
        case _: LiteralExpressionAST | _: VariableExpressionAST | _: AddressExpressionAST =>
        case AssignmentExpressionAST(lhs, _, rhs) =>
          lhs foreach { case (_, e) => strings(e) }
          rhs foreach { case (_, e) => strings(e) }
        case FunctionPart(guard, body) =>
          guard foreach strings
          strings(body)
        case BlockExpressionAST(stmts)                              => stmts foreach strings
        case UnaryExpressionAST(op, pos, expr)                      => strings(expr)
        case ApplyExpressionAST(fpos, f, apos, args, tailrecursive) => args foreach { case (_, e) => strings(e) }
        case BinaryExpressionAST(lpos, left, op, rpos, right) =>
          strings(left)
          strings(right)
        case ComparisonExpressionAST(pos, expr, comparisons) =>
          strings(expr)
          comparisons foreach { case (_, _, e) => strings(e) }
        case WhileExpressionAST(label, cond, body, els) =>
          strings(cond)
          body foreach strings
          els foreach strings
        case ConditionalExpressionAST(cond, els) =>
          cond foreach {
            case (_, c, t) =>
              strings(c)
              strings(t)
          }
          els foreach strings
      }

    def compileTopLevelStatementPass1(stmt: StatementAST): Unit =
      stmt match {
        case DeclarationBlockAST(decls) => decls foreach compileTopLevelStatementPass1
        case DefAST(_, name, FunctionPieceAST(_, ret, parms, arb, parts, where)) =>
          globalDefs(name) = FunctionDef(FunctionType(datatype(ret.get._1, ret.get._2), parms map {
            case TypePatternAST(_, pos, typename) => datatype(pos, typename)
          }, arb))
          parts foreach strings
        case VarAST(pos, name, dtyp, init) => // todo: variable type
          globalDefs get name match {
            case Some(_) => problem(pos, s"duplicate definition for '$name'")
            case None =>
              init map (_._2) foreach strings

              val (const, typ) =
                init match {
                  case None if dtyp isDefined => ("0", datatype(dtyp.get._1, dtyp.get._2))
                  case None                   => problem(pos, "missing type")
                  case Some((pos, AddressExpressionAST(apos, name))) =>
                    globalDefs get name match {
                      case Some(d: Def) => (s"@$name", d.typ)
                      case None         => problem(pos, s"unknown variable: $name")
                    }
                  case Some((pos, LiteralExpressionAST(s: String))) =>
                    (s"@.str.${stringMap(s)}", PointerType(ArrayType(s.length + 1, BCharType)))
                  case Some((pos, expr)) => eval(pos, expr)
                }

              val typ1 =
                dtyp match {
                  case None => typ
                  case Some(ddt) =>
                    (datatype(ddt._1, ddt._2), typ) match {
                      case (dt @ PointerType(t), PointerType(ArrayType(_, u))) =>
                        if (t == u)
                          dt
                        else
                          problem(dtyp.get._1, "type parameters are not the same type")
                    }
                }

              globalDefs(name) = VarDef(typ1, const, typ) // todo: actual type should be declared, with possible conversion
          }

          init map (_._2) foreach strings
      }

    def compileTopLevelStatementPass2(stmt: StatementAST): Unit =
      stmt match {
        case DeclarationBlockAST(decls) => decls foreach compileTopLevelStatementPass2
        case DefAST(_, name, FunctionPieceAST(_, ret, parms, arb, parts, where)) =>
          compileFunction(name, ret, parms, arb, parts, where)
        case VarAST(pos, name, _, _) =>
          val VarDef(typ, const, ctyp) = globalDefs(name).asInstanceOf[VarDef]
          val const1 =
            if (typ == ctyp)
              const
            else
              cast(typ, const, ctyp)

          line(s"@$name = global $typ $const1")
      }

    def literal(v: Any): (Any, Type) =
      v match {
        case null       => (null, PointerType(UnitType))
        case a: Int     => (a, IntType)
        case a: Long    => (a, LongType)
        case a: Char    => (a, CharType)
        case a: Double  => (a, DoubleType)
        case a: Boolean => (a, BoolType)
      }

    def eval(pos: Position, expr: ExpressionAST) = {
      def eval(expr: ExpressionAST): (Any, Type) = {
        def numeric(expr: ExpressionAST) =
          eval(expr) match {
            case r @ (_, _: NumericType) => r
            case _                       => problem(pos, "non-numeric type")
          }

        expr match {
          case LiteralExpressionAST(v: Char)    => literal(v)
          case LiteralExpressionAST(v: Int)     => literal(v)
          case LiteralExpressionAST(v: Double)  => literal(v)
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
              case ((a: Int, t), "+", (b: Int, _)) =>
                (a + b, t) // todo: have to be able to handle constant pointer arithmetic
              case ((a: Int, t), "-", (b: Int, _))       => (a - b, t)
              case ((a: Int, t), "*", (b: Int, _))       => (a * b, t)
              case ((a: Int, t), "/", (b: Int, _))       => (a / b, t)
              case ((a: Int, t), "mod", (b: Int, _))     => (a % b, t)
              case ((a: Long, t), "+", (b: Long, _))     => (a + b, t)
              case ((a: Long, t), "-", (b: Long, _))     => (a - b, t)
              case ((a: Long, t), "*", (b: Long, _))     => (a * b, t)
              case ((a: Long, t), "/", (b: Long, _))     => (a / b, t)
              case ((a: Long, t), "mod", (b: Long, _))   => (a % b, t)
              case ((a: Double, t), "+", (b: Double, _)) => (a + b, t)
              case ((a: Double, t), "-", (b: Double, _)) => (a - b, t)
              case ((a: Double, t), "*", (b: Double, _)) => (a * b, t)
              case ((a: Double, t), "/", (b: Double, _)) => (a / b, t)
            }
          case _ => problem(pos, s"initializer not compile-time constant")
        }
      }

      val (v, t) = eval(expr)

      (v.toString, t)
    }

    def compileFunction(name: String,
                        ret: Option[(Position, DatatypeAST)],
                        parms: List[PatternAST],
                        arb: Boolean,
                        parts: List[FunctionPart],
                        where: List[DeclarationStatementAST]): Unit = {
      val valueCounter = new Counter
      val blockCounter = new Counter
      val parmMap =
        mutable.LinkedHashMap[String, Type](parms map {
          case VariablePatternAST(pos, name)                            => name -> null
          case TypePatternAST(VariablePatternAST(pos, name), tpos, typ) => name -> datatype(tpos, typ)
          case p: PatternAST                                            => problem(p.pos, s"pattern type not implemented yet: $p")
        }: _*)
      var expval: String = "undef"

      def operation(s: String) = {
        indent(s"%${valueCounter.next} = $s")
        expval = valueCounter.toString
        expval
      }

      def labelName = s"l${blockCounter.next}"

      def label = {
        val name = labelName

        line(s"$name:")
        name
      }

      def compileStatement(stmt: StatementAST) =
        stmt match {
          case _: DeclarationStatementAST => UnitType
          case t: ExpressionAST           => compileExpression(true, t)._2
        }

      def compileBinaryExpression(lpos: Position,
                                  left: ExpressionAST,
                                  op: String,
                                  rpos: Position,
                                  right: ExpressionAST) = {
        binaryExpression(lpos, compileExpression(true, left), op, rpos, compileExpression(true, right))
      }

      def binaryExpression(lpos: Position, left: (String, Type), op: String, rpos: Position, right: (String, Type)) = {
        (left, op, right) match {
          case ((l, lt @ PointerType(ltp)), "+", (r, rt: IntegerType)) =>
            operation(s"getelementptr $ltp, $lt $l, $rt $r")
            lt
          case ((l, lt @ PointerType(ltp)), "-", (r, rt: IntegerType)) =>
            operation(s"getelementptr $ltp, $lt $l, $rt -$r")
            lt
          case ((l, lt: IntegerType), "+", (r, rt @ PointerType(rtp))) =>
            operation(s"getelementptr $rtp, $rt $r, $lt $l")
            rt
          case ((l, lt: IntegerType), "-", (r, rt @ PointerType(rtp))) =>
            operation(s"getelementptr $rtp, $rt $r, $lt -$l")
            rt
          case ((_, _: PointerType), _, (_, _: IntegerType)) | ((_, _: IntegerType), _, (_, _: PointerType)) =>
            problem(lpos, "invalid operation for pointer arithmetic") // todo: op position should be used
          case ((_, _: PointerType), _, (_, _: NumericType)) | ((_, _: NumericType), _, (_, _: PointerType)) =>
            problem(lpos, "non-integer type used in pointer arithmetic") // todo: op position should be used
          case _ =>
            val (l, tl) = left
            val (r, tr) = right
            val (rt, ol, or) =
              (tl, tr) match {
                case _ if tl == tr                             => (tl, l, r)
                case (DoubleType, LongType)                    => (DoubleType, l, operation(s"sitofp i64 $r to double"))
                case (LongType, DoubleType)                    => (DoubleType, operation(s"sitofp i64 $l to double"), r)
                case (LongType, IntType)                       => (LongType, l, operation(s"sext i32 $r to i64"))
                case (IntType, LongType)                       => (LongType, operation(s"sext i32 $l to i64"), r)
                case (DoubleType, IntType)                     => (DoubleType, l, operation(s"sitofp i32 $r to double"))
                case (IntType, DoubleType)                     => (DoubleType, operation(s"sitofp i32 $l to double"), r)
                case (IntType, CharType) | (CharType, IntType) => (IntType, l, r)
                case (IntType, BCharType)                      => (IntType, l, operation(s"zext i8 $r to i32"))
                case (BCharType, IntType)                      => (IntType, operation(s"zext i8 $l to i32"), r)
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

            operation(s"$inst $rt $ol, $or")
            rt
        }
      }

      def compileExpression(rvalue: Boolean, expr: ExpressionAST): (String, Type) = {
        val typ =
          expr match {
            case ConditionalExpressionAST(cond, els) =>
              val donelabel = labelName

              @tailrec
              def gencond(cs: Seq[(Position, ExpressionAST, ExpressionAST)],
                          truelist: List[(String, (String, Type))]): (List[(String, (String, Type))], String) = {
                val (pos, condexpr, trueexpr) = cs.head

                val truelabel  = labelName
                val falselabel = labelName

                position(pos)

                val (condvalue, condtype) = compileExpression(true, condexpr)

                if (condtype != BoolType)
                  problem(pos, s"expected expression of type Bool, found ${condtype.name}")

                indent(s"br i1 $condvalue, label %$truelabel, label %$falselabel")
                line(s"$truelabel:")

                val trueval = compileExpression(rvalue, trueexpr)

                indent(s"br label %$donelabel")
                line(s"$falselabel:")

                if (cs.tail nonEmpty)
                  gencond(cs.tail, truelist :+ (truelabel, trueval))
                else
                  (truelist :+ (truelabel, trueval), falselabel)
              }

              val (truelist, falselabel) = gencond(cond, Nil)
              val (falseval, falsetyp) =
                els match {
                  case None    => (0, UnitType)
                  case Some(e) => compileExpression(rvalue, e)
                }

              indent(s"br label %$donelabel")
              line(s"$donelabel:")
              operation(
                s"phi $falsetyp ${truelist map { case (truelabel, (trueval, _)) => s"[ $trueval, %$truelabel ]" } mkString ", "}, [ $falseval, %$falselabel ]")
              falsetyp // todo: get type correctly by looking all types
            case WhileExpressionAST(lab, cond, body, els) =>
              val begin = labelName

              indent(s"br label %$begin")
              line(s"$begin:")

              val end      = labelName
              val loopbody = labelName

              indent(s"br i1 ${compileExpression(true, cond)._1}, label %$loopbody, label %$end")
              line(s"$loopbody:")
              body foreach (compileExpression(true, _))
              indent(s"br label %$begin")
              line(s"$end:")

              if (!rvalue) // todo: flip the variable to lvalue
                expval = null

              UnitType // todo: get type correctly by looking all while body
            case UnaryExpressionAST("+", pos, expr) => compileExpression(true, expr)._2
            case UnaryExpressionAST("-", pos, expr) =>
              val (operand, typ) = compileExpression(true, expr)

              operation(s"sub $typ 0, $operand")
              typ
            case DerefExpressionAST(pos, epos, expr) =>
              val (value, typ) = compileExpression(true, expr)

              typ match {
                case PointerType(pointee) if pointee.firstclass =>
                  if (rvalue) {
                    operation(s"load $pointee, $typ $value")
                    pointee
                  } else {
                    typ
                  }
                case _: PointerType => problem(epos, "only pointers to LLVM 'first class' types can be dereferenced")
                case _              => problem(epos, "expression does not return a pointer")
              }
            case BinaryExpressionAST(lpos, left, op, rpos, right) =>
              compileBinaryExpression(lpos, left, op, rpos, right)
            case BlockExpressionAST(stmts) =>
              stmts.init foreach compileStatement
              compileStatement(stmts.last)
            case LiteralExpressionAST(s: String) =>
              operation(
                s"getelementptr inbounds [${s.length + 1} x i8], [${s.length + 1} x i8]* @.str.${stringMap(s)}, i64 0, i64 0")
              PointerType(BCharType)
            case LiteralExpressionAST(v: Any) =>
              val (value, typ) = literal(v)

              expval = value.toString
              typ
            case PreExpressionAST(op @ ("++" | "--"), pos, expr) =>
              val (lvalue, ltyp) = compileExpression(false, expr)
              val typ            = compileBinaryExpression(pos, expr, op.head.toString, null, LiteralExpressionAST(1))

              indent(s"store $typ $valueCounter, $ltyp $lvalue")
              typ
            case PostExpressionAST(op, pos, expr) =>
              val (lvalue, ltyp) = compileExpression(false, expr)
              val (rvalue, rtyp) = compileExpression(true, expr)
              val typ            = binaryExpression(pos, (rvalue, rtyp), op.head.toString, null, ("1", IntType))

              indent(s"store $typ $valueCounter, $ltyp $lvalue")
              expval = rvalue
              rtyp
            case VariableExpressionAST(pos, name) =>
              globalDefs get name match {
                case Some(VarDef(typ, _, _)) =>
                  if (rvalue) {
                    operation(s"load $typ, $typ* @$name")
                    typ
                  } else {
                    expval = s"@$name"
                    PointerType(typ)
                  }
                case Some(FunctionDef(typ)) =>
                  indent(s"store $typ* @$name, $typ** ${operation(s"alloca $typ*, align 8")}, align 8")
                  operation(s"load $typ*, $typ** $valueCounter, align 8")
                  typ
                case Some(_) => problem(pos, "unimplemented")
                case None =>
                  parmMap get name match {
                    case Some(typ) =>
                      indent(s"store $typ %$name, $typ* ${operation(s"alloca $typ")}")
                      operation(s"load $typ, $typ* $valueCounter")
                      typ
                    case None =>
                      problem(pos, s"unknown identifier: $name")
                  }
              }
            case AddressExpressionAST(pos, name) =>
              globalDefs get name match {
                case Some(VarDef(typ, _, _)) =>
                  expval = s"@$name"
                  PointerType(typ)
                case Some(_) => problem(pos, "not yet implemented")
                case None    => problem(pos, s"undefined: $name")
              }
            case ApplyExpressionAST(fpos, VariableExpressionAST(_, "print"), apos, args, tailrecursive) =>
              indent(
                s"store i32 (i8*, ...)* @printf, i32 (i8*, ...)** ${operation("alloca i32 (i8*, ...)*, align 8")}, align 8")
              val printf = operation(s"load i32 (i8*, ...)*, i32 (i8*, ...)** $valueCounter, align 8")

              @scala.annotation.tailrec
              def printargs(args: List[(Position, ExpressionAST)]): Unit =
                args match {
                  case Nil =>
                  case (pos, arg) :: tl =>
                    val (a, at) = compileExpression(true, arg)

                    at match {
                      case BoolType =>
                        operation(s"call i32 (i8*, ...) $printf (i8* bitcast ([9 x i8]* @.bool.format to i8*), i1 $a)")
                      case BCharType =>
                        operation(s"call i32 (i8*, ...) $printf (i8* bitcast ([3 x i8]* @.char.format to i8*), i8 $a)")
                      case IntType =>
                        operation(s"call i32 (i8*, ...) $printf (i8* bitcast ([3 x i8]* @.int.format to i8*), i32 $a)")
                      case DoubleType =>
                        operation(
                          s"call i32 (i8*, ...) $printf (i8* bitcast ([3 x i8]* @.double.format to i8*), double $a)")
                      case PointerType(BCharType) | PointerType(ArrayType(_, BCharType)) =>
                        val LiteralExpressionAST(s: String) = arg

                        operation(s"call i32 (i8*, ...) $printf (i8* bitcast ([3 x i8]* @.str.format to i8*), i8* $a)")
                      case _ => problem(pos, s"don't know how to print that type (yet): $at")
                    }

                    if (tl nonEmpty)
                      operation(s"call i32 (i8*, ...) $printf (i8* bitcast ([3 x i8]* @.comma.format to i8*))")

                    printargs(tl)
                }

              printargs(args)
              operation(s"call i32 (i8*, ...) $printf (i8* bitcast ([2 x i8]* @.nl.format to i8*))")
              UnitType
            case ApplyExpressionAST(fpos, f, apos, args, tailrecursive) =>
              val (func, ftyp) = compileExpression(true, f)
              val rtyp =
                ftyp match {
                  case FunctionType(ret, parms, arb) => ret
                  case a                             => problem(fpos, s"expected function type: $a")
                }
              val argvals =
                for ((p, a) <- args)
                  yield {
                    val (e, t) = compileExpression(true, a)

                    s"$t $e"
                  }

              operation(
                s"call $rtyp (${Iterator.fill(args.length)("i32") mkString ", "}) $func (${argvals mkString ", "})")
              rtyp
            case ComparisonExpressionAST(lpos, left, List((comp, rpos, right))) =>
              compileBinaryExpression(lpos, left, comp, rpos, right)
              BoolType
            case AssignmentExpressionAST(lhs, op, rhs) =>
              val (lpos, l) = lhs.head
              val (rpos, r) = rhs.head

              val (lvalue, ltyp) = compileExpression(false, l)
              val (rvalue, rtyp) = compileExpression(true, r)

              ltyp match {
                case PointerType(typ) =>
                  if (typ != rtyp)
                    problem(rpos, s"incompatible types")
                case _ => problem(lpos, "not an l-value")
              }

              indent(s"store $rtyp $rvalue, $ltyp $lvalue")
              UnitType // should be the resulting value
          }

        (if (typ == UnitType) UnitType.void else expval, typ)
      }

      // todo: assuming return type was given
      line(
        s"define ${datatype(ret.get._1, ret.get._2)} @" ++ name ++ s"(${parmMap map { case (k, v) => s"$v %$k" } mkString ", "}) {")
      line("entry:")

      val (v, t) = compileExpression(true, parts.head.body)

      indent(if (t == UnitType) "ret void" else s"ret $t $v") // todo: convert 't' (expression type) to 'ret' (function's declared type)
      line("}")
    }

    compileSource(as.head)
    out.toString
  }

  abstract class Def { val typ: Type }
  case class VarDef(typ: Type, const: String, ctyp: Type) extends Def
  case class FunctionDef(typ: FunctionType)               extends Def

  class Counter {

    private var count = 0

    def next: Int = {
      count += 1
      count - 1
    }

    override def toString =
      if (count == 0)
        sys.error("no last count")
      else
        s"%${count - 1}"

  }

}
