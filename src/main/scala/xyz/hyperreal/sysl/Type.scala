package xyz.hyperreal.sysl

abstract class Type { def llvm: String }

abstract class SimpleType(val llvm: String) extends Type

case object VoidType extends SimpleType("void")

abstract class IntegerType(llvm: String, val signed: Boolean) extends SimpleType(llvm)
case object ByteType                                          extends IntegerType("i8", signed = true)
case object ShortType                                         extends IntegerType("i16", signed = true)
case object IntType                                           extends IntegerType("i32", signed = true)
case object LongType                                          extends IntegerType("i64", signed = true)
case object UByteType                                         extends IntegerType("i8", signed = false)
case object UShortType                                        extends IntegerType("i16", signed = false)
case object UIntType                                          extends IntegerType("i32", signed = false)
case object ULongType                                         extends IntegerType("i64", signed = false)
case object CharType                                          extends IntegerType("i32", signed = false)

case object BoolType extends SimpleType("i1")

abstract class FloatType(llvm: String) extends SimpleType(llvm)
case object SingleType                 extends FloatType("float")
case object DoubleType                 extends FloatType("double")
case object QuadrupleType              extends FloatType("fp128")

abstract class DerivedType(val llvm: String) extends Type { val typ: Type }

case class ArrayType(size: Int, typ: Type) extends DerivedType(s"[$size x $typ]")

case class PointerType(typ: Type) extends DerivedType(s"$typ*")

case class FunctionType(ret: Type, parms: List[Type], arb: Boolean = false) extends Type {
  val llvm = s"$ret (${(parms mkString ", ") ++ (if (arb) "..." else "")})"
}
