package xyz.hyperreal.sysl

abstract class Type { val name: String }

abstract class SimpleType(override val toString: String, val name: String) extends Type

// todo: better Unit type
case object UnitType extends SimpleType("i32", "Unit") {
  val void = "1234567890"
}

abstract class NumericType(llvm: String, name: String) extends SimpleType(llvm, name)

abstract class IntegerType(llvm: String, name: String, val signed: Boolean) extends NumericType(llvm, name)
case object ByteType                                                        extends IntegerType("i8", "Byte", signed = true)
case object ShortType                                                       extends IntegerType("i16", "Short", signed = true)
case object IntType                                                         extends IntegerType("i32", "Int", signed = true)
case object LongType                                                        extends IntegerType("i64", "Long", signed = true)
case object UByteType                                                       extends IntegerType("i8", "UByte", signed = false)
case object UShortType                                                      extends IntegerType("i16", "UShort", signed = false)
case object UIntType                                                        extends IntegerType("i32", "UInt", signed = false)
case object ULongType                                                       extends IntegerType("i64", "ULong", signed = false)
case object CharType                                                        extends IntegerType("i32", "Char", signed = false)

//abstract class AnyIntType extends Type { def llvm: String = sys.error("internal type") }

case object BoolType extends SimpleType("i1", "Bool")

abstract class FloatType(llvm: String, name: String) extends NumericType(llvm, name)
case object SingleType                               extends FloatType("float", "Float")
case object DoubleType                               extends FloatType("double", "Double")
case object QuadrupleType                            extends FloatType("fp128", "Quad")

abstract class DerivedType(override val toString: String, val name: String) extends Type { val typ: Type }

case class ArrayType(size: Int, typ: Type) extends DerivedType(s"[$size x $typ]", s"${typ.name}[$size]")

case class PointerType(typ: Type) extends DerivedType(s"$typ*", s"${typ.name}*")

case class FunctionType(ret: Type, parms: List[Type], arb: Boolean = false) extends Type {
  val name                      = s"(${(parms mkString ", ") ++ (if (arb) "..." else "")}) -> ${ret.name}"
  override def toString: String = s"$ret (${(parms mkString ", ") ++ (if (arb) "..." else "")})"
}
