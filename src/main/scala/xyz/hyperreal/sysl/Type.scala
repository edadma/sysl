package xyz.hyperreal.sysl

abstract class Type { val name: String; val firstclass = true }

abstract class SimpleType(override val toString: String, val name: String) extends Type

// todo: better Unit type
case object UnitType extends SimpleType("i32", "unit") {
  val void = "1234567890"
}

abstract class NumericType(llvm: String, name: String) extends SimpleType(llvm, name)

abstract class IntegerType(llvm: String, name: String, val signed: Boolean) extends NumericType(llvm, name)
case object ByteType                                                        extends IntegerType("i8", "byte", signed = true)
case object ShortType                                                       extends IntegerType("i16", "short", signed = true)
case object IntType                                                         extends IntegerType("i32", "int", signed = true)
case object LongType                                                        extends IntegerType("i64", "long", signed = true)
case object UByteType                                                       extends IntegerType("i8", "ubyte", signed = false)
case object UShortType                                                      extends IntegerType("i16", "ushort", signed = false)
case object UIntType                                                        extends IntegerType("i32", "uint", signed = false)
case object ULongType                                                       extends IntegerType("i64", "ulong", signed = false)
case object BCharType                                                       extends IntegerType("i8", "bchar", signed = false)
case object CharType                                                        extends IntegerType("i32", "char", signed = false)

case object BoolType extends SimpleType("i1", "bool")

abstract class FloatType(llvm: String, name: String) extends NumericType(llvm, name)
case object SingleType                               extends FloatType("float", "float")
case object DoubleType                               extends FloatType("double", "double")
case object QuadrupleType                            extends FloatType("fp128", "quad")

abstract class DerivedType(override val toString: String, val name: String) extends Type { val typ: Type }

case class ArrayType(size: Int, typ: Type) extends DerivedType(s"[$size x $typ]", s"${typ.name}[$size]") {
  override val firstclass: Boolean = false
}

case class PointerType(typ: Type) extends DerivedType(s"$typ*", s"${typ.name}*")

case class FunctionType(ret: Type, parms: List[Type], arb: Boolean = false) extends Type {
  val name                         = s"(${(parms mkString ", ") ++ (if (arb) "..." else "")}) -> ${ret.name}"
  override val firstclass: Boolean = false

  override def toString: String = s"$ret (${(parms mkString ", ") ++ (if (arb) "..." else "")})"
}
