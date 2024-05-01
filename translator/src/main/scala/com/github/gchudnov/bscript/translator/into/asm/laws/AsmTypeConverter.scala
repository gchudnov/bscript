package com.github.gchudnov.bscript.translator.into.asm.laws

import com.github.gchudnov.bscript.lang.symbols.{DeclType, Type, VectorType}
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.translator.laws.TypeConverter

/**
 * Scala Type Names NOTE: It is possible that not all types be expressed in Scala
 */
private[into] final class AsmTypeConverter(typeNames: TypeNames) extends TypeNames with TypeConverter:
  override val autoType: String     = "auto"
  override val nothingType: String  = "null"
  override val voidType: String     = "void"
  override val boolType: String     = "bool"
  override val i32Type: String      = "i32"
  override val i64Type: String      = "i64"
  override val f32Type: String      = "f32"
  override val f64Type: String      = "f64"
  override val decType: String      = "f64"
  override val strType: String      = "string"
  override val dateType: String     = "Date"
  override val datetimeType: String = "Date"

  override val trueValue: String  = "true"
  override val falseValue: String = "false"

  private val i32ArrType: String = "Int32Array"
  private val i64ArrType: String = "Int64Array"
  private val f32ArrType: String = "Float32Array"
  private val f64ArrType: String = "Float64Array"

  private val typeMap = Map(
    typeNames.autoType     -> autoType,
    typeNames.nothingType  -> nothingType,
    typeNames.voidType     -> voidType,
    typeNames.boolType     -> boolType,
    typeNames.i32Type      -> i32Type,
    typeNames.i64Type      -> i64Type,
    typeNames.f32Type      -> f32Type,
    typeNames.f64Type      -> f64Type,
    typeNames.decType      -> decType,
    typeNames.strType      -> strType,
    typeNames.dateType     -> dateType,
    typeNames.datetimeType -> datetimeType
  )

  override def toTypeName(t: Type): Either[Throwable, String] =
    t match
      case VectorType(elementType) =>
        toTypeName(elementType).map(typeName => s"Array<${typeName}>")
      case DeclType(expr) =>
        toTypeName(expr.evalType)
      case _ =>
        mapTypeName(t.name)

  /**
   * If this a known type, get it, else return as-is.
   */
  private def mapTypeName(typeName: String): Either[Throwable, String] =
    Right(typeMap.getOrElse(typeName, typeName))
