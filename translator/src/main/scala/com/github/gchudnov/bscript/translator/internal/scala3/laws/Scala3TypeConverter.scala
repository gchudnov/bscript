package com.github.gchudnov.bscript.translator.internal.scala3.laws

import com.github.gchudnov.bscript.translator.laws.TypeConverter
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, Type, VectorType }
import com.github.gchudnov.bscript.lang.types.TypeNames

/**
 * Scala Type Names NOTE: It is possible that not all types be expressed in Scala
 */
final class Scala3TypeConverter(typeNames: TypeNames) extends TypeNames with TypeConverter:
  override val autoType: String     = "T"
  override val nothingType: String  = "Nothing"
  override val voidType: String     = "Unit"
  override val boolType: String     = "Boolean"
  override val i32Type: String      = "Int"
  override val i64Type: String      = "Long"
  override val f32Type: String      = "Float"
  override val f64Type: String      = "Double"
  override val decType: String      = "BigDecimal"
  override val strType: String      = "String"
  override val dateType: String     = "LocalDate"
  override val datetimeType: String = "OffsetDateTime"

  override val trueValue: String  = "true"
  override val falseValue: String = "false"

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
        toTypeName(elementType).map(typeName => s"List[${typeName}]")
      case DeclType(expr) =>
        toTypeName(expr.evalType)
      case _ =>
        mapTypeName(t.name)

  /**
   * If this a known type, get it, else return as-is.
   */
  private def mapTypeName(typeName: String): Either[Throwable, String] =
    Right(typeMap.getOrElse(typeName, typeName))
