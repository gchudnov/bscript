package com.github.gchudnov.bscript.translator.internal.c.laws

import com.github.gchudnov.bscript.lang.symbols.{DeclType, Type, VectorType}
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.translator.laws.TypeConverter

/**
 * Scala Type Names NOTE: It is possible that not all types be expressed in Scala
 */
private[internal] final class CTypeConverter(typeNames: TypeNames) extends TypeNames with TypeConverter:
  override val autoType: String     = "NULL"
  override val nothingType: String  = "NULL"
  override val voidType: String     = "void"
  override val boolType: String     = "int"
  override val i32Type: String      = "int32_t"
  override val i64Type: String      = "int64_t"
  override val f32Type: String      = "float"
  override val f64Type: String      = "double"
  override val decType: String      = "double"
  override val strType: String      = "char*"
  override val dateType: String     = "int64_t"
  override val datetimeType: String = "int64_t"

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
        toTypeName(elementType).map(typeName => s"${typeName}") // WRONG: `int[] a` --> should be `int a[]` instead
      case DeclType(expr) =>
        toTypeName(expr.evalType)
      case _ =>
        mapTypeName(t.name)

  /**
   * If this a known type, get it, else return as-is.
   */
  private def mapTypeName(typeName: String): Either[Throwable, String] =
    Right(typeMap.getOrElse(typeName, typeName))
