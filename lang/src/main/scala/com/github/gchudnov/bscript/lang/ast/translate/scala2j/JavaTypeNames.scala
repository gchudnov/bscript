package com.github.gchudnov.bscript.lang.ast.translate.scala2j

import com.github.gchudnov.bscript.lang.ast.translate.TranslateType
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, Type, VectorType }
import com.github.gchudnov.bscript.lang.types.TypeNames

/**
 * Java Type Names NOTE: it is possible that not all types might be expressed in Java
 */
class JavaTypeNames(typeNames: TypeNames) extends TypeNames with TranslateType:
  override val autoType: String     = "T"
  override val nothingType: String  = "???"
  override val voidType: String     = "Void"
  override val boolType: String     = "Boolean"
  override val i32Type: String      = "Integer"
  override val i64Type: String      = "Long"
  override val f32Type: String      = "Float"
  override val f64Type: String      = "Double"
  override val decType: String      = "BigDecimal"
  override val strType: String      = "String"
  override val dateType: String     = "LocalDate"
  override val datetimeType: String = "OffsetDateTime"

  override val boolTrue: String  = "true"
  override val boolFalse: String = "false"

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

  override def toLangTypeName(t: Type): Either[Throwable, String] =
    t match
      case VectorType(elementType) =>
        toLangTypeName(elementType).map(typeName => s"List[${typeName}]")
      case DeclType(expr) =>
        toLangTypeName(expr.evalType)
      case _ =>
        mapTypeName(t.name)

  /**
   * If this a known type, get it, else return as-is.
   */
  private def mapTypeName(typeName: String): Either[Throwable, String] =
    Right(typeMap.getOrElse(typeName, typeName))
