package com.github.gchudnov.bscript.lang.ast.types

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.decls.*

/**
 * Struct Type
 *
 * @param tfields
 *   type fields
 * @param fields
 *   fields
 */
final case class StructType(
  tfields: List[TypeDecl],
  fields: List[VarDecl],
) extends RealType:
  override def asString: String =
    val tfieldsStr = tfields.zipWithIndex.map { case (t, i) => ('A' + i).toChar.toString }.mkString(", ")
    val fieldsStr  = fields.map(_.aType.asString).mkString(", ")
    s"struct<$tfieldsStr> { $fieldsStr }"
