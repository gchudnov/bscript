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
) extends RealType