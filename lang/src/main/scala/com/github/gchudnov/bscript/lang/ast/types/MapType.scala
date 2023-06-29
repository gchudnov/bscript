package com.github.gchudnov.bscript.lang.ast.types

import com.github.gchudnov.bscript.lang.ast.decls.*

/**
  * Map Type
  *
  * @param keyType key type
  * @param valueType value type
  */
final case class MapType(
  keyType: TypeAST,
  valueType: TypeAST
) extends RealType