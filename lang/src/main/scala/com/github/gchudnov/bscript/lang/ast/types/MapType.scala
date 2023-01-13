package com.github.gchudnov.bscript.lang.ast.types

import com.github.gchudnov.bscript.lang.ast.decls.*

final case class MapType(
  keyType: TypeAST,
  valueType: TypeAST
) extends TypeAST
