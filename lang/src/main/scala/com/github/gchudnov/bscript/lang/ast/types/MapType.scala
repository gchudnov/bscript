package com.github.gchudnov.bscript.lang.ast.types

import com.github.gchudnov.bscript.lang.ast.decls.*

final case class MapType(
  tkey: Option[TypeDecl],
  key: TypeAST,
  tvalue: Option[TypeDecl],
  value: TypeAST
) extends TypeAST
