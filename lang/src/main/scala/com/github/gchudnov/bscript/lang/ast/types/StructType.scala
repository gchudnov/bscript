package com.github.gchudnov.bscript.lang.ast.types

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.decls.*

final case class StructType(
  tfields: List[TypeDecl],
  fields: List[VarDecl]
) extends TypeAST
