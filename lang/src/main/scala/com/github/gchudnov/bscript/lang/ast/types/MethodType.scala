package com.github.gchudnov.bscript.lang.ast.types

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.decls.*

final case class MethodType(
  tparams: List[TypeDecl],
  params: List[VarDecl],
  retType: TypeAST
) extends TypeAST
