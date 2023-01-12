package com.github.gchudnov.bscript.lang.ast.types

final case class VecType(
  elemType: TypeAST
) extends TypeAST
