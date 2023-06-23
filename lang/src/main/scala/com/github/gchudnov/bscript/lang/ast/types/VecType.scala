package com.github.gchudnov.bscript.lang.ast.types

/**
  * Vector Type
  *
  * @param elemType element type
  */
final case class VecType(
  elemType: TypeAST
) extends TypeAST:
  override def asString: String =
    s"vec<${elemType.asString}>"
