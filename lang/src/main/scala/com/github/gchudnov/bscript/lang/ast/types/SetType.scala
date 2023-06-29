package com.github.gchudnov.bscript.lang.ast.types

import com.github.gchudnov.bscript.lang.ast.decls.*

/**
  * Set Type
  *
  * @param keyType key type
  */
final case class SetType(
  keyType: TypeAST,
) extends RealType:
  override def asString: String =
    s"set<${keyType.asString}>"

