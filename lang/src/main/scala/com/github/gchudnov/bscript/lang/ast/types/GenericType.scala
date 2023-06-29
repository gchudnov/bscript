package com.github.gchudnov.bscript.lang.ast.types

/**
 * Generic Type
 */
final case class GenericType(name: String) extends TypeAST:
  override def asString: String =
    name
