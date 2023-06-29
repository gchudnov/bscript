package com.github.gchudnov.bscript.lang.ast.types

/**
 * Generic Type
 */
final case class GenericType(name: String) extends RealType:
  override def asString: String =
    name
