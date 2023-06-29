package com.github.gchudnov.bscript.lang.ast.types

/**
 * Built-In Type
 */
final case class BuiltInType(name: String) extends RealType:
  override def asString: String =
    name
