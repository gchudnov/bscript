package com.github.gchudnov.bscript.lang.ast.types

/**
 * Built-In Type
 */
final case class BuiltInType(name: String) extends TypeAST:
  override def asString: String =
    name
