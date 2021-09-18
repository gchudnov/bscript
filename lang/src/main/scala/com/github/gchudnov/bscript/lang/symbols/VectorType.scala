package com.github.gchudnov.bscript.lang.symbols

/**
 * Vector Type.
 *
 * The element of the vector is inside of the type
 */
final case class VectorType(elementType: Type) extends Type:
  override def name: String =
    s"[]${elementType.name}"

  override def toString: String =
    name
