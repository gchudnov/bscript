package com.github.gchudnov.bscript.lang.symbols

/**
 * Reference to a type
 * @param name
 *   Name of the type that is defined in one of the scopes
 */
final case class TypeRef(name: String) extends Type
