package com.github.gchudnov.bscript.lang.symbols

/**
 * A built-in type, e.g. int, long, float
 *
 * @param name
 *   name of the type
 */
final case class SBuiltIn(name: String) extends Symbol with Type
