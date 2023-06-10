package com.github.gchudnov.bscript.lang.symbols

import com.github.gchudnov.bscript.lang.types.TypeName

/**
 * A built-in type, e.g. int, long, float
 *
 * @param name
 *   name of the type
 */
final case class SType(name: String) extends Symbol with Type
