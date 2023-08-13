package com.github.gchudnov.bscript.lang.symbols

/**
 * A method symbol
 * 
 * {{{
 *   NAME, (int, string)
 * }}}
 * 
 * @param name
 *   name
 * @param signature
 *   signature
 */
final case class SMethod(name: String, signature: String) extends Symbol
