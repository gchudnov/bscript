package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * String Literal
 *
 * {{{
 *   "a"
 * }}}
 */
final case class StrVal(value: String) extends ConstVal