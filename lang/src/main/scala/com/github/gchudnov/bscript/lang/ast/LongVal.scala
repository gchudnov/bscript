package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Long Literal
 *
 * {{{
 *   12
 * }}}
 */
final case class LongVal(value: Long) extends ConstVal