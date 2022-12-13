package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Double Literal
 *
 * {{{
 *   12.34
 * }}}
 */
final case class DoubleVal(value: Double) extends ConstVal