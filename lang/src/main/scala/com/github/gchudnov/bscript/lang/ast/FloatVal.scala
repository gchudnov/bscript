package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Float Literal
 *
 * {{{
 *   12.34f
 * }}}
 */
final case class FloatVal(value: Float) extends ConstVal