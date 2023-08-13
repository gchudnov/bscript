package com.github.gchudnov.bscript.lang.const

/**
 * Short Literal
 *
 * {{{
 *   0 <= value <= 0xFFFF
 * }}}
 */
final case class ShortVal(value: Short) extends Const
