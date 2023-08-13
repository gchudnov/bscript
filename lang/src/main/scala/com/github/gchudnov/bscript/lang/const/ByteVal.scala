package com.github.gchudnov.bscript.lang.const

/**
 * Byte Literal
 *
 * {{{
 *   0 <= value <= 255
 * }}}
 */
final case class ByteVal(value: Byte) extends Const
