package com.github.gchudnov.bscript.lang.const

/**
 * Decimal Literal
 *
 * {{{
 *   3.3
 * }}}
 */
final case class DecVal(value: BigDecimal) extends Const