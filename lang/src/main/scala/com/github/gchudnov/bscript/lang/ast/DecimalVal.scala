package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Decimal Literal
 *
 * {{{
 *   3.3
 * }}}
 */
final case class DecimalVal(value: BigDecimal) extends ConstVal