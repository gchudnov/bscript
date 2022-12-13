package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

import java.time.LocalDate

/**
 * Date Literal
 *
 * {{{
 *   2021-10-12
 * }}}
 */
final case class DateVal(value: LocalDate) extends ConstVal