package com.github.gchudnov.bscript.lang.const

import java.time.LocalDate

/**
 * Date Literal
 *
 * {{{
 *   2021-10-12
 * }}}
 */
final case class DateVal(value: LocalDate) extends Const