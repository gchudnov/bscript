package com.github.gchudnov.bscript.lang.const

import java.time.OffsetDateTime

/**
 * DateTime Literal
 *
 * {{{
 *   "2007-12-03T10:15:30+01:00"
 * }}}
 */
final case class DateTimeVal(value: OffsetDateTime) extends Const
