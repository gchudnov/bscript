package com.github.gchudnov.bscript.b1.internal.stdlib.date

import java.time.{ LocalDate, OffsetDateTime, ZoneId }

object DateTime:
  val utcZone: ZoneId = ZoneId.of("Z")

  val unitDays: String    = "days"
  val unitHours: String   = "hours"
  val unitMinutes: String = "minutes"
  val unitSeconds: String = "seconds"
