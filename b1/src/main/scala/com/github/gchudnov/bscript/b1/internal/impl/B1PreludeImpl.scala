package com.github.gchudnov.bscript.b1.internal.impl

import java.time.{ LocalDate, OffsetDateTime, ZoneId }
import scala.util.control.Exception.allCatch

/**
 * Implementation of methods, defined in B1Prelude.
 */
private[b1] object B1PreludeImpl:

  private val utcZone: ZoneId = ZoneId.of("Z")

  private val unitDays: String    = "days"
  private val unitHours: String   = "hours"
  private val unitMinutes: String = "minutes"
  private val unitSeconds: String = "seconds"


// //   /**
// //    * Gets the length of a provided string
// //    *
// //    * {{{
// //    *   string s = "123"
// //    *   int sz = strLen(s);
// //    * }}}
// //    */
// //   private def strLen(s: Any): Either[Throwable, Any] =
// //     val arg0 = "s"

// //     s match
// //       case s @ InterpretState(_, ms, c) =>
// //         for
// //           cell <- ms.fetch(CellPath(arg0))
// //           retVal <- cell match
// //                       case StrCell(value) =>
// //                         Right(IntCell(value.length))
// //                       case other =>
// //                         Left(new CompiledException(s"Unexpected type of arguments passed to strLen: ${other}"))
// //         yield s.copy(memSpace = ms, retValue = retVal)

// //       case _: Scala2State =>
// //         for lines <- Right(
// //                        split(
// //                          s"""${arg0}.length
// //                             |""".stripMargin
// //                        )
// //                      )
// //         yield Scala2State(lines = lines)

// //       case other =>
// //         Left(new CompiledException(s"Unexpected state passed to strLen: ${other}"))

// //   /**
// //    * Adds the duration expressed as (offset, unit) to a datetime.
// //    *
// //    * {{{
// //    *   datetime d1 = "YYYY-MM-DD HH:MM:SS";
// //    *   datetime d2 = offsetDateTime(d1, 12, "hours");
// //    *   datetime d3 = offsetDateTime(d1, 30, "seconds");
// //    * }}}
// //    */
// //   private[visitors] def offsetDateTime(s: Any): Either[Throwable, Any] =
// //     val argValue  = "value"  // datetime
// //     val argOffset = "offset" // integer offset
// //     val argUnit   = "unit"   // string unit of the offset (DAYS | HOURS | MINUTES | SECONDS)

// //     s match
// //       case s @ InterpretState(_, ms, c) =>
// //         for
// //           valueCell  <- ms.fetch(CellPath(argValue))
// //           offsetCell <- ms.fetch(CellPath(argOffset))
// //           unitCell   <- ms.fetch(CellPath(argUnit))

// //           retVal <- (valueCell, offsetCell, unitCell) match
// //                       case (DateTimeCell(value), IntCell(offset), StrCell(unit)) =>
// //                         unit.trim.toLowerCase match
// //                           case `unitDays` =>
// //                             allCatch.either(value.plusDays(offset.toLong)).map(DateTimeCell.apply)
// //                           case `unitHours` =>
// //                             allCatch.either(value.plusHours(offset.toLong)).map(DateTimeCell.apply)
// //                           case `unitMinutes` =>
// //                             allCatch.either(value.plusMinutes(offset.toLong)).map(DateTimeCell.apply)
// //                           case `unitSeconds` =>
// //                             allCatch.either(value.plusSeconds(offset.toLong)).map(DateTimeCell.apply)
// //                           case other =>
// //                             Left(new CompiledException(s"Unexpected unit of time was passed to offsetDateTime: ${other}"))
// //                       case other =>
// //                         Left(new CompiledException(s"Unexpected type of arguments passed to offsetDateTime: ${other}"))
// //         yield s.copy(memSpace = ms, retValue = retVal)

// //       case _: Scala2State =>
// //         for lines <- Right(
// //                        split(
// //                          s"""${argUnit}.trim.toLowerCase match {
// //                             |  case `unitDays` =>
// //                             |    ${argValue}.plusDays(${argOffset}.toLong)
// //                             |  case `unitHours` =>
// //                             |    ${argValue}.plusHours(${argOffset}.toLong)
// //                             |  case `unitMinutes` =>
// //                             |    ${argValue}.plusMinutes(${argOffset}.toLong)
// //                             |  case `unitSeconds` =>
// //                             |    ${argValue}.plusSeconds(${argOffset}.toLong)
// //                             |  case other =>
// //                             |    throw new RuntimeException(s"Unexpected unit of time was passed to offsetDateTime: $${${argUnit}}")
// //                             |}
// //                             |""".stripMargin
// //                        )
// //                      )
// //         yield Scala2State(lines = lines)

// //       case other =>
// //         Left(new CompiledException(s"Unexpected state passed to offsetDateTime: ${other}"))

// //   /**
// //    * Sets the specified part of a datetime
// //    *
// //    * {{{
// //    *   datetime d1 = "YYYY-MM-DD HH:MM:SS";
// //    *   datetime d2 = setDateTime(d1, 12, "hours");
// //    *   datetime d3 = setDateTime(d1, 30, "seconds");
// //    * }}}
// //    */
// //   private[visitors] def setDateTime(s: Any): Either[Throwable, Any] =
// //     val argValue  = "value"  // datetime
// //     val argOffset = "offset" // integer offset
// //     val argUnit   = "unit"   // string unit of the offset (DAYS | HOURS | MINUTES | SECONDS)

// //     s match
// //       case s @ InterpretState(_, ms, c) =>
// //         for
// //           valueCell  <- ms.fetch(CellPath(argValue))
// //           offsetCell <- ms.fetch(CellPath(argOffset))
// //           unitCell   <- ms.fetch(CellPath(argUnit))

// //           retVal <- (valueCell, offsetCell, unitCell) match
// //                       case (DateTimeCell(value), IntCell(offset), StrCell(unit)) =>
// //                         unit.trim.toLowerCase match
// //                           case `unitDays` =>
// //                             allCatch.either(value.withDayOfMonth(offset)).map(DateTimeCell.apply)
// //                           case `unitHours` =>
// //                             allCatch.either(value.withHour(offset)).map(DateTimeCell.apply)
// //                           case `unitMinutes` =>
// //                             allCatch.either(value.withMinute(offset)).map(DateTimeCell.apply)
// //                           case `unitSeconds` =>
// //                             allCatch.either(value.withSecond(offset)).map(DateTimeCell.apply)
// //                           case other =>
// //                             Left(new CompiledException(s"Unexpected unit of time was passed to setDateTime: ${other}"))
// //                       case other =>
// //                         Left(new CompiledException(s"Unexpected type of arguments passed to setDateTime: ${other}"))
// //         yield s.copy(memSpace = ms, retValue = retVal)

// //       case _: Scala2State =>
// //         for lines <- Right(
// //                        split(
// //                          s"""${argUnit}.trim.toLowerCase match {
// //                             |  case `unitDays` =>
// //                             |    ${argValue}.withDayOfMonth(${argOffset})
// //                             |  case `unitHours` =>
// //                             |    ${argValue}.withHour(${argOffset})
// //                             |  case `unitMinutes` =>
// //                             |    ${argValue}.withMinute(${argOffset})
// //                             |  case `unitSeconds` =>
// //                             |    ${argValue}.withSecond(${argOffset})
// //                             |  case other =>
// //                             |    throw new RuntimeException(s"Unexpected unit of time was passed to setDateTime: $${${argUnit}}")
// //                             |}
// //                             |""".stripMargin
// //                        )
// //                      )
// //         yield Scala2State(lines = lines)

// //       case other =>
// //         Left(new CompiledException(s"Unexpected state passed to setDateTime: ${other}"))

// //   /**
// //    * Gets a field from datetime
// //    *
// //    * {{{
// //    *   datetime d1 = "YYYY-MM-DD HH:MM:SS";
// //    *   int dd = fieldOfDateTime(d1, "days");
// //    *   int hh = fieldOfDateTime(d1, "hours");
// //    * }}}
// //    */
// //   private[visitors] def fieldOfDateTime(s: Any): Either[Throwable, Any] =
// //     val argValue = "value" // datetime
// //     val argUnit  = "unit"  // string unit of the offset (DAYS | HOURS | MINUTES | SECONDS)

// //     s match
// //       case s @ InterpretState(_, ms, c) =>
// //         for
// //           valueCell <- ms.fetch(CellPath(argValue))
// //           unitCell  <- ms.fetch(CellPath(argUnit))

// //           retVal <- (valueCell, unitCell) match
// //                       case (DateTimeCell(value), StrCell(unit)) =>
// //                         unit.trim.toLowerCase match
// //                           case `unitDays` =>
// //                             allCatch.either(value.getDayOfMonth).map(IntCell.apply)
// //                           case `unitHours` =>
// //                             allCatch.either(value.getHour).map(IntCell.apply)
// //                           case `unitMinutes` =>
// //                             allCatch.either(value.getMinute).map(IntCell.apply)
// //                           case `unitSeconds` =>
// //                             allCatch.either(value.getSecond).map(IntCell.apply)
// //                           case other =>
// //                             Left(new CompiledException(s"Unexpected unit of time was passed to fieldOfDateTime: ${other}"))
// //                       case other =>
// //                         Left(new CompiledException(s"Unexpected type of arguments passed to fieldOfDateTime: ${other}"))
// //         yield s.copy(memSpace = ms, retValue = retVal)

// //       case _: Scala2State =>
// //         for lines <- Right(
// //                        split(
// //                          s"""${argUnit}.trim.toLowerCase match {
// //                             |  case `unitDays` =>
// //                             |    ${argValue}.getDayOfMonth
// //                             |  case `unitHours` =>
// //                             |    ${argValue}.getHour
// //                             |  case `unitMinutes` =>
// //                             |    ${argValue}.getMinute
// //                             |  case `unitSeconds` =>
// //                             |    ${argValue}.getSecond
// //                             |  case other =>
// //                             |    throw new RuntimeException(s"Unexpected unit of time was passed to fieldOfDateTime: $${${argUnit}}")
// //                             |}
// //                             |""".stripMargin
// //                        )
// //                      )
// //         yield Scala2State(lines = lines)

// //       case other =>
// //         Left(new CompiledException(s"Unexpected state passed to fieldOfDateTime: ${other}"))

// //   /**
// //    * checks that the provided parameter is not null
// //    *
// //    * {{{
// //    *   int x = null;
// //    *   bool xr = isDefined(x); // false
// //    *
// //    *   float y = 12.34;
// //    *   bool yr = isDefined(y); // true
// //    * }}}
// //    */
// //   private def isDefined(s: Any): Either[Throwable, Any] =
// //     val argX = "x" // auto

// //     s match
// //       case s @ InterpretState(_, ms, c) =>
// //         for
// //           xCell <- ms.fetch(CellPath(argX))
// //           flag = xCell match
// //                    case NothingCell =>
// //                      false
// //                    case _ =>
// //                      true
// //           retVal = BoolCell(flag)
// //         yield s.copy(memSpace = ms, retValue = retVal)

// //       case _: Scala2State =>
// //         for lines <- Right(
// //                        split(
// //                          s"""${argX} match {
// //                             |  case null => false
// //                             |  case None => false
// //                             |  case _ => true
// //                             |}
// //                             |""".stripMargin
// //                        )
// //                      )
// //         yield Scala2State(lines = lines)

// //       case other =>
// //         Left(new CompiledException(s"Unexpected state passed to isDefined: ${other}"))

// //   /**
// //    * returns the first argument if not null, otherwise the second one
// //    *
// //    * {{{
// //    *   int x = null;
// //    *   int y = 17;
// //    *   int z = coalesce(x, y);
// //    *   z; // 17
// //    * }}}
// //    */
// //   private def coalesce(s: Any): Either[Throwable, Any] =
// //     val argX = "x" // auto
// //     val argY = "y" // auto

// //     s match
// //       case s @ InterpretState(_, ms, c) =>
// //         for
// //           xCell <- ms.fetch(CellPath(argX))
// //           yCell <- ms.fetch(CellPath(argY))
// //           retVal = (xCell, yCell) match
// //                      case (NothingCell, y) =>
// //                        y
// //                      case (x, _) =>
// //                        x
// //         yield s.copy(memSpace = ms, retValue = retVal)

// //       case _: Scala2State =>
// //         for lines <- Right(
// //                        split(
// //                          s"""(${argX}, ${argY}) match {
// //                             |  case (null, _) => ${argY}
// //                             |  case (None, _) => ${argY}
// //                             |  case _ => ${argX}
// //                             |}
// //                             |""".stripMargin
// //                        )
// //                      )
// //         yield Scala2State(lines = lines)

// //       case other =>
// //         Left(new CompiledException(s"Unexpected state passed to coalesce: ${other}"))

// //   /**
// //    * Returns current date in UTC format: YYYY-MM-DD
// //    */
// //   private def today(s: Any): Either[Throwable, Any] =
// //     s match
// //       case s @ InterpretState(_, ms, c) =>
// //         for retVal <- Right(DateCell(LocalDate.now(utcZone)))
// //         yield s.copy(memSpace = ms, retValue = retVal)

// //       case _: Scala2State =>
// //         for lines <- Right(
// //                        split(
// //                          s"""LocalDate.now(ZoneId.of("Z"))
// //                             |""".stripMargin
// //                        )
// //                      )
// //         yield Scala2State(lines = lines)

// //       case other =>
// //         Left(new CompiledException(s"Unexpected state passed to today: ${other}"))

// //   /**
// //    * Returns current datetime in UTC format: 2021-12-11T13:20:13.521645485Z
// //    */
// //   private def now(s: Any): Either[Throwable, Any] =
// //     s match
// //       case s @ InterpretState(_, ms, c) =>
// //         for retVal <- Right(DateTimeCell(OffsetDateTime.now(utcZone)))
// //         yield s.copy(memSpace = ms, retValue = retVal)

// //       case _: Scala2State =>
// //         for lines <- Right(
// //                        split(
// //                          s"""OffsetDateTime.now(ZoneId.of("Z"))
// //                             |""".stripMargin
// //                        )
// //                      )
// //         yield Scala2State(lines = lines)

// //       case other =>
// //         Left(new CompiledException(s"Unexpected state passed to now: ${other}"))

// //   /**
// //    * Returns the rounded numerical value (3.1234, 2) -> 3.12 (3.1264, 2) -> 3.13
// //    */
// //   private[visitors] def round(s: Any): Either[Throwable, Any] =
// //     val argValue     = "value"     // auto: f32, f64, dec
// //     val argPrecision = "precision" // i32

// //     def roundF64(n: Double, p: Int): Double =
// //       val s: Double = math.pow(10.toDouble, p.toDouble)
// //       math.round(n * s) / s

// //     def roundF32(n: Float, p: Int): Float =
// //       roundF64(n.toDouble, p).toFloat

// //     def roundDec(n: BigDecimal, p: Int): BigDecimal =
// //       n.setScale(p, BigDecimal.RoundingMode.HALF_UP)

// //     s match
// //       case s @ InterpretState(_, ms, c) =>
// //         for
// //           valueCell     <- ms.fetch(CellPath(argValue))
// //           precisionCell <- ms.fetch(CellPath(argPrecision))
// //           retVal <- (valueCell, precisionCell) match
// //                       case (FloatCell(x), IntCell(p)) =>
// //                         Right(FloatCell(roundF32(x, p)))
// //                       case (DoubleCell(x), IntCell(p)) =>
// //                         Right(DoubleCell(roundF64(x, p)))
// //                       case (DecimalCell(x), IntCell(p)) =>
// //                         Right(DecimalCell(roundDec(x, p)))
// //                       case other =>
// //                         Left(new CompiledException(s"Unexpected type of arguments passed to round: ${other}"))
// //         yield s.copy(memSpace = ms, retValue = retVal)

// //       case _: Scala2State =>
// //         for lines <- Right(
// //                        split(
// //                          s"""${argValue}.setScale(${argPrecision}, BigDecimal.RoundingMode.HALF_UP)
// //                             |""".stripMargin
// //                        )
// //                      )
// //         yield Scala2State(lines = lines)

// //       case other =>
// //         Left(new CompiledException(s"Unexpected state passed to round: ${other}"))

// //   /**
// //    * Returns the truncated numerical value (3.1234, 2) -> 3.12 (3.1264, 2) -> 3.12
// //    */
// //   private def truncate(s: Any): Either[Throwable, Any] =
// //     val argValue     = "value"     // auto: f32, f64, dec
// //     val argPrecision = "precision" // i32

// //     def truncateF64(n: Double, p: Int): Double =
// //       val s: Double = math.pow(10.toDouble, p.toDouble)
// //       if n < 0.0 then math.ceil(n * s) / s
// //       else math.round(n * s) / s

// //     def truncateF32(n: Float, p: Int): Float =
// //       truncateF64(n.toDouble, p).toFloat

// //     def truncateDec(n: BigDecimal, p: Int): BigDecimal =
// //       n.setScale(p, BigDecimal.RoundingMode.DOWN)

// //     s match
// //       case s @ InterpretState(_, ms, c) =>
// //         for
// //           valueCell     <- ms.fetch(CellPath(argValue))
// //           precisionCell <- ms.fetch(CellPath(argPrecision))
// //           retVal <- (valueCell, precisionCell) match
// //                       case (FloatCell(x), IntCell(p)) =>
// //                         Right(FloatCell(truncateF32(x, p)))
// //                       case (DoubleCell(x), IntCell(p)) =>
// //                         Right(DoubleCell(truncateF64(x, p)))
// //                       case (DecimalCell(x), IntCell(p)) =>
// //                         Right(DecimalCell(truncateDec(x, p)))
// //                       case other =>
// //                         Left(new CompiledException(s"Unexpected type of arguments passed to truncate: ${other}"))
// //         yield s.copy(memSpace = ms, retValue = retVal)

// //       case _: Scala2State =>
// //         for lines <- Right(
// //                        split(
// //                          s"""${argValue}.setScale(${argPrecision}, BigDecimal.RoundingMode.DOWN)
// //                             |""".stripMargin
// //                        )
// //                      )
// //         yield Scala2State(lines = lines)

// //       case other =>
// //         Left(new CompiledException(s"Unexpected state passed to truncate: ${other}"))
