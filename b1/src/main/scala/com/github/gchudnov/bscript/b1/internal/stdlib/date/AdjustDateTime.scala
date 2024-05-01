package com.github.gchudnov.bscript.b1.internal.stdlib.date

import com.github.gchudnov.bscript.b1.B1Exception
import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State
import com.github.gchudnov.bscript.translator.internal.scalax.scala3j.Scala3JState

import java.time.OffsetDateTime
import scala.util.control.Exception.allCatch

private[internal] object AdjustDateTime:
  import DateTime.*

  private val fnName = "offsetDateTime"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.datetimeType),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.datetimeType), "value"),
        ArgDecl(TypeRef(typeNames.i32Type), "offset"),
        ArgDecl(TypeRef(typeNames.strType), "unit")
      ),
      Block(
        CompiledExpr(callback = AdjustDateTime.offsetDateTime, retType = TypeRef(typeNames.datetimeType))
      ),
      Seq(ComAnn("Offsets the provided date-time"), StdAnn())
    )

  /**
   * Adds the duration expressed as (offset, unit) to a datetime.
   *
   * {{{
   *   datetime d1 = "YYYY-MM-DD HH:MM:SS";
   *   datetime d2 = offsetDateTime(d1, 12, "hours");
   *   datetime d3 = offsetDateTime(d1, 30, "seconds");
   * }}}
   */
  private def offsetDateTime(s: Any): Either[Throwable, Any] =
    val argValue  = "value"  // datetime
    val argOffset = "offset" // integer offset
    val argUnit   = "unit"   // string unit of the offset (DAYS | HOURS | MINUTES | SECONDS)

    def dateTimePlusDays(x: OffsetDateTime, offset: Int): Either[Throwable, OffsetDateTime] =
      allCatch.either(x.plusDays(offset.toLong))

    def dateTimePlusHours(x: OffsetDateTime, offset: Int): Either[Throwable, OffsetDateTime] =
      allCatch.either(x.plusHours(offset.toLong))

    def dateTimePlusMinutes(x: OffsetDateTime, offset: Int): Either[Throwable, OffsetDateTime] =
      allCatch.either(x.plusMinutes(offset.toLong))

    def dateTimePlusSeconds(x: OffsetDateTime, offset: Int): Either[Throwable, OffsetDateTime] =
      allCatch.either(x.plusSeconds(offset.toLong))

    s match
      case s @ InterpretState(_, _, ms, c) =>
        for
          valueCell  <- ms.tryFetch(CellPath(argValue))
          offsetCell <- ms.tryFetch(CellPath(argOffset))
          unitCell   <- ms.tryFetch(CellPath(argUnit))

          retVal <- (valueCell, offsetCell, unitCell) match
                      case (DateTimeCell(value), IntCell(offset), StrCell(unit)) =>
                        unit.trim.toLowerCase match
                          case `unitDays` =>
                            dateTimePlusDays(value, offset).map(DateTimeCell.apply)
                          case `unitHours` =>
                            dateTimePlusHours(value, offset).map(DateTimeCell.apply)
                          case `unitMinutes` =>
                            dateTimePlusMinutes(value, offset).map(DateTimeCell.apply)
                          case `unitSeconds` =>
                            dateTimePlusSeconds(value, offset).map(DateTimeCell.apply)
                          case other =>
                            Left(new B1Exception(s"Unexpected date-time unit passed to ${fnName}: '${other}'"))
                      case other =>
                        Left(new B1Exception(s"Unexpected type of arguments passed to ${fnName}: ${other}"))
        yield s.copy(memSpace = ms, retValue = retVal)

      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""val unitDays: String    = "${unitDays}"
                            |val unitHours: String   = "${unitHours}"
                            |val unitMinutes: String = "${unitMinutes}"
                            |val unitSeconds: String = "${unitSeconds}"
                            |
                            |${argUnit}.trim.toLowerCase match {
                            |  case `unitDays` =>
                            |    ${argValue}.plusDays(${argOffset}.toLong)
                            |  case `unitHours` =>
                            |    ${argValue}.plusHours(${argOffset}.toLong)
                            |  case `unitMinutes` =>
                            |    ${argValue}.plusMinutes(${argOffset}.toLong)
                            |  case `unitSeconds` =>
                            |    ${argValue}.plusSeconds(${argOffset}.toLong)
                            |  case _ =>
                            |    throw new RuntimeException(s"Unexpected date-time unit passed to ${fnName}: '$${${argUnit}}'")
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines, imports = s.imports + "java.time.OffsetDateTime")

      case s: Scala3JState =>
        for lines <- Right(
                       split(
                         s"""val unitDays: JString    = "${unitDays}"
                            |val unitHours: JString   = "${unitHours}"
                            |val unitMinutes: JString = "${unitMinutes}"
                            |val unitSeconds: JString = "${unitSeconds}"
                            |
                            |def toJLong(x: JInteger): JLong =
                            |  if (x == null) then null else x.longValue()
                            |
                            |val longOffset = toJLong(${argOffset})
                            |
                            |${argUnit}.trim.toLowerCase match {
                            |  case `unitDays` =>
                            |    ${argValue}.plusDays(longOffset)
                            |  case `unitHours` =>
                            |    ${argValue}.plusHours(longOffset)
                            |  case `unitMinutes` =>
                            |    ${argValue}.plusMinutes(longOffset)
                            |  case `unitSeconds` =>
                            |    ${argValue}.plusSeconds(longOffset)
                            |  case _ =>
                            |    throw new RuntimeException(s"Unexpected date-time unit passed to ${fnName}: '$${${argUnit}}'")
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(
          lines = lines,
          imports = s.imports ++ Set("java.time.OffsetDateTime", "java.lang.String as JString", "java.lang.Integer as JInteger", "java.lang.Long as JLong")
        )

      case other =>
        Left(new B1Exception(s"Unexpected state passed to ${fnName}: ${other}"))
