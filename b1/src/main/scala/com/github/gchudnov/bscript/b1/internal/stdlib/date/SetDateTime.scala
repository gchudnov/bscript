package com.github.gchudnov.bscript.b1.internal.stdlib.date

import com.github.gchudnov.bscript.b1.B1Exception
import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State
import com.github.gchudnov.bscript.translator.internal.scala3j.Scala3JState

import scala.util.control.Exception.allCatch

private[internal] object SetDateTime:
  import DateTime.*

  private val fnName = "setDateTime"

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
        CompiledExpr(callback = SetDateTime.setDateTime, retType = TypeRef(typeNames.datetimeType))
      ),
      Seq(ComAnn("Sets a field of datetime to the specified value"), StdAnn())
    )

  /**
   * Sets the specified part of a datetime
   *
   * {{{
   *   datetime d1 = "YYYY-MM-DD HH:MM:SS";
   *   datetime d2 = setDateTime(d1, 12, "hours");
   *   datetime d3 = setDateTime(d1, 30, "seconds");
   * }}}
   */
  private def setDateTime(s: Any): Either[Throwable, Any] =
    val argValue  = "value"  // datetime
    val argOffset = "offset" // integer offset
    val argUnit   = "unit"   // string unit of the offset (DAYS | HOURS | MINUTES | SECONDS)

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
                            allCatch.either(value.withDayOfMonth(offset)).map(DateTimeCell.apply)
                          case `unitHours` =>
                            allCatch.either(value.withHour(offset)).map(DateTimeCell.apply)
                          case `unitMinutes` =>
                            allCatch.either(value.withMinute(offset)).map(DateTimeCell.apply)
                          case `unitSeconds` =>
                            allCatch.either(value.withSecond(offset)).map(DateTimeCell.apply)
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
                            |    ${argValue}.withDayOfMonth(${argOffset})
                            |  case `unitHours` =>
                            |    ${argValue}.withHour(${argOffset})
                            |  case `unitMinutes` =>
                            |    ${argValue}.withMinute(${argOffset})
                            |  case `unitSeconds` =>
                            |    ${argValue}.withSecond(${argOffset})
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
                            |${argUnit}.trim.toLowerCase match {
                            |  case `unitDays` =>
                            |    ${argValue}.withDayOfMonth(${argOffset})
                            |  case `unitHours` =>
                            |    ${argValue}.withHour(${argOffset})
                            |  case `unitMinutes` =>
                            |    ${argValue}.withMinute(${argOffset})
                            |  case `unitSeconds` =>
                            |    ${argValue}.withSecond(${argOffset})
                            |  case _ =>
                            |    throw new RuntimeException(s"Unexpected date-time unit passed to ${fnName}: '$${${argUnit}}'")
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines, imports = s.imports ++ Set("java.time.OffsetDateTime", "java.lang.String as JString", "java.lang.Integer as JInteger"))

      case other =>
        Left(new B1Exception(s"Unexpected state passed to ${fnName}: ${other}"))
