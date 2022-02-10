package com.github.gchudnov.bscript.b1.internal.stdlib.date

import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.translator.internal.scala2.Scala2State
import com.github.gchudnov.bscript.b1.B1Exception
import com.github.gchudnov.bscript.lang.util.ShowOps.split
import com.github.gchudnov.bscript.interpreter.memory.*
import scala.util.control.Exception.allCatch
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames

private[internal] object SetDateTime:
  import DateTime.*

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.datetimeType),
      "setDateTime",
      List(
        ArgDecl(TypeRef(typeNames.datetimeType), "value"),
        ArgDecl(TypeRef(typeNames.i32Type), "offset"),
        ArgDecl(TypeRef(typeNames.strType), "unit")
      ),
      Block(
        CompiledExpr(callback = SetDateTime.setDateTime, retType = TypeRef(typeNames.datetimeType))
      ),
      Seq(ComAnn("sets data and time to the specified value"), StdAnn())
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
      case s @ InterpretState(_, ms, c) =>
        for
          valueCell  <- ms.fetch(CellPath(argValue))
          offsetCell <- ms.fetch(CellPath(argOffset))
          unitCell   <- ms.fetch(CellPath(argUnit))

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
                            Left(new B1Exception(s"Unexpected unit of time was passed to setDateTime: ${other}"))
                      case other =>
                        Left(new B1Exception(s"Unexpected type of arguments passed to setDateTime: ${other}"))
        yield s.copy(memSpace = ms, retValue = retVal)

      case s: Scala2State =>
        for lines <- Right(
                       split(
                         s"""${argUnit}.trim.toLowerCase match {
                            |  case `unitDays` =>
                            |    ${argValue}.withDayOfMonth(${argOffset})
                            |  case `unitHours` =>
                            |    ${argValue}.withHour(${argOffset})
                            |  case `unitMinutes` =>
                            |    ${argValue}.withMinute(${argOffset})
                            |  case `unitSeconds` =>
                            |    ${argValue}.withSecond(${argOffset})
                            |  case other =>
                            |    throw new RuntimeException(s"Unexpected unit of time was passed to setDateTime: $${${argUnit}}")
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case other =>
        Left(new B1Exception(s"Unexpected state passed to setDateTime: ${other}"))
