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

private[internal] object FieldOfDateTime:
  import DateTime.*

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.i32Type),
      "fieldOfDateTime",
      List(
        ArgDecl(TypeRef(typeNames.datetimeType), "value"),
        ArgDecl(TypeRef(typeNames.strType), "unit")
      ),
      Block(
        CompiledExpr(callback = FieldOfDateTime.fieldOfDateTime, retType = TypeRef(typeNames.i32Type))
      ),
      Seq(ComAnn("return the specified part of date-time as an integer value"), StdAnn())
    )

  /**
   * Gets a field from datetime
   *
   * {{{
   *   datetime d1 = "YYYY-MM-DD HH:MM:SS";
   *   int dd = fieldOfDateTime(d1, "days");
   *   int hh = fieldOfDateTime(d1, "hours");
   * }}}
   */
  private def fieldOfDateTime(s: Any): Either[Throwable, Any] =
    val argValue = "value" // datetime
    val argUnit  = "unit"  // string unit of the offset (DAYS | HOURS | MINUTES | SECONDS)

    s match
      case s @ InterpretState(_, ms, c) =>
        for
          valueCell <- ms.fetch(CellPath(argValue))
          unitCell  <- ms.fetch(CellPath(argUnit))

          retVal <- (valueCell, unitCell) match
                      case (DateTimeCell(value), StrCell(unit)) =>
                        unit.trim.toLowerCase match
                          case `unitDays` =>
                            allCatch.either(value.getDayOfMonth).map(IntCell.apply)
                          case `unitHours` =>
                            allCatch.either(value.getHour).map(IntCell.apply)
                          case `unitMinutes` =>
                            allCatch.either(value.getMinute).map(IntCell.apply)
                          case `unitSeconds` =>
                            allCatch.either(value.getSecond).map(IntCell.apply)
                          case other =>
                            Left(new B1Exception(s"Unexpected unit of time was passed to fieldOfDateTime: ${other}"))
                      case other =>
                        Left(new B1Exception(s"Unexpected type of arguments passed to fieldOfDateTime: ${other}"))
        yield s.copy(memSpace = ms, retValue = retVal)

      case _: Scala2State =>
        for lines <- Right(
                       split(
                         s"""${argUnit}.trim.toLowerCase match {
                            |  case `unitDays` =>
                            |    ${argValue}.getDayOfMonth
                            |  case `unitHours` =>
                            |    ${argValue}.getHour
                            |  case `unitMinutes` =>
                            |    ${argValue}.getMinute
                            |  case `unitSeconds` =>
                            |    ${argValue}.getSecond
                            |  case other =>
                            |    throw new RuntimeException(s"Unexpected unit of time was passed to fieldOfDateTime: $${${argUnit}}")
                            |}
                            |""".stripMargin
                       )
                     )
        yield Scala2State(lines = lines)

      case other =>
        Left(new B1Exception(s"Unexpected state passed to fieldOfDateTime: ${other}"))
