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

private[internal] object AdjustDateTime:
  import DateTime.*

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.datetimeType),
      "offsetDateTime",
      List(
        ArgDecl(TypeRef(typeNames.datetimeType), "value"),
        ArgDecl(TypeRef(typeNames.i32Type), "offset"),
        ArgDecl(TypeRef(typeNames.strType), "unit")
      ),
      Block(
        CompiledExpr(callback = AdjustDateTime.offsetDateTime, retType = TypeRef(typeNames.datetimeType))
      ),
      Seq(ComAnn("offsets the provided date-time"), StdAnn())
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
                            allCatch.either(value.plusDays(offset.toLong)).map(DateTimeCell.apply)
                          case `unitHours` =>
                            allCatch.either(value.plusHours(offset.toLong)).map(DateTimeCell.apply)
                          case `unitMinutes` =>
                            allCatch.either(value.plusMinutes(offset.toLong)).map(DateTimeCell.apply)
                          case `unitSeconds` =>
                            allCatch.either(value.plusSeconds(offset.toLong)).map(DateTimeCell.apply)
                          case other =>
                            Left(new B1Exception(s"Unexpected unit of time was passed to offsetDateTime: ${other}"))
                      case other =>
                        Left(new B1Exception(s"Unexpected type of arguments passed to offsetDateTime: ${other}"))
        yield s.copy(memSpace = ms, retValue = retVal)

      case _: Scala2State =>
        for lines <- Right(
                       split(
                         s"""${argUnit}.trim.toLowerCase match {
                            |  case `unitDays` =>
                            |    ${argValue}.plusDays(${argOffset}.toLong)
                            |  case `unitHours` =>
                            |    ${argValue}.plusHours(${argOffset}.toLong)
                            |  case `unitMinutes` =>
                            |    ${argValue}.plusMinutes(${argOffset}.toLong)
                            |  case `unitSeconds` =>
                            |    ${argValue}.plusSeconds(${argOffset}.toLong)
                            |  case other =>
                            |    throw new RuntimeException(s"Unexpected unit of time was passed to offsetDateTime: $${${argUnit}}")
                            |}
                            |""".stripMargin
                       )
                     )
        yield Scala2State(lines = lines)

      case other =>
        Left(new B1Exception(s"Unexpected state passed to offsetDateTime: ${other}"))
