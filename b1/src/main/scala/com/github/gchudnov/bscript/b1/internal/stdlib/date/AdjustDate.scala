package com.github.gchudnov.bscript.b1.internal.stdlib.date

import com.github.gchudnov.bscript.b1.B1Exception
import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

import java.time.LocalDate
import scala.util.control.Exception.allCatch

private[internal] object AdjustDate:
  import DateTime.*

  private val fnName = "offsetDate"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.dateType),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.dateType), "value"),
        ArgDecl(TypeRef(typeNames.i32Type), "offset"),
        ArgDecl(TypeRef(typeNames.strType), "unit")
      ),
      Block(
        CompiledExpr(callback = AdjustDate.offsetDate, retType = TypeRef(typeNames.dateType))
      ),
      Seq(ComAnn("Offsets the provided date"), StdAnn())
    )

  /**
   * Adds the duration expressed as (offset, unit) to a date.
   *
   * {{{
   *   date d1 = "YYYY-MM-DD";
   *   date d2 = offsetDate(d1, 1, "days");
   *   date d3 = offsetDate(d1, -3, "days");
   * }}}
   */
  private def offsetDate(s: Any): Either[Throwable, Any] =
    val argValue  = "value"  // date
    val argOffset = "offset" // integer offset
    val argUnit   = "unit"   // string unit of the offset (DAYS)

    def datePlusDays(x: LocalDate, offset: Int): Either[Throwable, LocalDate] =
      allCatch.either(x.plusDays(offset.toLong))

    s match
      case s @ InterpretState(_, _, ms, c) =>
        for
          valueCell  <- ms.tryFetch(CellPath(argValue))
          offsetCell <- ms.tryFetch(CellPath(argOffset))
          unitCell   <- ms.tryFetch(CellPath(argUnit))

          retVal <- (valueCell, offsetCell, unitCell) match
                      case (DateCell(value), IntCell(offset), StrCell(unit)) =>
                        unit.trim.toLowerCase match
                          case `unitDays` =>
                            datePlusDays(value, offset).map(DateCell.apply)
                          case other =>
                            Left(new B1Exception(s"Unexpected date-time unit passed to ${fnName}: '${other}'"))
                      case other =>
                        Left(new B1Exception(s"Unexpected type of arguments passed to ${fnName}: ${other}"))
        yield s.copy(memSpace = ms, retValue = retVal)

      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""val unitDays: String    = "${unitDays}"
                            |
                            |${argUnit}.trim.toLowerCase match {
                            |  case `unitDays` =>
                            |    ${argValue}.plusDays(${argOffset}.toLong)
                            |  case _ =>
                            |    throw new RuntimeException(s"Unexpected unit of time was passed to ${fnName}: '$${${argUnit}}'")
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines, imports = s.imports + "java.time.LocalDate")

      case s: Scala3JState =>
        for lines <- Right(
                       split(
                         s"""val unitDays: JString    = "${unitDays}"
                            |
                            |def toJLong(x: JInteger): JLong =
                            |  if (x == null) then null else x.longValue()
                            |
                            |val longOffset = toJLong(${argOffset})
                            |
                            |${argUnit}.trim.toLowerCase match {
                            |  case `unitDays` =>
                            |    ${argValue}.plusDays(longOffset)
                            |  case _ =>
                            |    throw new RuntimeException(s"Unexpected unit of time was passed to ${fnName}: '$${${argUnit}}'")
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines, imports = s.imports ++ Set("java.time.LocalDate", "java.lang.String as JString", "java.lang.Integer as JInteger", "java.lang.Long as JLong"))

      case other =>
        Left(new B1Exception(s"Unexpected state passed to ${fnName}: ${other}"))
