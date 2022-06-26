package com.github.gchudnov.bscript.b1.internal.stdlib.date

import com.github.gchudnov.bscript.b1.B1Exception
import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.internal.scala2.Scala2State

import scala.util.control.Exception.allCatch

private[internal] object AdjustDate:
  import DateTime.*

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.dateType),
      "offsetDate",
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
                            allCatch.either(value.plusDays(offset.toLong)).map(DateCell.apply)
                          case other =>
                            Left(new B1Exception(s"Unexpected unit of date was passed to offsetDate: ${other}"))
                      case other =>
                        Left(new B1Exception(s"Unexpected type of arguments passed to offsetDate: ${other}"))
        yield s.copy(memSpace = ms, retValue = retVal)

      case s: Scala2State =>
        for lines <- Right(
                       split(
                         s"""val unitDays: String    = "${unitDays}"
                            |
                            |${argUnit}.trim.toLowerCase match {
                            |  case `unitDays` =>
                            |    ${argValue}.plusDays(${argOffset}.toLong)
                            |  case _ =>
                            |    throw new RuntimeException(s"Unexpected unit of time was passed to offsetDate: $${${argUnit}}")
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines, imports = s.imports + "java.time.LocalDate")

      case other =>
        Left(new B1Exception(s"Unexpected state passed to offsetDate: ${other}"))
