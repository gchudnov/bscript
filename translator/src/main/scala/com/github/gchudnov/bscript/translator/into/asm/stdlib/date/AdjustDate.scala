package com.github.gchudnov.bscript.translator.into.asm.stdlib.date

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

import java.time.LocalDate
import scala.util.control.Exception.allCatch

private[into] object AdjustDate:
  import DateTime.*

  private val fnName = "offsetDate_date"

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
      case s: AsmState =>
        for lines <- Right(
                       split(
                         s"""if (${argUnit} === "${unitDays}") {
                            |  ${argValue}.setUTCDate(${argValue}.getUTCDate() + ${argOffset});
                            |  return ${argValue};
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines, imports = s.imports)

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
