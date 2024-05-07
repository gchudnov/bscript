package com.github.gchudnov.bscript.translator.into.asm.stdlib.date

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

import java.time.OffsetDateTime
import scala.util.control.Exception.allCatch

private[into] object AdjustDateTime:
  import DateTime.*

  private val fnName = "offsetDateTime_datetime"

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

    s match
      case s: AsmState =>
        for lines <- Right(
                       split(
                         s"""if (${argUnit} === "${unitDays}") {
                            |  ${argValue}.setUTCDate(${argValue}.getUTCDate() + ${argOffset});
                            |} else if (${argUnit} === "${unitHours}") {
                            |  ${argValue}.setUTCHours(${argValue}.getUTCHours() + ${argOffset});
                            |} else if (${argUnit} === "${unitMinutes}") {
                            |  ${argValue}.setUTCMinutes(${argValue}.getUTCMinutes() + ${argOffset});
                            |} else if (${argUnit} === "${unitSeconds}") {
                            |  ${argValue}.setUTCSeconds(${argValue}.getUTCSeconds() + ${argOffset});
                            |}
                            |return ${argValue};
                            |""".stripMargin
                       )
                     )
        yield s.copy(
          lines = lines,
          imports = s.imports
        )

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
