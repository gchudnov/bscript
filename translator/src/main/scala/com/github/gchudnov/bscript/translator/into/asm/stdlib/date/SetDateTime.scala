package com.github.gchudnov.bscript.translator.into.asm.stdlib.date

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

import scala.util.control.Exception.allCatch

private[into] object SetDateTime:
  import DateTime.*

  private val fnName = "setDateTime_datetime"

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
      case s: AsmState =>
        for lines <- Right(
                       split(
                         s"""if (${argUnit} === "${unitDays}") {
                            |  ${argValue}.setUTCDate(${argOffset});
                            |  return ${argValue};
                            |} else if (${argUnit} === "${unitHours}") {
                            |  ${argValue}.setUTCHours(${argOffset});
                            |  return ${argValue};
                            |} else if (${argUnit} === "${unitMinutes}") {
                            |  ${argValue}.setUTCMinutes(${argOffset});
                            |  return ${argValue};
                            |} else if (${argUnit} === "${unitSeconds}") {
                            |  ${argValue}.setUTCSeconds(${argOffset});
                            |  return ${argValue};
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines, imports = s.imports)

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
