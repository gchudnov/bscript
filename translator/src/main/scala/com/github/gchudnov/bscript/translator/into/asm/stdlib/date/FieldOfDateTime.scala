package com.github.gchudnov.bscript.translator.into.asm.stdlib.date

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

import scala.util.control.Exception.allCatch

private[into] object FieldOfDateTime:
  import DateTime.*

  private val fnName = "fieldOfDateTime"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.i32Type),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.datetimeType), "value"),
        ArgDecl(TypeRef(typeNames.strType), "unit")
      ),
      Block(
        CompiledExpr(callback = FieldOfDateTime.fieldOfDateTime, retType = TypeRef(typeNames.i32Type))
      ),
      Seq(ComAnn("Returns the specified field of date-time as an integer value"), StdAnn())
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
      case s: AsmState =>
        for lines <- Right(
                       split(
                         s"""if (${argUnit} === "${unitDays}") {
                            |  return ${argValue}.getUTCDate();
                            |} else if (${argUnit} === "${unitHours}") {
                            |  return ${argValue}.getUTCHours();
                            |} else if (${argUnit} === "${unitMinutes}") {
                            |  return ${argValue}.getUTCMinutes();
                            |} else if (${argUnit} === "${unitSeconds}") {
                            |  return ${argValue}.getUTCSeconds();
                            |}
                            |return -1;
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines, imports = s.imports)

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
