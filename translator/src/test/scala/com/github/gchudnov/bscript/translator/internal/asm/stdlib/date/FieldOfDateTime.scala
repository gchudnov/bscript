package com.github.gchudnov.bscript.translator.internal.asm.stdlib.date

import com.github.gchudnov.bscript.translator.internal.asm.AsmException
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State
import com.github.gchudnov.bscript.translator.internal.scalax.scala3j.Scala3JState

import scala.util.control.Exception.allCatch

private[internal] object FieldOfDateTime:
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
                            |    ${argValue}.getDayOfMonth
                            |  case `unitHours` =>
                            |    ${argValue}.getHour
                            |  case `unitMinutes` =>
                            |    ${argValue}.getMinute
                            |  case `unitSeconds` =>
                            |    ${argValue}.getSecond
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
                            |    ${argValue}.getDayOfMonth
                            |  case `unitHours` =>
                            |    ${argValue}.getHour
                            |  case `unitMinutes` =>
                            |    ${argValue}.getMinute
                            |  case `unitSeconds` =>
                            |    ${argValue}.getSecond
                            |  case _ =>
                            |    throw new RuntimeException(s"Unexpected date-time unit passed to ${fnName}: '$${${argUnit}}'")
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines, imports = s.imports ++ Set("java.time.OffsetDateTime", "java.lang.String as JString"))

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
