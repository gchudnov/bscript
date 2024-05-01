package com.github.gchudnov.bscript.translator.into.asm.stdlib.date

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

import java.time.temporal.ChronoUnit
import java.time.temporal.Temporal
import scala.util.control.Exception.allCatch

private[into] object BetweenTemp:
  import DateTime.*

  private val fnName = "betweenTemp"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.i32Type),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.autoType), "first"),
        ArgDecl(TypeRef(typeNames.autoType), "last"),
        ArgDecl(TypeRef(typeNames.strType), "unit")
      ),
      Block(
        CompiledExpr(callback = BetweenTemp.betweenDates, retType = TypeRef(typeNames.i32Type))
      ),
      Seq(ComAnn("Calculates difference between two temporal points in time"), StdAnn())
    )

  /**
   * Get the number of date-time units between two given dates
   *
   * {{{
   *   Given:
   *     result = betweenDates(a, b, "days")
   *
   *   Returns:
   *     result > 0, if a < b
   *     result < 0, if a > b
   * }}}
   */
  private def betweenDates(s: Any): Either[Throwable, Any] =
    val argFirst = "first" // date | dateTime
    val argLast  = "last"  // date | dateTime
    val argUnit  = "unit"  // string unit of time  (DAYS | HOURS | MINUTES | SECONDS)

    def tempDiff(first: Temporal, last: Temporal, unit: ChronoUnit): Either[Throwable, Int] =
      allCatch.either(unit.between(first, last).toInt).left.map(t => new AsmException(s"Cannot find ${unit.name()}-diff between '${first}' and '${last}' in ${fnName}", t))

    def tempDiffByUnit(first: Temporal, last: Temporal, unit: String): Either[Throwable, Int] =
      val chronoUnitOrErr = unit.trim.toLowerCase match
        case `unitDays` =>
          Right(ChronoUnit.DAYS)
        case `unitHours` =>
          Right(ChronoUnit.HOURS)
        case `unitMinutes` =>
          Right(ChronoUnit.MINUTES)
        case `unitSeconds` =>
          Right(ChronoUnit.SECONDS)
        case other =>
          Left(new AsmException(s"Unexpected date-time unit passed to ${fnName}: '${other}'"))

      chronoUnitOrErr.flatMap(chronoUnit => tempDiff(first, last, chronoUnit))

    s match
      case s: AsmState =>
        Right(s) // TODO: change later

      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""// NOTE: Add [T <: Temporal] to the method
                            |
                            |val unitDays: String    = "${unitDays}"
                            |val unitHours: String   = "${unitHours}"
                            |val unitMinutes: String = "${unitMinutes}"
                            |val unitSeconds: String = "${unitSeconds}"
                            |
                            |val chronoUnit = ${argUnit}.trim.toLowerCase match {
                            |  case `unitDays` =>
                            |    ChronoUnit.DAYS
                            |  case `unitHours` =>
                            |    ChronoUnit.HOURS
                            |  case `unitMinutes` =>
                            |    ChronoUnit.MINUTES
                            |  case `unitSeconds` =>
                            |    ChronoUnit.SECONDS
                            |  case _ =>
                            |    throw new RuntimeException(s"Unexpected date-time unit passed to ${fnName}: '$${${argUnit}}'")
                            |}
                            |
                            |chronoUnit.between(${argFirst}, ${argLast}).toInt
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines, imports = s.imports + "java.time.temporal.{ ChronoUnit, Temporal }")

      case s: Scala3JState =>
        for lines <- Right(
                       split(
                         s"""// NOTE: Add [T <: Temporal] to the method
                            |
                            |val unitDays: JString    = "${unitDays}"
                            |val unitHours: JString   = "${unitHours}"
                            |val unitMinutes: JString = "${unitMinutes}"
                            |val unitSeconds: JString = "${unitSeconds}"
                            |
                            |val chronoUnit = ${argUnit}.trim.toLowerCase match {
                            |  case `unitDays` =>
                            |    ChronoUnit.DAYS
                            |  case `unitHours` =>
                            |    ChronoUnit.HOURS
                            |  case `unitMinutes` =>
                            |    ChronoUnit.MINUTES
                            |  case `unitSeconds` =>
                            |    ChronoUnit.SECONDS
                            |  case _ =>
                            |    throw new RuntimeException(s"Unexpected date-time unit passed to ${fnName}: '$${${argUnit}}'")
                            |}
                            |
                            |chronoUnit.between(${argFirst}, ${argLast}).intValue()
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines, imports = s.imports ++ Set("java.time.temporal.{ ChronoUnit, Temporal }", "java.lang.String as JString"))

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
