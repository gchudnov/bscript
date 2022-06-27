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
import java.time.temporal.{ ChronoUnit, Temporal }

private[internal] object BetweenDates:
  import DateTime.*

  private val fnName = "betweenDates" // TODO: should we rename?

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.i32Type),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.dateType), "first"),
        ArgDecl(TypeRef(typeNames.dateType), "last"),
        ArgDecl(TypeRef(typeNames.strType), "unit")
      ),
      Block(
        CompiledExpr(callback = BetweenDates.betweenDates, retType = TypeRef(typeNames.i32Type))
      ),
      Seq(ComAnn("Calculates difference between two temporal points in time"), StdAnn())
    )

  private def betweenDates(s: Any): Either[Throwable, Any] =
    val argFirst = "first" // date
    val argLast  = "last"  // date
    val argUnit  = "unit"  // string unit of the offset (DAYS)

    def tempDiff(first: Temporal, last: Temporal, unit: ChronoUnit): Either[Throwable, Int] =
      allCatch.either(unit.between(last, first).toInt).left.map(t => new B1Exception(s"Cannot find ${unit.name()}-diff between '${first}' and '${last}' in ${fnName}", t))

    def tempDiffByUnit(first: Temporal, last: Temporal, unit: String): Either[Throwable, Int] =
      unit.trim.toLowerCase match
        case `unitDays` =>
          tempDiff(first, last, ChronoUnit.DAYS)
        case `unitHours` =>
          tempDiff(first, last, ChronoUnit.HOURS)
        case `unitMinutes` =>
          tempDiff(first, last, ChronoUnit.MINUTES)
        case `unitSeconds` =>
          tempDiff(first, last, ChronoUnit.SECONDS)
        case other =>
          Left(new B1Exception(s"Unexpected date-time unit passed to ${fnName}: '${other}'"))

    s match
      case s @ InterpretState(_, _, ms, c) =>
        for
          firstCell <- ms.tryFetch(CellPath(argFirst))
          lastCell  <- ms.tryFetch(CellPath(argLast))
          unitCell  <- ms.tryFetch(CellPath(argUnit))

          retVal <- (firstCell, lastCell, unitCell) match
                      case (DateCell(first), DateCell(last), StrCell(unit)) =>
                        tempDiffByUnit(first, last, unit).map(IntCell.apply)
                      case (DateCell(first), DateTimeCell(last), StrCell(unit)) =>
                        tempDiffByUnit(first, last, unit).map(IntCell.apply)
                      case (DateTimeCell(first), DateCell(last), StrCell(unit)) =>
                        tempDiffByUnit(first, last, unit).map(IntCell.apply)
                      case (DateTimeCell(first), DateTimeCell(last), StrCell(unit)) =>
                        tempDiffByUnit(first, last, unit).map(IntCell.apply)
                      case other =>
                        Left(new B1Exception(s"Unexpected type of arguments passed to ${fnName}: ${other}"))
        yield s.copy(memSpace = ms, retValue = retVal)

      case s: Scala2State =>
        for lines <- Right(
                       split(
                         s"""// NOTE: Add [T <: Temporal] to the method
                            |
                            |val unitDays: String    = "${unitDays}"
                            |val unitHours: String   = "${unitHours}"
                            |val unitMinutes: String = "${unitMinutes}"
                            |val unitSeconds: String = "${unitSeconds}"
                            |
                            |${argUnit}.trim.toLowerCase match {
                            |  case `unitDays` =>
                            |    ChronoUnit.DAYS.between(${argFirst}, ${argLast}).toInt
                            |  case `unitHours` =>
                            |    ChronoUnit.HOURS.between(${argFirst}, ${argLast}).toInt
                            |  case `unitMinutes` =>
                            |    ChronoUnit.MINUTES.between(${argFirst}, ${argLast}).toInt
                            |  case `unitSeconds` =>
                            |    ChronoUnit.SECONDS.between(${argFirst}, ${argLast}).toInt
                            |  case _ =>
                            |    throw new RuntimeException(s"Unexpected date-time unit passed to ${fnName}: '$${${argUnit}}'")
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines, imports = s.imports + "java.time.temporal.{ ChronoUnit, Temporal }")

      case other =>
        Left(new B1Exception(s"Unexpected state passed to ${fnName}: ${other}"))
