package com.github.gchudnov.bscript.b1.internal.stdlib.date

import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.translator.internal.scala2.Scala2State
import com.github.gchudnov.bscript.b1.B1Exception
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.interpreter.memory.*
import java.time.{ LocalDate, OffsetDateTime, ZoneId }
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames

private[internal] object Today:
  import DateTime.*

  private val fnName = "today"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.dateType),
      fnName,
      List.empty[ArgDecl],
      Block(
        CompiledExpr(callback = Today.today, retType = TypeRef(typeNames.dateType))
      ),
      Seq(ComAnn("Returns today as date"), StdAnn())
    )

  /**
   * Returns current date in UTC format: YYYY-MM-DD
   */
  private def today(s: Any): Either[Throwable, Any] =
    s match
      case s @ InterpretState(_, _, ms, c) =>
        for retVal <- Right(DateCell(LocalDate.now(utcZone)))
        yield s.copy(memSpace = ms, retValue = retVal)

      case s: Scala2State =>
        for lines <- Right(
                       split(
                         s"""LocalDate.now(ZoneId.of("Z"))
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines, imports = s.imports ++ Seq("java.time.LocalDate", "java.time.ZoneId"))

      case other =>
        Left(new B1Exception(s"Unexpected state passed to ${fnName}: ${other}"))
