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

private[internal] object Now:
  import DateTime.*

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.datetimeType),
      "now",
      List.empty[ArgDecl],
      Block(
        CompiledExpr(callback = Now.now, retType = TypeRef(typeNames.datetimeType))
      ),
      Seq(ComAnn("Returns the current date and time"), StdAnn())
    )

  /**
   * Returns current datetime in UTC format: 2021-12-11T13:20:13.521645485Z
   */
  private def now(s: Any): Either[Throwable, Any] =
    s match
      case s @ InterpretState(_, ms, c) =>
        for retVal <- Right(DateTimeCell(OffsetDateTime.now(utcZone)))
        yield s.copy(memSpace = ms, retValue = retVal)

      case s: Scala2State =>
        for lines <- Right(
                       split(
                         s"""OffsetDateTime.now(ZoneId.of("Z"))
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines, imports = s.imports ++ Seq("java.time.OffsetDateTime", "java.time.ZoneId"))

      case other =>
        Left(new B1Exception(s"Unexpected state passed to now: ${other}"))
