package com.github.gchudnov.bscript.translator.into.asm.stdlib.date

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.translator.into.asm.stdlib.Inits
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

import java.time.LocalDate
import java.time.OffsetDateTime
import java.time.ZoneId

private[into] object Today:
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
      case s: AsmState =>
        Right(s) // TODO: change later

      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""LocalDate.now(ZoneId.of("Z"))
                            |""".stripMargin
                       )
                     )
        yield s.copy(
          lines = lines,
          imports = s.imports ++ Seq("java.time.LocalDate", "java.time.ZoneId"),
          inits = s.inits ++ Inits.codeBlocks(Seq(Inits.Keys.ToOrderedLocalDate, Inits.Keys.ToOrderedOffsetDateTime))
        )

      case s: Scala3JState =>
        for lines <- Right(
                       split(
                         s"""LocalDate.now(ZoneId.of("Z"))
                            |""".stripMargin
                       )
                     )
        yield s.copy(
          lines = lines,
          imports = s.imports ++ Seq("java.time.LocalDate", "java.time.ZoneId"),
          inits = s.inits ++ Inits.codeBlocks(Seq(Inits.Keys.ToOrderedLocalDate, Inits.Keys.ToOrderedOffsetDateTime))
        )

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
