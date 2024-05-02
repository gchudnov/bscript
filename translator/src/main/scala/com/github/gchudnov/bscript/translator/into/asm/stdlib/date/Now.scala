package com.github.gchudnov.bscript.translator.into.asm.stdlib.date

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

import java.time.LocalDate
import java.time.OffsetDateTime
import java.time.ZoneId

private[asm] object Now:
  import DateTime.*

  private val fnName = "now"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.datetimeType),
      fnName,
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
      case s: AsmState =>
        for lines <- Right(
                       split(
                         s"""return new Date(wasi_Date.now())
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines, imports = s.imports ++ Seq("{ Date} from \"date\";"))

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
