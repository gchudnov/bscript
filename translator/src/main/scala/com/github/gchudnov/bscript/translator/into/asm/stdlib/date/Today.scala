package com.github.gchudnov.bscript.translator.into.asm.stdlib.date

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.translator.into.asm.stdlib.Inits
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split


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
        for lines <- Right(
                       split(
                         s"""let dt = new Date(wasi_Date.now())
                            |return Date.parse(dt.toISOString().substring(0, 10))
                            |""".stripMargin
                       )
                     )
        yield s.copy(
          lines = lines,
          imports = s.imports ++ Seq("{ Date} from \"date\";"),
          inits = s.inits //++ Inits.codeBlocks(Seq(Inits.Keys.ToOrderedLocalDate, Inits.Keys.ToOrderedOffsetDateTime))
        )

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
