package com.github.gchudnov.bscript.translator.into.asm.stdlib

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

private[into] object CoalesceI32:

  private val fnName = "coalesce_int"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      DeclType(Var(SymbolRef("x"))),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.i32Type), "x"),
        ArgDecl(TypeRef(typeNames.i32Type), "y")
      ),
      Block(
        CompiledExpr(callback = CoalesceI32.coalesce, retType = DeclType(Var(SymbolRef("x"))))
      ),
      Seq(ComAnn("returns the first non-null value out of two values that were provided"), StdAnn())
    )

  /**
   * returns the first argument if not null, otherwise the second one
   *
   * {{{
   *   int x = null;
   *   int y = 17;
   *   int z = coalesce(x, y);
   *   z; // 17
   * }}}
   */
  private def coalesce(s: Any): Either[Throwable, Any] =
    val argX = "x" // auto
    val argY = "y" // auto

    s match
      case s: AsmState =>
        for lines <- Right(
                       split(
                         s"""if (${argX} !== I32.MIN_VALUE) {
                            |  return ${argX};
                            |}
                            |return ${argY};
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
