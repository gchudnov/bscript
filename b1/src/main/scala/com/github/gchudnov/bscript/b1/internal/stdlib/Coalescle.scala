package com.github.gchudnov.bscript.b1.internal.stdlib

import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.translator.internal.scala2.Scala2State
import com.github.gchudnov.bscript.b1.B1Exception
import com.github.gchudnov.bscript.lang.util.ShowOps.split
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames

private[internal] object Coalesce:

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      DeclType(Var(SymbolRef("x"))),
      "coalesce",
      List(
        ArgDecl(TypeRef(typeNames.autoType), "x"),
        ArgDecl(TypeRef(typeNames.autoType), "y")
      ),
      Block(
        CompiledExpr(callback = Coalesce.coalesce, retType = DeclType(Var(SymbolRef("x"))))
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
      case s @ InterpretState(_, ms, c) =>
        for
          xCell <- ms.fetch(CellPath(argX))
          yCell <- ms.fetch(CellPath(argY))
          retVal = (xCell, yCell) match
                     case (NothingCell, y) =>
                       y
                     case (x, _) =>
                       x
        yield s.copy(memSpace = ms, retValue = retVal)

      case _: Scala2State =>
        for lines <- Right(
                       split(
                         s"""(${argX}, ${argY}) match {
                            |  case (null, _) => ${argY}
                            |  case (None, _) => ${argY}
                            |  case _ => ${argX}
                            |}
                            |""".stripMargin
                       )
                     )
        yield Scala2State(lines = lines)

      case other =>
        Left(new B1Exception(s"Unexpected state passed to coalesce: ${other}"))
