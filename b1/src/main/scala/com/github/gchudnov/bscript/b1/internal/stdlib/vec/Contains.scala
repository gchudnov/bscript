package com.github.gchudnov.bscript.b1.internal.stdlib.vec

import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.translator.internal.scala2.Scala2State
import com.github.gchudnov.bscript.b1.B1Exception
import com.github.gchudnov.bscript.lang.util.ShowOps.split
import com.github.gchudnov.bscript.interpreter.memory.*
import scala.util.control.Exception.allCatch
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames

private[internal] object Contains:

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.boolType),
      "contains",
      List(
        ArgDecl(TypeRef(typeNames.autoType), "x"),
        ArgDecl(VectorType(DeclType(Var(SymbolRef("x")))), "xs")
      ),
      Block(
        CompiledExpr(callback = Contains.contains, retType = TypeRef(typeNames.boolType))
      ),
      Seq(ComAnn("Tests whether the list contains the given element."), StdAnn())
    )

  private def contains(s: Any): Either[Throwable, Any] =
    val argX  = "x"  // auto
    val argXS = "xs" // vec[decltype(x)]

    s match
      case s @ InterpretState(_, ms, c) =>
        for
          xCell  <- ms.fetch(CellPath(argX))
          xsCell <- ms.fetch(CellPath(argXS))
          retVal <- (xCell, xsCell) match
                      case (x: Cell, xs: VecCell) =>
                        Right(BoolCell(xs.value.contains(x)))
                      case (x, xs) =>
                        Left(new B1Exception(s"Unexpected parameter types passed to contains: (${x}, ${xs})"))
        yield s.copy(memSpace = ms, retValue = retVal)

      case s: Scala2State =>
        for lines <- Right(
                       split(
                         s"""// NOTE: Add [T] to the method
                            |???
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case other =>
        Left(new B1Exception(s"Unexpected state passed to contains: ${other}"))
