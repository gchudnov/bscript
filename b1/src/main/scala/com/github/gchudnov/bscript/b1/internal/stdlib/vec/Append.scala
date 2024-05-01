package com.github.gchudnov.bscript.b1.internal.stdlib.vec

import com.github.gchudnov.bscript.b1.B1Exception
import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

import scala.util.control.Exception.allCatch

private[internal] object Append:

  private val fnName = "append"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      VectorType(DeclType(Var(SymbolRef("x")))),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.autoType), "x"),
        ArgDecl(VectorType(DeclType(Var(SymbolRef("x")))), "xs")
      ),
      Block(
        CompiledExpr(callback = Append.append, retType = VectorType(DeclType(Var(SymbolRef("x")))))
      ),
      Seq(ComAnn("Append an element to the collection."), StdAnn())
    )

  private def append(s: Any): Either[Throwable, Any] =
    val argX  = "x"  // auto
    val argXS = "xs" // vec[decltype(x)]

    s match
      case s @ InterpretState(_, _, ms, c) =>
        for
          xCell  <- ms.tryFetch(CellPath(argX))
          xsCell <- ms.tryFetch(CellPath(argXS))
          retVal <- (xCell, xsCell) match
                      case (x: Cell, xs: VecCell) =>
                        Right(VecCell(xs.value :+ x))
                      case (x, xs) =>
                        Left(new B1Exception(s"Unexpected parameter types are passed to ${fnName}: (${x}, ${xs})"))
        yield s.copy(memSpace = ms, retValue = retVal)

      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""// NOTE: Add [T] to the method
                            |xs :+ x
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case s: Scala3JState =>
        for lines <- Right(
                       split(
                         s"""// NOTE: Add [T] to the method
                            |xs :+ x
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case other =>
        Left(new B1Exception(s"Unexpected state passed to ${fnName}: ${other}"))
