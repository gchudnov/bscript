package com.github.gchudnov.bscript.translator.internal.asm.stdlib.vec

import com.github.gchudnov.bscript.translator.internal.asm.AsmException
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State
import com.github.gchudnov.bscript.translator.internal.scalax.scala3j.Scala3JState

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
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
