package com.github.gchudnov.bscript.translator.internal.asm.stdlib.vec

import com.github.gchudnov.bscript.translator.internal.asm.AsmException
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State
import com.github.gchudnov.bscript.translator.internal.scalax.scala3j.Scala3JState

import scala.util.control.Exception.allCatch

private[internal] object Contains:

  private val fnName = "contains"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.boolType),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.autoType), "x"),
        ArgDecl(VectorType(DeclType(Var(SymbolRef("x")))), "xs")
      ),
      Block(
        CompiledExpr(callback = Contains.contains, retType = TypeRef(typeNames.boolType))
      ),
      Seq(ComAnn("Tests whether the collection contains the given element."), StdAnn())
    )

  private def contains(s: Any): Either[Throwable, Any] =
    val argX  = "x"  // auto
    val argXS = "xs" // vec[decltype(x)]

    s match
      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""// NOTE: Add [T] to the method
                            |xs.contains(x)
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case s: Scala3JState =>
        for lines <- Right(
                       split(
                         s"""// NOTE: Add [T] to the method
                            |xs.contains(x)
                            |""".stripMargin
                       )
                     )
        yield s.copy(
          lines = lines,
          imports = s.imports ++ Set(
            "java.lang.Boolean as JBoolean"
          )
        )

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
