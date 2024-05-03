package com.github.gchudnov.bscript.translator.into.asm.stdlib.num

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

import java.lang.Math
import scala.util.control.Exception.allCatch

private[into] class ExactInt(typeNames: TypeNames, typeName: String):

  private val fnName = s"exactInt_${typeName}"

  def decl: MethodDecl =
    MethodDecl(
      TypeRef(typeNames.i32Type),
      fnName,
      List(
        ArgDecl(TypeRef(typeName), "value")
      ),
      Block(
        CompiledExpr(callback = this.exactToInt, retType = TypeRef(typeNames.i32Type))
      ),
      Seq(ComAnn(s"Safely casts a number to ${typeNames.i32Type} or returns an error on runtime"), StdAnn())
    )

  /**
   * Safely casts a number to integer, or returns an error on runtime
   */
  private def exactToInt(s: Any): Either[Throwable, Any] =
    val argValue = "value" // auto: f32, f64, dec

    s match
      case s: AsmState =>
        for lines <- Right(
                       split(
                         s"""return <i32>value;
                            |""".stripMargin
                       )
                     )
        yield s.copy(
          lines = lines,
          imports = s.imports
        )

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
