package com.github.gchudnov.bscript.translator.into.asm.stdlib.num

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.translator.into.asm.stdlib.Inits
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

final class RoundT(typeNames: TypeNames, typeName: String):

  private val fnName = s"round_${typeName}"

  def decl: MethodDecl =
    MethodDecl(
      DeclType(Var(SymbolRef("value"))),
      fnName,
      List(
        ArgDecl(TypeRef(typeName), "value"), // f32, f64, dec
        ArgDecl(TypeRef(typeNames.i32Type), "precision")
      ),
      Block(
        CompiledExpr(callback = this.round, retType = DeclType(Var(SymbolRef("value"))))
      ),
      Seq(ComAnn("Rounds the provided value with the given precision"), StdAnn())
    )

  /**
   * Returns the rounded numerical value (3.1234, 2) -> 3.12 ; (3.1264, 2) -> 3.13
   */
  private def round(s: Any): Either[Throwable, Any] =
    val argValue     = "value"     // auto: f32, f64, dec
    val argPrecision = "precision" // i32

    s match
      case s: AsmState =>
        for lines <- Right(
                       split(
                         s"""let s: f64 = Math.pow(<f64>10, <f64>${argPrecision})
                            |return Math.round(${argValue} * s) / s
                            |""".stripMargin
                       )
                     )
        yield s.copy(
          lines = lines,
          imports = s.imports,
          inits = s.inits
        )

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
