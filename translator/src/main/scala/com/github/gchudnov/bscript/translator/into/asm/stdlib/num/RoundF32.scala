package com.github.gchudnov.bscript.translator.into.asm.stdlib.num

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.translator.into.asm.stdlib.Inits
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

private[into] object RoundF32:

  private val fnName = "round_float"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      DeclType(Var(SymbolRef("value"))),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.f32Type), "value"), // f32, f64, dec
        ArgDecl(TypeRef(typeNames.i32Type), "precision")
      ),
      Block(
        CompiledExpr(callback = RoundF32.round, retType = DeclType(Var(SymbolRef("value"))))
      ),
      Seq(ComAnn("Rounds the provided value with the given precision"), StdAnn())
    )

  /**
   * Returns the rounded numerical value (3.1234, 2) -> 3.12 ; (3.1264, 2) -> 3.13
   */
  private def round(s: Any): Either[Throwable, Any] =
    val argValue = "value" // auto: f32, f64, dec
    val argPrecision = "precision" // i32

    s match
      case s: AsmState =>
        for lines <- Right(
          split(
            s"""let s: f32 = Mathf.pow(<f32>10, <f32>${argPrecision})
               |return Mathf.round(${argValue} * s) / s
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
