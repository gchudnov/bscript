package com.github.gchudnov.bscript.translator.into.asm.stdlib.num

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.translator.into.asm.stdlib.Inits
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

private[into] object TruncateF32:

  private val fnName = "truncate_float"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      DeclType(Var(SymbolRef("value"))),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.f32Type), "value"), // f32, f64, dec
        ArgDecl(TypeRef(typeNames.i32Type), "precision")
      ),
      Block(
        CompiledExpr(callback = TruncateF32.truncate, retType = DeclType(Var(SymbolRef("value"))))
      ),
      Seq(ComAnn("Truncates the provided value with the given precision"), StdAnn())
    )

  /**
   * Returns the truncated numerical value (3.1234, 2) -> 3.12 (3.1264, 2) -> 3.12
   */
  private def truncate(s: Any): Either[Throwable, Any] =
    val argValue = "value" // auto: f32, f64, dec
    val argPrecision = "precision" // i32

    s match
      case s: AsmState =>
        for lines <- Right(
          split(
            s"""let s: f32 = Mathf.pow(<f32>10, <f32>${argPrecision})
               |if (${argValue} < 0.0) {
               |  return Mathf.ceil(${argValue} * s) / s;
               |} else {
               |  return Mathf.floor(${argValue} * s) / s;
               |}
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
