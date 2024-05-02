package com.github.gchudnov.bscript.translator.into.asm.stdlib

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

private[asm] final class CoalesceStr(typeNames: TypeNames, typeName: String) extends Coalesce(typeNames, typeName, s"x !== \"!#\"")

private[asm] final class CoalesceI32(typeNames: TypeNames, typeName: String) extends Coalesce(typeNames, typeName, s"x !== I32.MIN_VALUE")

private[asm] final class CoalesceI64(typeNames: TypeNames, typeName: String) extends Coalesce(typeNames, typeName, s"x !== I64.MIN_VALUE")

private[asm] final class CoalesceF32(typeNames: TypeNames, typeName: String) extends Coalesce(typeNames, typeName, "!F32.isNaN(x)")

private[asm] final class CoalesceF64(typeNames: TypeNames, typeName: String) extends Coalesce(typeNames, typeName, "!F64.isNaN(x)")

private[asm] final class CoalesceDat(typeNames: TypeNames, typeName: String) extends Coalesce(typeNames, typeName, "x.getTime() !== NAdate.getTime()")

private[asm] final class CoalesceDtm(typeNames: TypeNames, typeName: String) extends Coalesce(typeNames, typeName, "x.getTime() !== NAdate.getTime()")


private abstract class Coalesce(typeNames: TypeNames, typeName: String, check: String):

  private def fnName: String =
    s"coalesce_${typeName}"

  def decl: MethodDecl =
    MethodDecl(
      DeclType(Var(SymbolRef("x"))),
      fnName,
      List(
        ArgDecl(TypeRef(typeName), "x"),
        ArgDecl(TypeRef(typeName), "y")
      ),
      Block(
        CompiledExpr(callback = this.coalesce, retType = DeclType(Var(SymbolRef("x"))))
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
            s"""if (${check}) {
               |  return ${argX};
               |}
               |return ${argY};
               |""".stripMargin
          )
        )
        yield s.copy(lines = lines)

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
