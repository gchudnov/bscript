package com.github.gchudnov.bscript.translator.into.asm.stdlib

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}

private[asm] final class IsDefinedStr(typeNames: TypeNames, typeName: String) extends IsDefined(typeNames, typeName, s"x !== \"!#\"")

private[asm] final class IsDefinedI32(typeNames: TypeNames, typeName: String) extends IsDefined(typeNames, typeName, s"x !== I32.MIN_VALUE")
private[asm] final class IsDefinedI64(typeNames: TypeNames, typeName: String) extends IsDefined(typeNames, typeName, s"x !== I64.MIN_VALUE")

private[asm] final class IsDefinedF32(typeNames: TypeNames, typeName: String) extends IsDefined(typeNames, typeName, "F32.isNaN(x)")
private[asm] final class IsDefinedF64(typeNames: TypeNames, typeName: String) extends IsDefined(typeNames, typeName, "F64.isNaN(x)")

private[asm] final class IsDefinedDat(typeNames: TypeNames, typeName: String) extends IsDefined(typeNames, typeName, "x.getTime() !== Date.parse(\"1900-01-01\").getTime()")
private[asm] final class IsDefinedDtm(typeNames: TypeNames, typeName: String) extends IsDefined(typeNames, typeName, "x.getTime() !== Date.parse(\"1900-01-01\").getTime()")


private abstract class IsDefined(typeNames: TypeNames, typeName: String, check: String):

  private val fnName: String =
    s"isDefined_${typeName}"

  def decl: MethodDecl =
    MethodDecl(
      TypeRef(typeNames.boolType),
      fnName,
      List(
        ArgDecl(TypeRef(typeName), "x")
      ),
      Block(
        CompiledExpr(callback = this.isDefined, retType = TypeRef(typeNames.boolType))
      ),
      Seq(ComAnn("returns true of the provided variable is defined, otherwise false"), StdAnn())
    )

  /**
   * checks that the provided parameter is not null
   *
   * {{{
   *   int x = "str";
   *   bool xr = isDefined_string(x); // true
   * }}}
   */
  private def isDefined(s: Any): Either[Throwable, Any] =
    val argX = "x" // auto

    s match
      case s: AsmState =>
        for lines <- Right(
          split(
            s"""return ${check};
               |""".stripMargin
          )
        )
        yield s.copy(lines = lines)

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
