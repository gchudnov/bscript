package com.github.gchudnov.bscript.translator.into.asm.stdlib

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split

private[asm] final class IsDefinedT(typeNames: TypeNames, typeName: String):

  private val fnName: String =
    s"isDefined_${typeName}"

  private val magic: String =
    Undefined.magic(typeNames)(typeName)

  def decl: MethodDecl =
    MethodDecl(
      TypeRef(typeNames.boolType),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.strType), "x")
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
            s"""return x !== "${magic}";
               |""".stripMargin
          )
        )
        yield s.copy(lines = lines)

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
