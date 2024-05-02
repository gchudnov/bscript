package com.github.gchudnov.bscript.translator.into.asm.stdlib.vec

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

import scala.util.control.Exception.allCatch

private[asm] final class ContainsStr(typeNames: TypeNames, typeName: String) extends Contains(typeNames, typeName, "return xs.includes(x);")

private[asm] final class ContainsI32(typeNames: TypeNames, typeName: String) extends Contains(typeNames, typeName, "return xs.includes(x);")

private[asm] final class ContainsI64(typeNames: TypeNames, typeName: String) extends Contains(typeNames, typeName, "return xs.includes(x);")

private[asm] final class ContainsF32(typeNames: TypeNames, typeName: String) extends Contains(typeNames, typeName, "return xs.includes(x);")

private[asm] final class ContainsF64(typeNames: TypeNames, typeName: String) extends Contains(typeNames, typeName, "return xs.includes(x);")

private[asm] final class ContainsDat(typeNames: TypeNames, typeName: String) extends Contains(typeNames, typeName,
  """for (let i = 0; i < xs.length; i++) {
    |  if (xs[i].getTime() == x.getTime()) {
    |    return true;
    |  }
    |}
    |return false;
    |""".stripMargin)

private[asm] final class ContainsDtm(typeNames: TypeNames, typeName: String) extends Contains(typeNames, typeName,
  """for (let i = 0; i < xs.length; i++) {
    |  if (xs[i].getTime() == x.getTime()) {
    |    return true;
    |  }
    |}
    |return false;
    |""".stripMargin)


private[asm] abstract class Contains(typeNames: TypeNames, typeName: String, check: String):

  private val fnName: String =
    s"contains_${typeName}"

  def decl: MethodDecl =
    MethodDecl(
      TypeRef(typeNames.boolType),
      fnName,
      List(
        ArgDecl(TypeRef(typeName), "x"),
        ArgDecl(VectorType(DeclType(Var(SymbolRef("x")))), "xs")
      ),
      Block(
        CompiledExpr(callback = this.contains, retType = TypeRef(typeNames.boolType))
      ),
      Seq(ComAnn("Tests whether the collection contains the given element."), StdAnn())
    )

  private def contains(s: Any): Either[Throwable, Any] =
    val argX  = "x"  // auto
    val argXS = "xs" // vec[decltype(x)]

    s match
      case s: AsmState =>
        for lines <- Right(
                       split(
                         s"""${check}
                            |""".stripMargin
                       )
                     )
        yield s.copy(
          lines = lines,
          imports = s.imports
        )

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
