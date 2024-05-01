package com.github.gchudnov.bscript.translator.internal.asm.stdlib

import com.github.gchudnov.bscript.translator.internal.asm.AsmException
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State
import com.github.gchudnov.bscript.translator.internal.scalax.scala3j.Scala3JState

private[internal] object IsDefined:

  private val fnName = "isDefined"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.boolType),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.autoType), "x")
      ),
      Block(
        CompiledExpr(callback = IsDefined.isDefined, retType = TypeRef(typeNames.boolType))
      ),
      Seq(ComAnn("returns true of the provided variable is defined, otherwise false"), StdAnn())
    )

  /**
   * checks that the provided parameter is not null
   *
   * {{{
   *   int x = null;
   *   bool xr = isDefined(x); // false
   *
   *   float y = 12.34;
   *   bool yr = isDefined(y); // true
   * }}}
   */
  private def isDefined(s: Any): Either[Throwable, Any] =
    val argX = "x" // auto

    s match
      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""// NOTE: Add [T] to the method
                            |${argX} match {
                            |  case null => false
                            |  case None => false
                            |  case _ => true
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case s: Scala3JState =>
        for lines <- Right(
                       split(
                         s"""// NOTE: Add [T] to the method
                            |${argX} match {
                            |  case null => false
                            |  case None => false
                            |  case _ => true
                            |}
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
