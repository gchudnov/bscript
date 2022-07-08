package com.github.gchudnov.bscript.b1.internal.stdlib.str

import com.github.gchudnov.bscript.b1.B1Exception
import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State
import com.github.gchudnov.bscript.translator.internal.scala3j.Scala3JState

private[internal] object StrLen:

  private val fnName = "strLen"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.i32Type),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.strType), "s")
      ),
      Block(
        CompiledExpr(callback = StrLen.strLen, retType = TypeRef(typeNames.i32Type))
      ),
      Seq(ComAnn("Returns the length of the provided string"), StdAnn())
    )

  /**
   * Gets the length of a provided string
   *
   * {{{
   *   string s = "123"
   *   int sz = strLen(s);
   * }}}
   */
  private def strLen(s: Any): Either[Throwable, Any] =
    val arg0 = "s"

    s match
      case s @ InterpretState(_, _, ms, c) =>
        for
          cell <- ms.tryFetch(CellPath(arg0))
          retVal <- cell match
                      case StrCell(value) =>
                        Right(IntCell(value.length))
                      case other =>
                        Left(new B1Exception(s"Unexpected type of arguments passed to ${fnName}: ${other}"))
        yield s.copy(memSpace = ms, retValue = retVal)

      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""${arg0}.length
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case s: Scala3JState =>
        for lines <- Right(
                       split(
                         s"""${arg0}.length
                            |""".stripMargin
                       )
                     )
        yield s.copy(
          lines = lines,
          imports = s.imports ++ Set(
            "java.lang.Integer as JInteger",
            "java.lang.String as JString"
          )
        )

      case other =>
        Left(new B1Exception(s"Unexpected state passed to ${fnName}: ${other}"))
