package com.github.gchudnov.bscript.b1.internal.stdlib

import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.translator.internal.scala2.Scala2State
import com.github.gchudnov.bscript.b1.B1Exception
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames

private[internal] object IsDefined:

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.boolType),
      "isDefined",
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
      case s @ InterpretState(_, _, ms, c) =>
        for
          xCell <- ms.fetch(CellPath(argX))
          flag = xCell match
                   case NothingCell =>
                     false
                   case _ =>
                     true
          retVal = BoolCell(flag)
        yield s.copy(memSpace = ms, retValue = retVal)

      case s: Scala2State =>
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

      case other =>
        Left(new B1Exception(s"Unexpected state passed to isDefined: ${other}"))
