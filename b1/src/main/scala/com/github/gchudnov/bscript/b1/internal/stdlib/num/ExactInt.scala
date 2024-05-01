package com.github.gchudnov.bscript.b1.internal.stdlib.num

import com.github.gchudnov.bscript.b1.B1Exception
import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

import java.lang.Math
import scala.util.control.Exception.allCatch

private[internal] object ExactInt:

  private val fnName = "exactInt"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.i32Type),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.autoType), "value")
      ),
      Block(
        CompiledExpr(callback = ExactInt.exactToInt, retType = TypeRef(typeNames.i32Type))
      ),
      Seq(ComAnn(s"Safely casts a number to ${typeNames.i32Type} or returns an error on runtime"), StdAnn())
    )

  /**
   * Safely casts a number to integer, or returns an error on runtime
   */
  private def exactToInt(s: Any): Either[Throwable, Any] =
    val argValue = "value" // auto: f32, f64, dec

    def exactCastI32(n: Int): Either[Throwable, Int] =
      Right(n)

    def exactCastI64(n: Long): Either[Throwable, Int] =
      allCatch.either(Math.toIntExact(n)).left.map(t => new B1Exception(s"Cannot convert long ${n} to the exact int", t))

    def exactCastF32(n: Float): Either[Throwable, Int] =
      allCatch.either(BigDecimal.valueOf(n).toIntExact).left.map(t => new B1Exception(s"Cannot convert float ${n} to the exact int", t))

    def exactCastF64(n: Double): Either[Throwable, Int] =
      allCatch.either(BigDecimal.valueOf(n).toIntExact).left.map(t => new B1Exception(s"Cannot convert double ${n} to the exact int", t))

    def exactCastDec(n: BigDecimal): Either[Throwable, Int] =
      allCatch.either(n.toIntExact).left.map(t => new B1Exception(s"Cannot convert bigDecimal ${n} to the exact int", t))

    s match
      case s @ InterpretState(_, _, ms, _) =>
        for
          valueCell <- ms.tryFetch(CellPath(argValue))
          retVal <- (valueCell) match
                      case (IntCell(x)) =>
                        exactCastI32(x).map(IntCell.apply)
                      case (LongCell(x)) =>
                        exactCastI64(x).map(IntCell.apply)
                      case (FloatCell(x)) =>
                        exactCastF32(x).map(IntCell.apply)
                      case (DoubleCell(x)) =>
                        exactCastF64(x).map(IntCell.apply)
                      case (DecimalCell(x)) =>
                        exactCastDec(x).map(IntCell.apply)
                      case other =>
                        Left(new B1Exception(s"Unexpected type of arguments passed to ${fnName}: ${other}"))
        yield s.copy(memSpace = ms, retValue = retVal)

      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""// NOTE: Add [T: Numeric] to the method
                            |
                            |${argValue} match {
                            |  case x: Int =>
                            |    x
                            |  case x: Long =>
                            |    Math.toIntExact(x)
                            |  case x: Float =>
                            |    BigDecimal.valueOf(x).toIntExact
                            |  case x: Double =>
                            |    BigDecimal.valueOf(x).toIntExact
                            |  case x: BigDecimal =>
                            |    x.toIntExact
                            |  case other =>
                            |    throw new RuntimeException(s"Cannot safely cast the provided value: $${other}, the type is not supported")
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines, imports = s.imports + "java.lang.Math")

      case s: Scala3JState =>
        for lines <- Right(
                       split(
                         s"""// NOTE: Add [T <: Number] to the method
                            |
                            |${argValue} match {
                            |  case x: JInteger =>
                            |    x
                            |  case x: JLong =>
                            |    Math.toIntExact(x)
                            |  case x: JFloat =>
                            |    JBigDecimal.valueOf(x.doubleValue()).intValueExact()
                            |  case x: JDouble =>
                            |    JBigDecimal.valueOf(x).intValueExact()
                            |  case x: JBigDecimal =>
                            |    x.intValueExact()
                            |  case other =>
                            |    throw new RuntimeException(s"Cannot safely cast the provided value: $${other}, the type is not supported")
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(
          lines = lines,
          imports = s.imports ++ Set(
            "java.lang.Math",
            "java.lang.Integer as JInteger",
            "java.lang.Long as JLong",
            "java.math.BigDecimal as JBigDecimal",
            "java.lang.Float as JFloat",
            "java.lang.Double as JDouble"
          )
        )

      case other =>
        Left(new B1Exception(s"Unexpected state passed to ${fnName}: ${other}"))
