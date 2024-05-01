package com.github.gchudnov.bscript.translator.internal.asm.stdlib.num

import com.github.gchudnov.bscript.translator.internal.asm.AsmException
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State
import com.github.gchudnov.bscript.translator.internal.scalax.scala3j.Scala3JState

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
      allCatch.either(Math.toIntExact(n)).left.map(t => new AsmException(s"Cannot convert long ${n} to the exact int", t))

    def exactCastF32(n: Float): Either[Throwable, Int] =
      allCatch.either(BigDecimal.valueOf(n).toIntExact).left.map(t => new AsmException(s"Cannot convert float ${n} to the exact int", t))

    def exactCastF64(n: Double): Either[Throwable, Int] =
      allCatch.either(BigDecimal.valueOf(n).toIntExact).left.map(t => new AsmException(s"Cannot convert double ${n} to the exact int", t))

    def exactCastDec(n: BigDecimal): Either[Throwable, Int] =
      allCatch.either(n.toIntExact).left.map(t => new AsmException(s"Cannot convert bigDecimal ${n} to the exact int", t))

    s match
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
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
