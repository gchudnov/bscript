package com.github.gchudnov.bscript.translator.internal.asm.stdlib.num

import com.github.gchudnov.bscript.translator.internal.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State
import com.github.gchudnov.bscript.translator.internal.scalax.scala3j.Scala3JState

import java.lang.Math
import scala.util.control.Exception.allCatch

private[internal] object ExactLong:

  private val fnName = "exactLong"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.i64Type),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.autoType), "value")
      ),
      Block(
        CompiledExpr(callback = ExactLong.exactToLong, retType = TypeRef(typeNames.i64Type))
      ),
      Seq(ComAnn(s"Safely casts a number to ${typeNames.i64Type} or returns an error on runtime"), StdAnn())
    )

  /**
   * Safely casts a number to long, or returns an error on runtime
   */
  private def exactToLong(s: Any): Either[Throwable, Any] =
    val argValue = "value" // auto: f32, f64, dec

    def exactCastI32(n: Int): Either[Throwable, Long] =
      Right(n.toLong)

    def exactCastI64(n: Long): Either[Throwable, Long] =
      Right(n)

    def exactCastF32(n: Float): Either[Throwable, Long] =
      allCatch.either(BigDecimal.valueOf(n).toLongExact).left.map(t => new AsmException(s"Cannot convert float ${n} to the exact long", t))

    def exactCastF64(n: Double): Either[Throwable, Long] =
      allCatch.either(BigDecimal.valueOf(n).toLongExact).left.map(t => new AsmException(s"Cannot convert double ${n} to the exact long", t))

    def exactCastDec(n: BigDecimal): Either[Throwable, Long] =
      allCatch.either(n.toLongExact).left.map(t => new AsmException(s"Cannot convert bigDecimal ${n} to the exact long", t))

    s match
      case s: AsmState =>
        Right(s) // TODO: change later

      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""// NOTE: Add [T: Numeric] to the method
                            |
                            |${argValue} match {
                            |  case x: Int =>
                            |    x.toLong
                            |  case x: Long =>
                            |    x
                            |  case x: Float =>
                            |    BigDecimal.valueOf(x).toLongExact
                            |  case x: Double =>
                            |    BigDecimal.valueOf(x).toLongExact
                            |  case x: BigDecimal =>
                            |    x.toLongExact
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
                            |    x.longValue()
                            |  case x: JLong =>
                            |    x
                            |  case x: JFloat =>
                            |    JBigDecimal.valueOf(x.doubleValue()).longValueExact()
                            |  case x: JDouble =>
                            |    JBigDecimal.valueOf(x).longValueExact()
                            |  case x: JBigDecimal =>
                            |    x.longValueExact()
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
