package com.github.gchudnov.bscript.b1.internal.stdlib.num

import com.github.gchudnov.bscript.b1.B1Exception
import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State

import java.lang.Math
import scala.util.control.Exception.allCatch

private[internal] object CastLong:

  private val fnName = "exactLong"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.i64Type),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.autoType), "value")
      ),
      Block(
        CompiledExpr(callback = CastLong.exactToLong, retType = TypeRef(typeNames.i64Type))
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
      allCatch.either(BigDecimal.valueOf(n).toLongExact).left.map(t => new B1Exception(s"Cannot convert float ${n} to the exact long", t))

    def exactCastF64(n: Double): Either[Throwable, Long] =
      allCatch.either(BigDecimal.valueOf(n).toLongExact).left.map(t => new B1Exception(s"Cannot convert double ${n} to the exact long", t))

    def exactCastDec(n: BigDecimal): Either[Throwable, Long] =
      allCatch.either(n.toLongExact).left.map(t => new B1Exception(s"Cannot convert bigDecimal ${n} to the exact long", t))

    s match
      case s @ InterpretState(_, _, ms, _) =>
        for
          valueCell <- ms.tryFetch(CellPath(argValue))
          retVal <- (valueCell) match
                      case (IntCell(x)) =>
                        exactCastI32(x).map(LongCell.apply)
                      case (LongCell(x)) =>
                        exactCastI64(x).map(LongCell.apply)
                      case (FloatCell(x)) =>
                        exactCastF32(x).map(LongCell.apply)
                      case (DoubleCell(x)) =>
                        exactCastF64(x).map(LongCell.apply)
                      case (DecimalCell(x)) =>
                        exactCastDec(x).map(LongCell.apply)
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

      case other =>
        Left(new B1Exception(s"Unexpected state passed to ${fnName}: ${other}"))
