package com.github.gchudnov.bscript.b1.internal.stdlib.num

import com.github.gchudnov.bscript.b1.B1Exception
import com.github.gchudnov.bscript.b1.internal.stdlib.Inits
import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State
import com.github.gchudnov.bscript.translator.internal.scala3j.Scala3JState

private[internal] object Round:

  private val fnName = "round"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      DeclType(Var(SymbolRef("value"))),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.autoType), "value"), // f32, f64, dec
        ArgDecl(TypeRef(typeNames.i32Type), "precision")
      ),
      Block(
        CompiledExpr(callback = Round.round, retType = DeclType(Var(SymbolRef("value"))))
      ),
      Seq(ComAnn("Rounds the provided value with the given precision"), StdAnn())
    )

  /**
   * Returns the rounded numerical value (3.1234, 2) -> 3.12 ; (3.1264, 2) -> 3.13
   */
  private def round(s: Any): Either[Throwable, Any] =
    val argValue     = "value"     // auto: f32, f64, dec
    val argPrecision = "precision" // i32

    def roundF64(n: Double, p: Int): Double =
      val s: Double = math.pow(10.toDouble, p.toDouble)
      math.round(n * s) / s

    def roundF32(n: Float, p: Int): Float =
      roundF64(n.toDouble, p).toFloat

    def roundDec(n: BigDecimal, p: Int): BigDecimal =
      n.setScale(p, BigDecimal.RoundingMode.HALF_UP)

    s match
      case s @ InterpretState(_, _, ms, c) =>
        for
          valueCell     <- ms.tryFetch(CellPath(argValue))
          precisionCell <- ms.tryFetch(CellPath(argPrecision))
          retVal <- (valueCell, precisionCell) match
                      case (FloatCell(x), IntCell(p)) =>
                        Right(FloatCell(roundF32(x, p)))
                      case (DoubleCell(x), IntCell(p)) =>
                        Right(DoubleCell(roundF64(x, p)))
                      case (DecimalCell(x), IntCell(p)) =>
                        Right(DecimalCell(roundDec(x, p)))
                      case other =>
                        Left(new B1Exception(s"Unexpected type of arguments passed to ${fnName}: ${other}"))
        yield s.copy(memSpace = ms, retValue = retVal)

      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""// NOTE: Add [T: Fractional] to the method
                            |
                            |def roundF64(n: Double, p: Int): Double = {
                            |  val s: Double = math.pow(10.toDouble, p.toDouble)
                            |  math.round(n * s) / s
                            |}
                            |
                            |def roundF32(n: Float, p: Int): Float =
                            |  roundF64(n.toDouble, p).toFloat
                            |
                            |def roundDec(n: BigDecimal, p: Int): BigDecimal =
                            |  n.setScale(p, BigDecimal.RoundingMode.HALF_UP)
                            |
                            |${argValue} match {
                            |  case x: Double =>
                            |    roundF64(x, ${argPrecision}).asInstanceOf[T]
                            |  case x: Float =>
                            |    roundF32(x, ${argPrecision}).asInstanceOf[T]
                            |  case x: BigDecimal =>
                            |    roundDec(x, ${argPrecision}).asInstanceOf[T]
                            |  case other =>
                            |    throw new RuntimeException(s"Cannot round the provided value: $${other}, the type is not supported")
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case s: Scala3JState =>
        for lines <- Right(
                       split(
                         s"""// NOTE: Add [T: JFractional] to the method
                            |
                            |def roundF64(n: JDouble, p: JInteger): JDouble = {
                            |  val s: JDouble = Math.pow(10.0, p.doubleValue())
                            |  Math.round(n * s) / s
                            |}
                            |
                            |def roundF32(n: JFloat, p: JInteger): JFloat =
                            |  roundF64(n.doubleValue(), p).floatValue()
                            |
                            |def roundDec(n: JBigDecimal, p: JInteger): JBigDecimal =
                            |  n.setScale(p, JRoundingMode.HALF_UP)
                            |
                            |${argValue} match {
                            |  case x: JDouble =>
                            |    roundF64(x, ${argPrecision}).asInstanceOf[T]
                            |  case x: JFloat =>
                            |    roundF32(x, ${argPrecision}).asInstanceOf[T]
                            |  case x: JBigDecimal =>
                            |    roundDec(x, ${argPrecision}).asInstanceOf[T]
                            |  case other =>
                            |    throw new RuntimeException(s"Cannot round the provided value: $${other}, the type is not supported")
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
          ),
          inits = s.inits ++ Inits.codeBlocks(Seq(
            Inits.Keys.JFractional
          ))
        )

      case other =>
        Left(new B1Exception(s"Unexpected state passed to ${fnName}: ${other}"))
