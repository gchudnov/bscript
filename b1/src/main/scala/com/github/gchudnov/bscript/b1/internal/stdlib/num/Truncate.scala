package com.github.gchudnov.bscript.b1.internal.stdlib.num

import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.translator.internal.scala2.Scala2State
import com.github.gchudnov.bscript.b1.B1Exception
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames

private[internal] object Truncate:

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      DeclType(Var(SymbolRef("value"))),
      "truncate",
      List(
        ArgDecl(TypeRef(typeNames.autoType), "value"), // f32, f64, dec
        ArgDecl(TypeRef(typeNames.i32Type), "precision")
      ),
      Block(
        CompiledExpr(callback = Truncate.truncate, retType = DeclType(Var(SymbolRef("value"))))
      ),
      Seq(ComAnn("Truncates the provided value with the given precision"), StdAnn())
    )

  /**
   * Returns the truncated numerical value (3.1234, 2) -> 3.12 (3.1264, 2) -> 3.12
   */
  private def truncate(s: Any): Either[Throwable, Any] =
    val argValue     = "value"     // auto: f32, f64, dec
    val argPrecision = "precision" // i32

    def truncateF64(n: Double, p: Int): Double =
      val s: Double = math.pow(10.toDouble, p.toDouble)
      if n < 0.0 then math.ceil(n * s) / s
      else math.round(n * s) / s

    def truncateF32(n: Float, p: Int): Float =
      truncateF64(n.toDouble, p).toFloat

    def truncateDec(n: BigDecimal, p: Int): BigDecimal =
      n.setScale(p, BigDecimal.RoundingMode.DOWN)

    s match
      case s @ InterpretState(_, ms, c) =>
        for
          valueCell     <- ms.fetch(CellPath(argValue))
          precisionCell <- ms.fetch(CellPath(argPrecision))
          retVal <- (valueCell, precisionCell) match
                      case (FloatCell(x), IntCell(p)) =>
                        Right(FloatCell(truncateF32(x, p)))
                      case (DoubleCell(x), IntCell(p)) =>
                        Right(DoubleCell(truncateF64(x, p)))
                      case (DecimalCell(x), IntCell(p)) =>
                        Right(DecimalCell(truncateDec(x, p)))
                      case other =>
                        Left(new B1Exception(s"Unexpected type of arguments passed to truncate: ${other}"))
        yield s.copy(memSpace = ms, retValue = retVal)

      case s: Scala2State =>
        for lines <- Right(
                       split(
                         s"""// NOTE: Add [T: Fractional] to the method
                            |
                            |def truncateF64(n: Double, p: Int): Double = {
                            |  val s: Double = math.pow(10.toDouble, p.toDouble)
                            |  if (n < 0.0)
                            |    math.ceil(n * s) / s
                            |  else 
                            |    math.round(n * s) / s
                            |  }
                            |
                            |def truncateF32(n: Float, p: Int): Float =
                            |  truncateF64(n.toDouble, p).toFloat
                            |
                            |def truncateDec(n: BigDecimal, p: Int): BigDecimal =
                            |  n.setScale(p, BigDecimal.RoundingMode.DOWN)
                            |    
                            |${argValue} match {
                            |  case x: Double =>
                            |    truncateF64(x, ${argPrecision}).asInstanceOf[T]
                            |  case x: Float =>
                            |    truncateF32(x, ${argPrecision}).asInstanceOf[T]
                            |  case x: BigDecimal =>
                            |    truncateDec(x, ${argPrecision}).asInstanceOf[T]
                            |  case other =>
                            |    throw new RuntimeException(s"Cannot truncate the provided value: $${other}, the type is not supported")
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case other =>
        Left(new B1Exception(s"Unexpected state passed to truncate: ${other}"))
