package com.github.gchudnov.bscript.b1.internal.stdlib.num

import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.translator.internal.scala2.Scala2State
import com.github.gchudnov.bscript.b1.B1Exception
import com.github.gchudnov.bscript.lang.util.ShowOps.split
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames

private[internal] object Round:

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      DeclType(Var(SymbolRef("value"))),
      "round",
      List(
        ArgDecl(TypeRef(typeNames.autoType), "value"), // f32, f64, dec
        ArgDecl(TypeRef(typeNames.i32Type), "precision")
      ),
      Block(
        CompiledExpr(callback = Round.round, retType = DeclType(Var(SymbolRef("value"))))
      ),
      Seq(ComAnn("rounds the provided value with the given precision"), StdAnn())
    )

  /**
   * Returns the rounded numerical value (3.1234, 2) -> 3.12 (3.1264, 2) -> 3.13
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
      case s @ InterpretState(_, ms, c) =>
        for
          valueCell     <- ms.fetch(CellPath(argValue))
          precisionCell <- ms.fetch(CellPath(argPrecision))
          retVal <- (valueCell, precisionCell) match
                      case (FloatCell(x), IntCell(p)) =>
                        Right(FloatCell(roundF32(x, p)))
                      case (DoubleCell(x), IntCell(p)) =>
                        Right(DoubleCell(roundF64(x, p)))
                      case (DecimalCell(x), IntCell(p)) =>
                        Right(DecimalCell(roundDec(x, p)))
                      case other =>
                        Left(new B1Exception(s"Unexpected type of arguments passed to round: ${other}"))
        yield s.copy(memSpace = ms, retValue = retVal)

      case s: Scala2State =>
        for lines <- Right(
                       split(
                         s"""${argValue}.setScale(${argPrecision}, BigDecimal.RoundingMode.HALF_UP)
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case other =>
        Left(new B1Exception(s"Unexpected state passed to round: ${other}"))