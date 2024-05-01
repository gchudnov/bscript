package com.github.gchudnov.bscript.translator.internal.asm.stdlib.num

import com.github.gchudnov.bscript.translator.internal.asm.AsmException
import com.github.gchudnov.bscript.translator.internal.asm.stdlib.Inits
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State
import com.github.gchudnov.bscript.translator.internal.scalax.scala3j.Scala3JState

private[internal] object Truncate:

  private val fnName = "truncate"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      DeclType(Var(SymbolRef("value"))),
      fnName,
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
      case s: Scala3State =>
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

      case s: Scala3JState =>
        for lines <- Right(
                       split(
                         s"""// NOTE: Add [T: JFractional] to the method
                            |
                            |def truncateF64(n: JDouble, p: JInteger): JDouble = {
                            |  val s: JDouble = Math.pow(10.0, p.doubleValue())
                            |  if (n < 0.0) then
                            |    Math.ceil(n * s) / s
                            |  else
                            |    Math.round(n * s) / s
                            |}
                            |
                            |def truncateF32(n: JFloat, p: JInteger): JFloat =
                            |  truncateF64(n.toDouble, p).floatValue()
                            |
                            |def truncateDec(n: JBigDecimal, p: JInteger): JBigDecimal =
                            |  n.setScale(p, JRoundingMode.DOWN)
                            |    
                            |${argValue} match {
                            |  case x: JDouble =>
                            |    truncateF64(x, ${argPrecision}).asInstanceOf[T]
                            |  case x: JFloat =>
                            |    truncateF32(x, ${argPrecision}).asInstanceOf[T]
                            |  case x: JBigDecimal =>
                            |    truncateDec(x, ${argPrecision}).asInstanceOf[T]
                            |  case other =>
                            |    throw new RuntimeException(s"Cannot truncate the provided value: $${other}, the type is not supported")
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
            "java.math.RoundingMode as JRoundingMode",
            "java.lang.Float as JFloat",
            "java.lang.Double as JDouble"
          ),
          inits = s.inits ++ Inits.codeBlocks(
            Seq(
              Inits.Keys.JFractional,
              Inits.Keys.JBigDecimalOps
            )
          )
        )

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
