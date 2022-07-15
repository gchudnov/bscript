package com.github.gchudnov.bscript.b1.internal.stdlib

import com.github.gchudnov.bscript.lang.util.LineOps.split

/**
 * Top Level Initializations, required to make the translated code working
 */
object Inits:

  object Keys:
    val ToOrderedLocalDate      = "toOrderedLocalDate"
    val ToOrderedOffsetDateTime = "toOrderedOffsetDateTime"
    val JFractional             = "JFractional"
    val JBigDecimalOps          = "JBigDecimalOps"

  private val m = Map(
    Keys.ToOrderedLocalDate -> split(
      s"""// Allow LocalDate ordering: <, <=, >, >=
         |given toOrderedLocalDate: Conversion[LocalDate, Ordered[LocalDate]] with
         |  def apply(x: LocalDate): Ordered[LocalDate] = (y: LocalDate) => x.compareTo(y)
         |""".stripMargin
    ),
    Keys.ToOrderedOffsetDateTime -> split(
      s"""// Allow OffsetDateTime ordering: <, <=, >, >=
         |given toOrderedOffsetDateTime: Conversion[OffsetDateTime, Ordered[OffsetDateTime]] with
         |  def apply(x: OffsetDateTime): Ordered[OffsetDateTime] = (y: OffsetDateTime) => x.compareTo(y)
         |""".stripMargin
    ),
    Keys.JFractional -> split(
      s"""// Fractional trait, but for Java
         |trait JFractional[T]
         |
         |object JFloatFractional extends JFractional[JFloat]
         |object JDoubleFractional extends JFractional[JDouble]
         |object JBigDecimalFractional extends JFractional[JBigDecimal]
         |
         |given jFloatFractional: JFractional[JFloat] =
         |  JFloatFractional
         |
         |given jDoubleFractional: JFractional[JDouble] =
         |  JDoubleFractional
         |
         |given jBigDecimalFractional: JFractional[JBigDecimal] =
         |  JBigDecimalFractional
         |""".stripMargin
    ),
    Keys.JBigDecimalOps -> split(
      s"""object JBigDecimalOps:
         |
         |  // Auto-Convert Double to JBigDecimal
         |  given doubleToJBigDecimal: Conversion[Double, JBigDecimal] with
         |    def apply(x: Double): JBigDecimal =
         |      JBigDecimal.valueOf(x)
         |
         |  // Auto-Convert Float to JBigDecimal
         |  given floatToJBigDecimal: Conversion[Float, JBigDecimal] with
         |    def apply(x: Float): JBigDecimal =
         |      JBigDecimal.valueOf(x)
         |
         |  // Auto-Convert Int to JBigDecimal
         |  given intToJBigDecimal: Conversion[Int, JBigDecimal] with
         |    def apply(x: Int): JBigDecimal =
         |      JBigDecimal.valueOf(x)
         |
         |  // Auto-Convert Long to JBigDecimal
         |  given longToJBigDecimal: Conversion[Long, JBigDecimal] with
         |    def apply(x: Long): JBigDecimal =
         |      JBigDecimal.valueOf(x)
         |
         |  // Auto-Convert Double to JBigDecimal
         |  given jDoubleToJBigDecimal: Conversion[JDouble, JBigDecimal] with
         |    def apply(x: JDouble): JBigDecimal =
         |      Option(x).map(JBigDecimal.valueOf(_)).orNull
         |
         |  // Auto-Convert Float to JBigDecimal
         |  given jFloatToJBigDecimal: Conversion[JFloat, JBigDecimal] with
         |    def apply(x: JFloat): JBigDecimal =
         |      Option(x).map(it => JBigDecimal.valueOf(it.doubleValue())).orNull
         |
         |  // Auto-Convert Int to JBigDecimal
         |  given jIntegerToJBigDecimal: Conversion[JInteger, JBigDecimal] with
         |    def apply(x: JInteger): JBigDecimal =
         |      Option(x).map(it => JBigDecimal.valueOf(it.longValue())).orNull
         |
         |  // Auto-Convert Long to JBigDecimal
         |  given jLongToJBigDecimal: Conversion[JLong, JBigDecimal] with
         |    def apply(x: JLong): JBigDecimal =
         |      Option(x).map(it => JBigDecimal.valueOf(it)).orNull
         |
         |  extension (x: JBigDecimal)
         |    def *(y: JBigDecimal): JBigDecimal =
         |      x.multiply(y)
         |
         |    def /(y: JBigDecimal): JBigDecimal =
         |      x.divide(y)
         |
         |    def %(y: JBigDecimal): JBigDecimal =
         |      x.divideToIntegralValue(y)
         |
         |    def +(y: JBigDecimal): JBigDecimal =
         |      x.add(y)
         |
         |    def -(y: JBigDecimal): JBigDecimal =
         |      x.subtract(y)
         |""".stripMargin
    )
  )

  def codeBlocks(keys: Iterable[String]): Map[String, Seq[String]] =
    keys.map(key => (key, m(key))).toMap
