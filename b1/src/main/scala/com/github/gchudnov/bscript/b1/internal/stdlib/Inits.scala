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
    )
  )

  def codeBlocks(keys: Iterable[String]): Map[String, Seq[String]] =
    keys.map(key => (key, m(key))).toMap
