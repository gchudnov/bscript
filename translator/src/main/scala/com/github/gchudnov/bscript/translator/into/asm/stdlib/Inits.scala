package com.github.gchudnov.bscript.translator.into.asm.stdlib

import com.github.gchudnov.bscript.lang.util.LineOps.split
import scala.collection.immutable.Seq

/**
 * Top Level Initializations, required to make the translated code working
 */
object Inits:

  object Keys:
    val ToOrderedLocalDate      = "toOrderedLocalDate"
    val ToOrderedOffsetDateTime = "toOrderedOffsetDateTime"
    val JFractional             = "JFractional"
    val JBigDecimalOps          = "JBigDecimalOps"
    val InlineTest              = "InlineTest"
    val NAConstants             = "NAConstants"

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
    ),
    Keys.NAConstants -> split(
      """
        |const NAdate: Date = Date.parse("1900-01-01");
        |""".stripMargin
    ),
    Keys.InlineTest -> split(
      """
        |// isDefined
        |console.log("\n\n# isDefined\n");
        |console.log("isDefined(\"hello world\"): " + isDefined_string("hello world").toString());
        |console.log("isDefined(\"!#\"): " + isDefined_string("!#").toString());
        |console.log("isDefined(123): " + isDefined_int(123).toString());
        |console.log("isDefined(I32.MIN_VALUE): " + isDefined_int(I32.MIN_VALUE).toString());
        |console.log("isDefined(123L): " + isDefined_long(123).toString());
        |console.log("isDefined(I64.MIN_VALUE): " + isDefined_long(I64.MIN_VALUE).toString());
        |console.log("isDefined(123.0f): " + isDefined_float(123.0).toString());
        |console.log("isDefined(F32.NaN): " + isDefined_float(F32.NaN).toString());
        |console.log("isDefined(123.0): " + isDefined_double(123.0).toString());
        |console.log("isDefined(F64.NaN): " + isDefined_double(F64.NaN).toString());
        |console.log("isDefined_date(Date.parse(\"2024-05-01\")): " + isDefined_date(Date.parse("2024-05-01")).toString());
        |console.log("isDefined_date(Date.parse(\"1900-01-01\")): " + isDefined_date(Date.parse("1900-01-01")).toString());
        |console.log("isDefined_datetime(Date.parse(\"2024-05-01T21:30:43+00:00\")): " + isDefined_datetime(Date.parse("2024-05-01T21:30:43+00:00")).toString());
        |console.log("isDefined_datetime(Date.parse(\"1900-01-01\")): " + isDefined_datetime(Date.parse("1900-01-01")).toString());
        |
        |// now
        |console.log("\n\n# now\n");
        |console.log("now(): " + now().toString());
        |
        |// today
        |console.log("\n\n# today\n");
        |console.log("today(): " + today().toString());
        |
        |// round
        |console.log("\n\n# round\n");
        |console.log("round(123.456, 2): " + round_double(123.456, 2).toString());
        |console.log("round(123.444, 2): " + round_double(123.444, 2).toString());
        |console.log("round(123.456f, 2): " + round_float(123.456, 2).toString());
        |console.log("round(123.444f, 2): " + round_float(123.444, 2).toString());
        |
        |// truncate
        |console.log("\n\n# truncate\n");
        |console.log("truncate(123.456, 2): " + truncate_double(123.456, 2).toString());
        |console.log("truncate(123.444, 2): " + truncate_double(123.444, 2).toString());
        |console.log("truncate(123.456f, 2): " + truncate_float(123.456, 2).toString());
        |console.log("truncate(123.444f, 2): " + truncate_float(123.444, 2).toString());
        |
        |// coalesce
        |console.log("\n\n# coalesce\n");
        |console.log("coalesce(\"a\", \"b\"): " + coalesce_string("a", "b").toString());
        |console.log("coalesce(\"!#\", \"b\"): " + coalesce_string("!#", "b").toString());
        |console.log("coalesce(10, 20): " + coalesce_int(10, 20).toString());
        |console.log("coalesce(I32.MIN_VALUE, 20): " + coalesce_int(I32.MIN_VALUE, 20).toString());
        |console.log("coalesce(10L, 20L): " + coalesce_long(10, 20).toString());
        |console.log("coalesce(I64.MIN_VALUE, 20L): " + coalesce_long(I64.MIN_VALUE, 20).toString());
        |console.log("coalesce(10.0f, 20.0f): " + coalesce_float(10.0, 20.0).toString());
        |console.log("coalesce(F32.NaN, 20.0f): " + coalesce_float(F32.NaN, 20.0).toString());
        |console.log("coalesce(10.0, 20.0): " + coalesce_double(10.0, 20.0).toString());
        |console.log("coalesce(F64.NaN, 20.0): " + coalesce_double(F64.NaN, 20.0).toString());
        |console.log("coalesce(Date.parse(\"2024-05-01\"), Date.parse(\"2024-05-02\")): " + coalesce_date(Date.parse("2024-05-01"), Date.parse("2024-05-02")).toString());
        |console.log("coalesce(Date.parse(\"1900-01-01\"), Date.parse(\"2024-05-02\")): " + coalesce_date(Date.parse("1900-01-01"), Date.parse("2024-05-02")).toString());
        |console.log("coalesce(Date.parse(\"2024-05-01T21:30:43+00:00\"), Date.parse(\"2024-05-02T22:32:44+00:00\")): " + coalesce_datetime(Date.parse("2024-05-01T21:30:43+00:00"), Date.parse("2024-05-02T22:32:44+00:00")).toString());
        |console.log("coalesce(Date.parse(\"1900-01-01\"), Date.parse(\"2024-05-02T22:32:44+00:00\")): " + coalesce_datetime(Date.parse("1900-01-01"), Date.parse("2024-05-02T22:32:44+00:00")).toString());
        |
        |// contains
        |console.log("\n\n# contains\n");
        |console.log("contains(\"a\", [\"a\", \"b\", \"c\"): " + contains_string("a", ["a", "b", "c"]).toString());
        |console.log("contains(\"d\", [\"a\", \"b\", \"c\"): " + contains_string("d", ["a", "b", "c"]).toString());
        |console.log("contains(1, [1, 2, 3]): " + contains_int(1, [1, 2, 3]).toString());
        |console.log("contains(4, [1, 2, 3]): " + contains_int(4, [1, 2, 3]).toString());
        |console.log("contains(1L, [1L, 2L, 3L]): " + contains_long(1, [1, 2, 3]).toString());
        |console.log("contains(4L, [1L, 2L, 3L]): " + contains_long(4, [1, 2, 3]).toString());
        |console.log("contains(1.0f, [1.0f, 2.0f, 3.0f]): " + contains_float(1.0, [1.0, 2.0, 3.0]).toString());
        |console.log("contains(4.0f, [1.0f, 2.0f, 3.0f]): " + contains_float(4.0, [1.0, 2.0, 3.0]).toString());
        |console.log("contains(1.0, [1.0, 2.0, 3.0]): " + contains_double(1.0, [1.0, 2.0, 3.0]).toString());
        |console.log("contains(4.0, [1.0, 2.0, 3.0]): " + contains_double(4.0, [1.0, 2.0, 3.0]).toString());
        |console.log("contains(Date.parse(\"2024-05-01\"), [Date.parse(\"2024-05-01\"), Date.parse(\"2024-05-02\"), Date.parse(\"2024-05-03\")]): " + contains_date(Date.parse("2024-05-01"), [Date.parse("2024-05-01"), Date.parse("2024-05-02"), Date.parse("2024-05-03")]).toString());
        |console.log("contains(Date.parse(\"2024-05-04\"), [Date.parse(\"2024-05-01\"), Date.parse(\"2024-05-02\"), Date.parse(\"2024-05-03\")]): " + contains_date(Date.parse("2024-05-04"), [Date.parse("2024-05-01"), Date.parse("2024-05-02"), Date.parse("2024-05-03")]).toString());
        |console.log("contains(Date.parse(\"2024-05-01T21:30:43+00:00\"), [Date.parse(\"2024-05-01T21:30:43+00:00\"), Date.parse(\"2024-05-02T21:30:43+00:00\"), Date.parse(\"2024-05-03T21:30:43+00:00\")]): " + contains_datetime(Date.parse("2024-05-01T21:30:43+00:00"), [Date.parse("2024-05-01T21:30:43+00:00"), Date.parse("2024-05-02T21:30:43+00:00"), Date.parse("2024-05-03T21:30:43+00:00")]).toString());
        |console.log("contains(Date.parse(\"2024-05-04T21:30:43+00:00\"), [Date.parse(\"2024-05-01T21:30:43+00:00\"), Date.parse(\"2024-05-02T21:30:43+00:00\"), Date.parse(\"2024-05-03T21:30:43+00:00\")]): " + contains_datetime(Date.parse("2024-05-04T21:30:43+00:00"), [Date.parse("2024-05-01T21:30:43+00:00"), Date.parse("2024-05-02T21:30:43+00:00"), Date.parse("2024-05-03T21:30:43+00:00")]).toString());
        |
        |""".stripMargin
    )
  )

  def codeBlocks(keys: Iterable[String]): List[(String, Seq[String])] =
    keys.map(key => (key, m(key))).toList
