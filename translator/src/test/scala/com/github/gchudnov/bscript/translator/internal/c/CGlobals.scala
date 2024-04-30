package com.github.gchudnov.bscript.translator.internal.c

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.TranslateException
import com.github.gchudnov.bscript.translator.internal.c.CState
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State

import java.time.{LocalDate, OffsetDateTime, ZoneId}
import scala.util.control.Exception.allCatch

object CGlobals:

  val typeNames: TypeNames = new TypeNames:
    override def autoType: String     = "auto"
    override def nothingType: String  = "nothing"
    override def voidType: String     = "void"
    override def boolType: String     = "boolean"
    override def i32Type: String      = "int"
    override def i64Type: String      = "long"
    override def f32Type: String      = "float"
    override def f64Type: String      = "double"
    override def decType: String      = "decimal"
    override def strType: String      = "string"
    override def dateType: String     = "date"
    override def datetimeType: String = "datetime"

  def make(): (Meta, SBlock) =
    val g = SBlock("#global")

    val autoType     = SBuiltInType(typeNames.autoType)
    val nothingType  = SBuiltInType(typeNames.nothingType)
    val voidType     = SBuiltInType(typeNames.voidType)
    val boolType     = SBuiltInType(typeNames.boolType)
    val i32Type      = SBuiltInType(typeNames.i32Type)
    val i64Type      = SBuiltInType(typeNames.i64Type)
    val f32Type      = SBuiltInType(typeNames.f32Type)
    val f64Type      = SBuiltInType(typeNames.f64Type)
    val decType      = SBuiltInType(typeNames.decType)
    val strType      = SBuiltInType(typeNames.strType)
    val dateType     = SBuiltInType(typeNames.dateType)
    val datetimeType = SBuiltInType(typeNames.datetimeType)

    val meta =
      Meta.empty
        .defineBlock(g)
        .defineBuiltInType(autoType, g)
        .defineBuiltInType(nothingType, g)
        .defineBuiltInType(voidType, g)
        .defineBuiltInType(boolType, g)
        .defineBuiltInType(i32Type, g)
        .defineBuiltInType(i64Type, g)
        .defineBuiltInType(f32Type, g)
        .defineBuiltInType(f64Type, g)
        .defineBuiltInType(decType, g)
        .defineBuiltInType(strType, g)
        .defineBuiltInType(dateType, g)
        .defineBuiltInType(datetimeType, g)

    (meta, g)

  private val utcZone: ZoneId = ZoneId.of("Z")

  private val unitDays: String    = "days"
  private val unitHours: String   = "hours"
  private val unitMinutes: String = "minutes"
  private val unitSeconds: String = "seconds"

  def prelude: Block = Block(
    MethodDecl(
      TypeRef(typeNames.voidType),
      "printf",
      List(
        ArgDecl(TypeRef(typeNames.strType), "format"),
        ArgDecl(TypeRef(typeNames.autoType), "value")
      ),
      Block.empty,
      Seq(ComAnn("prints the formatted string to StdOut"), StdAnn())
    ),
    MethodDecl(
      TypeRef(typeNames.i32Type),
      "strLen",
      List(
        ArgDecl(TypeRef(typeNames.strType), "s")
      ),
      Block(
        CompiledExpr(callback = strLen, retType = TypeRef(typeNames.i32Type))
      ),
      Seq(ComAnn("returns the length of the provided string"), StdAnn())
    ),
    MethodDecl(
      TypeRef(typeNames.datetimeType),
      "offsetDateTime",
      List(
        ArgDecl(TypeRef(typeNames.datetimeType), "value"),
        ArgDecl(TypeRef(typeNames.i32Type), "offset"),
        ArgDecl(TypeRef(typeNames.strType), "unit")
      ),
      Block(
        CompiledExpr(callback = offsetDateTime, retType = TypeRef(typeNames.datetimeType))
      ),
      Seq(ComAnn("offsets the provided date-time"), StdAnn())
    ),
    MethodDecl(
      TypeRef(typeNames.datetimeType),
      "setDateTime",
      List(
        ArgDecl(TypeRef(typeNames.datetimeType), "value"),
        ArgDecl(TypeRef(typeNames.i32Type), "offset"),
        ArgDecl(TypeRef(typeNames.strType), "unit")
      ),
      Block(
        CompiledExpr(callback = setDateTime, retType = TypeRef(typeNames.datetimeType))
      ),
      Seq(ComAnn("sets data and time to the specified value"), StdAnn())
    ),
    MethodDecl(
      TypeRef(typeNames.i32Type),
      "fieldOfDateTime",
      List(
        ArgDecl(TypeRef(typeNames.datetimeType), "value"),
        ArgDecl(TypeRef(typeNames.strType), "unit")
      ),
      Block(
        CompiledExpr(callback = fieldOfDateTime, retType = TypeRef(typeNames.i32Type))
      ),
      Seq(ComAnn("return the specified part of date-time as an integer value"), StdAnn())
    ),
    MethodDecl(
      TypeRef(typeNames.boolType),
      "isDefined",
      List(
        ArgDecl(TypeRef(typeNames.autoType), "x")
      ),
      Block(
        CompiledExpr(callback = isDefined, retType = TypeRef(typeNames.boolType))
      ),
      Seq(ComAnn("returns true of the provided variable is defined, otherwise false"), StdAnn())
    ),
    MethodDecl(
      DeclType(Var(SymbolRef("x"))),
      "coalesce",
      List(
        ArgDecl(TypeRef(typeNames.autoType), "x"),
        ArgDecl(TypeRef(typeNames.autoType), "y")
      ),
      Block(
        CompiledExpr(callback = coalesce, retType = DeclType(Var(SymbolRef("x"))))
      ),
      Seq(ComAnn("returns the first non-null value out of two values that were provided"), StdAnn())
    ),
    MethodDecl(
      TypeRef(typeNames.dateType),
      "today",
      List.empty[ArgDecl],
      Block(
        CompiledExpr(callback = today, retType = TypeRef(typeNames.dateType))
      ),
      Seq(ComAnn("returns today as date"), StdAnn())
    ),
    MethodDecl(
      TypeRef(typeNames.datetimeType),
      "now",
      List.empty[ArgDecl],
      Block(
        CompiledExpr(callback = now, retType = TypeRef(typeNames.datetimeType))
      ),
      Seq(ComAnn("returns current date and time as date-time"), StdAnn())
    ),
    MethodDecl(
      DeclType(Var(SymbolRef("value"))),
      "round",
      List(
        ArgDecl(TypeRef(typeNames.autoType), "value"), // f32, f64, dec
        ArgDecl(TypeRef(typeNames.i32Type), "precision")
      ),
      Block(
        CompiledExpr(callback = round, retType = DeclType(Var(SymbolRef("value"))))
      ),
      Seq(ComAnn("rounds the provided value with the given precision"), StdAnn())
    ),
    MethodDecl(
      DeclType(Var(SymbolRef("value"))),
      "truncate",
      List(
        ArgDecl(TypeRef(typeNames.autoType), "value"), // f32, f64, dec
        ArgDecl(TypeRef(typeNames.i32Type), "precision")
      ),
      Block(
        CompiledExpr(callback = truncate, retType = DeclType(Var(SymbolRef("value"))))
      ),
      Seq(ComAnn("truncates the provided value with the given precision"), StdAnn())
    ),
    MethodDecl(
      TypeRef(typeNames.boolType),
      "contains",
      List(
        ArgDecl(TypeRef(typeNames.i32Type), "x"),
        ArgDecl(VectorType(DeclType(Var(SymbolRef("x")))), "xs")
      ),
      Block(
        CompiledExpr(callback = contains, retType = TypeRef(typeNames.boolType))
      ),
      Seq(ComAnn("Tests whether the collection contains the given element"), StdAnn())
    )
  )

  /**
   * Gets the length of a provided string
   *
   * {{{
   *   string s = "123"
   *   int sz = strLen(s);
   * }}}
   */
  private def strLen(s: Any): Either[Throwable, Any] =
    val arg0 = "s"

    s match
      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""${arg0}.length
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case s: CState =>
        Right(s) // TODO: change later

      case other =>
        Left(new TranslateException(s"Unexpected state passed to strLen: ${other}"))

  /**
   * Adds the duration expressed as (offset, unit) to a datetime.
   *
   * {{{
   *   datetime d1 = "YYYY-MM-DD HH:MM:SS";
   *   datetime d2 = offsetDateTime(d1, 12, "hours");
   *   datetime d3 = offsetDateTime(d1, 30, "seconds");
   * }}}
   */
  private def offsetDateTime(s: Any): Either[Throwable, Any] =
    val argValue  = "value"  // datetime
    val argOffset = "offset" // integer offset
    val argUnit   = "unit"   // string unit of the offset (DAYS | HOURS | MINUTES | SECONDS)

    s match
      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""${argUnit}.trim.toLowerCase match {
                            |  case `unitDays` =>
                            |    ${argValue}.plusDays(${argOffset}.toLong)
                            |  case `unitHours` =>
                            |    ${argValue}.plusHours(${argOffset}.toLong)
                            |  case `unitMinutes` =>
                            |    ${argValue}.plusMinutes(${argOffset}.toLong)
                            |  case `unitSeconds` =>
                            |    ${argValue}.plusSeconds(${argOffset}.toLong)
                            |  case other =>
                            |    throw new RuntimeException(s"Unexpected unit of time was passed to offsetDateTime: $${${argUnit}}")
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case s: CState =>
        Right(s) // TODO: change later

      case other =>
        Left(new TranslateException(s"Unexpected state passed to offsetDateTime: ${other}"))

  /**
   * Sets the specified part of a datetime
   *
   * {{{
   *   datetime d1 = "YYYY-MM-DD HH:MM:SS";
   *   datetime d2 = setDateTime(d1, 12, "hours");
   *   datetime d3 = setDateTime(d1, 30, "seconds");
   * }}}
   */
  private def setDateTime(s: Any): Either[Throwable, Any] =
    val argValue  = "value"  // datetime
    val argOffset = "offset" // integer offset
    val argUnit   = "unit"   // string unit of the offset (DAYS | HOURS | MINUTES | SECONDS)

    s match
      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""${argUnit}.trim.toLowerCase match {
                            |  case `unitDays` =>
                            |    ${argValue}.withDayOfMonth(${argOffset})
                            |  case `unitHours` =>
                            |    ${argValue}.withHour(${argOffset})
                            |  case `unitMinutes` =>
                            |    ${argValue}.withMinute(${argOffset})
                            |  case `unitSeconds` =>
                            |    ${argValue}.withSecond(${argOffset})
                            |  case other =>
                            |    throw new RuntimeException(s"Unexpected unit of time was passed to setDateTime: $${${argUnit}}")
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case s: CState =>
        Right(s) // TODO: change later

      case other =>
        Left(new TranslateException(s"Unexpected state passed to setDateTime: ${other}"))

  /**
   * Gets a field from datetime
   *
   * {{{
   *   datetime d1 = "YYYY-MM-DD HH:MM:SS";
   *   int dd = fieldOfDateTime(d1, "days");
   *   int hh = fieldOfDateTime(d1, "hours");
   * }}}
   */
  private def fieldOfDateTime(s: Any): Either[Throwable, Any] =
    val argValue = "value" // datetime
    val argUnit  = "unit"  // string unit of the offset (DAYS | HOURS | MINUTES | SECONDS)

    s match
      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""${argUnit}.trim.toLowerCase match {
                            |  case `unitDays` =>
                            |    ${argValue}.getDayOfMonth
                            |  case `unitHours` =>
                            |    ${argValue}.getHour
                            |  case `unitMinutes` =>
                            |    ${argValue}.getMinute
                            |  case `unitSeconds` =>
                            |    ${argValue}.getSecond
                            |  case other =>
                            |    throw new RuntimeException(s"Unexpected unit of time was passed to fieldOfDateTime: $${${argUnit}}")
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case s: CState =>
        Right(s) // TODO: change later

      case other =>
        Left(new TranslateException(s"Unexpected state passed to fieldOfDateTime: ${other}"))

  /**
   * checks that the provided parameter is not null
   *
   * {{{
   *   int x = null;
   *   bool xr = isDefined(x); // false
   *
   *   float y = 12.34;
   *   bool yr = isDefined(y); // true
   * }}}
   */
  private def isDefined(s: Any): Either[Throwable, Any] =
    val argX = "x" // auto

    s match
      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""${argX} match {
                            |  case null => false
                            |  case None => false
                            |  case _ => true
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case s: CState =>
        Right(s) // TODO: change later

      case other =>
        Left(new TranslateException(s"Unexpected state passed to isDefined: ${other}"))

  /**
   * returns the first argument if not null, otherwise the second one
   *
   * {{{
   *   int x = null;
   *   int y = 17;
   *   int z = coalesce(x, y);
   *   z; // 17
   * }}}
   */
  private def coalesce(s: Any): Either[Throwable, Any] =
    val argX = "x" // auto
    val argY = "y" // auto

    s match
      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""(${argX}, ${argY}) match {
                            |  case (null, _) => ${argY}
                            |  case (None, _) => ${argY}
                            |  case _ => ${argX}
                            |}
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case s: CState =>
        Right(s) // TODO: change later

      case other =>
        Left(new TranslateException(s"Unexpected state passed to coalesce: ${other}"))

  /**
   * Returns current date in UTC format: YYYY-MM-DD
   */
  private def today(s: Any): Either[Throwable, Any] =
    s match
      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""LocalDate.now(ZoneId.of("Z"))
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case s: CState =>
        Right(s) // TODO: change later

      case other =>
        Left(new TranslateException(s"Unexpected state passed to today: ${other}"))

  /**
   * Returns current datetime in UTC format: 2021-12-11T13:20:13.521645485Z
   */
  private def now(s: Any): Either[Throwable, Any] =
    s match
      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""OffsetDateTime.now(ZoneId.of("Z"))
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case s: CState =>
        Right(s) // TODO: change later

      case other =>
        Left(new TranslateException(s"Unexpected state passed to now: ${other}"))

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
      case s: Scala3State =>
        for lines <- Right(
                       split(
                         s"""${argValue}.setScale(${argPrecision}, BigDecimal.RoundingMode.HALF_UP)
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case s: CState =>
        Right(s) // TODO: change later

      case other =>
        Left(new TranslateException(s"Unexpected state passed to round: ${other}"))

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
                         s"""${argValue}.setScale(${argPrecision}, BigDecimal.RoundingMode.DOWN)
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case s: CState =>
        Right(s) // TODO: change later

      case other =>
        Left(new TranslateException(s"Unexpected state passed to truncate: ${other}"))

  private def contains(s: Any): Either[Throwable, Any] =
    val argX  = "x"  // auto
    val argXS = "xs" // vec[decltype(x)]

    s match
      case s: CState =>
        for lines <- Right(
          split(
            s"""for (int i = 0; i < size; i++) {
               |      if (arr[i] == value) {
               |          return 1; // Return true if the value is found in the array
               |      }
               |  }
               |  return 0; // Return false if the value is not found in the array
               |""".stripMargin
          )
        )
        yield s.copy(lines = lines)

      case other =>
        Left(new TranslateException(s"Unexpected state passed to contains: ${other}"))
