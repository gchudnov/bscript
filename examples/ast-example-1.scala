import java.lang.Math
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import java.time.LocalDate
import java.time.OffsetDateTime
import java.time.ZoneId
import java.time.temporal.{ ChronoUnit, Temporal }
import scala.util.control.Exception.allCatch

{
  /**
   * Offsets the provided date-time
   * [std]
   */
  def offsetDateTime(value: OffsetDateTime, offset: Int, unit: String): OffsetDateTime = {
    val unitDays: String    = "days"
    val unitHours: String   = "hours"
    val unitMinutes: String = "minutes"
    val unitSeconds: String = "seconds"
    
    unit.trim.toLowerCase match {
      case `unitDays` =>
        value.plusDays(offset.toLong)
      case `unitHours` =>
        value.plusHours(offset.toLong)
      case `unitMinutes` =>
        value.plusMinutes(offset.toLong)
      case `unitSeconds` =>
        value.plusSeconds(offset.toLong)
      case _ =>
        throw new RuntimeException(s"Unexpected date-time unit passed to offsetDateTime: '${unit}'")
    }
  }
  /**
   * Offsets the provided date
   * [std]
   */
  def offsetDate(value: LocalDate, offset: Int, unit: String): LocalDate = {
    val unitDays: String    = "days"
    
    unit.trim.toLowerCase match {
      case `unitDays` =>
        value.plusDays(offset.toLong)
      case _ =>
        throw new RuntimeException(s"Unexpected unit of time was passed to offsetDate: '${unit}'")
    }
  }
  /**
   * Calculates difference between two temporal points in time
   * [std]
   */
  def betweenTemp(first: LocalDate, last: LocalDate, unit: String): Int = {
    // NOTE: Add [T <: Temporal] to the method
    
    val unitDays: String    = "days"
    val unitHours: String   = "hours"
    val unitMinutes: String = "minutes"
    val unitSeconds: String = "seconds"
    
    val chronoUnit = unit.trim.toLowerCase match {
      case `unitDays` =>
        ChronoUnit.DAYS
      case `unitHours` =>
        ChronoUnit.HOURS
      case `unitMinutes` =>
        ChronoUnit.MINUTES
      case `unitSeconds` =>
        ChronoUnit.SECONDS
      case _ =>
        throw new RuntimeException(s"Unexpected date-time unit passed to betweenTemp: '${unit}'")
    }
    
    chronoUnit.between(first, last).toInt
  }
  /**
   * Returns the specified field of date-time as an integer value
   * [std]
   */
  def fieldOfDateTime(value: OffsetDateTime, unit: String): Int = {
    val unitDays: String    = "days"
    val unitHours: String   = "hours"
    val unitMinutes: String = "minutes"
    val unitSeconds: String = "seconds"
    
    unit.trim.toLowerCase match {
      case `unitDays` =>
        value.getDayOfMonth
      case `unitHours` =>
        value.getHour
      case `unitMinutes` =>
        value.getMinute
      case `unitSeconds` =>
        value.getSecond
      case _ =>
        throw new RuntimeException(s"Unexpected date-time unit passed to fieldOfDateTime: '${unit}'")
    }
  }
  /**
   * Returns the current date and time
   * [std]
   */
  def now(): OffsetDateTime = {
    OffsetDateTime.now(ZoneId.of("Z"))
  }
  /**
   * Sets a field of datetime to the specified value
   * [std]
   */
  def setDateTime(value: OffsetDateTime, offset: Int, unit: String): OffsetDateTime = {
    val unitDays: String    = "days"
    val unitHours: String   = "hours"
    val unitMinutes: String = "minutes"
    val unitSeconds: String = "seconds"
    
    unit.trim.toLowerCase match {
      case `unitDays` =>
        value.withDayOfMonth(offset)
      case `unitHours` =>
        value.withHour(offset)
      case `unitMinutes` =>
        value.withMinute(offset)
      case `unitSeconds` =>
        value.withSecond(offset)
      case _ =>
        throw new RuntimeException(s"Unexpected date-time unit passed to setDateTime: '${unit}'")
    }
  }
  /**
   * Returns today as date
   * [std]
   */
  def today(): LocalDate = {
    LocalDate.now(ZoneId.of("Z"))
  }
  /**
   * Prints the formatted string to StdOut
   * [std]
   */
  def printf(format: String, value: T): Unit = {}
  /**
   * loads text from a file using the provided path
   * [std]
   */
  def readFile(path: String): String = {
    val errOrContents = for {
      filePath <- allCatch.either(Paths.get(path))
      contents <- allCatch.either(Files.readString(filePath, StandardCharsets.UTF_8))
    } yield contents
    
    errOrContents.toTry.get
  }
  /**
   * Safely casts a number to i32 or returns an error on runtime
   * [std]
   */
  def exactInt(value: T): Int = {
    // NOTE: Add [T: Numeric] to the method
    
    value match {
      case x: Int =>
        x
      case x: Long =>
        Math.toIntExact(x)
      case x: Float =>
        BigDecimal.valueOf(x).toIntExact
      case x: Double =>
        BigDecimal.valueOf(x).toIntExact
      case x: BigDecimal =>
        x.toIntExact
      case other =>
        throw new RuntimeException(s"Cannot safely cast the provided value: ${other}, the type is not supported")
    }
  }
  /**
   * Safely casts a number to i64 or returns an error on runtime
   * [std]
   */
  def exactLong(value: T): Long = {
    // NOTE: Add [T: Numeric] to the method
    
    value match {
      case x: Int =>
        x.toLong
      case x: Long =>
        x
      case x: Float =>
        BigDecimal.valueOf(x).toLongExact
      case x: Double =>
        BigDecimal.valueOf(x).toLongExact
      case x: BigDecimal =>
        x.toLongExact
      case other =>
        throw new RuntimeException(s"Cannot safely cast the provided value: ${other}, the type is not supported")
    }
  }
  /**
   * Rounds the provided value with the given precision
   * [std]
   */
  def round(value: T, precision: Int): T = {
    // NOTE: Add [T: Fractional] to the method
    
    def roundF64(n: Double, p: Int): Double = {
      val s: Double = math.pow(10.toDouble, p.toDouble)
      math.round(n * s) / s
    }
    
    def roundF32(n: Float, p: Int): Float =
      roundF64(n.toDouble, p).toFloat
    
    def roundDec(n: BigDecimal, p: Int): BigDecimal =
      n.setScale(p, BigDecimal.RoundingMode.HALF_UP)
    
    value match {
      case x: Double =>
        roundF64(x, precision).asInstanceOf[T]
      case x: Float =>
        roundF32(x, precision).asInstanceOf[T]
      case x: BigDecimal =>
        roundDec(x, precision).asInstanceOf[T]
      case other =>
        throw new RuntimeException(s"Cannot round the provided value: ${other}, the type is not supported")
    }
  }
  /**
   * Truncates the provided value with the given precision
   * [std]
   */
  def truncate(value: T, precision: Int): T = {
    // NOTE: Add [T: Fractional] to the method
    
    def truncateF64(n: Double, p: Int): Double = {
      val s: Double = math.pow(10.toDouble, p.toDouble)
      if (n < 0.0)
        math.ceil(n * s) / s
      else 
        math.round(n * s) / s
      }
    
    def truncateF32(n: Float, p: Int): Float =
      truncateF64(n.toDouble, p).toFloat
    
    def truncateDec(n: BigDecimal, p: Int): BigDecimal =
      n.setScale(p, BigDecimal.RoundingMode.DOWN)
        
    value match {
      case x: Double =>
        truncateF64(x, precision).asInstanceOf[T]
      case x: Float =>
        truncateF32(x, precision).asInstanceOf[T]
      case x: BigDecimal =>
        truncateDec(x, precision).asInstanceOf[T]
      case other =>
        throw new RuntimeException(s"Cannot truncate the provided value: ${other}, the type is not supported")
    }
  }
  /**
   * Returns the length of the provided string
   * [std]
   */
  def strLen(s: String): Int = {
    s.length
  }
  /**
   * Append an element to the collection.
   * [std]
   */
  def append(x: T, xs: List[T]): List[T] = {
    // NOTE: Add [T] to the method
    xs :+ x
  }
  /**
   * Tests whether the collection contains the given element.
   * [std]
   */
  def contains(x: T, xs: List[T]): Boolean = {
    // NOTE: Add [T] to the method
    xs.contains(x)
  }
  /**
   * returns true of the provided variable is defined, otherwise false
   * [std]
   */
  def isDefined(x: T): Boolean = {
    // NOTE: Add [T] to the method
    x match {
      case null => false
      case None => false
      case _ => true
    }
  }
  /**
   * returns the first non-null value out of two values that were provided
   * [std]
   */
  def coalesce(x: T, y: T): T = {
    // NOTE: Add [T] to the method
    (x, y) match {
      case (null, _) => y
      case (None, _) => y
      case _ => x
    }
  }
  final case class A(
    var x: Int,
    var b: B
  )
  final case class B(
    var y: Int
  )
  var a: A = A(
    x = 0,
    b = B(
        y = 0
      )
  )
  def f(x: Int): Unit = {
    a.b.y = x
  }
  def g(x: Int): Unit = {
    a.x = x
  }
  def h(): Unit = {}
  f(10)
  g(20)
  h()
  a
}