package com.github.gchudnov.bscript.interpreter.memory

import com.github.gchudnov.bscript.lang.util.Show
import com.github.gchudnov.bscript.lang.util.LineOps

import java.time.{ LocalDate, OffsetDateTime }

trait Cell

case object NothingCell                               extends Cell
case object VoidCell                                  extends Cell
final case class BoolCell(value: Boolean)             extends Cell
final case class IntCell(value: Int)                  extends Cell
final case class LongCell(value: Long)                extends Cell
final case class FloatCell(value: Float)              extends Cell
final case class DoubleCell(value: Double)            extends Cell
final case class DecimalCell(value: BigDecimal)       extends Cell
final case class StrCell(value: String)               extends Cell
final case class DateCell(value: LocalDate)           extends Cell
final case class DateTimeCell(value: OffsetDateTime)  extends Cell
final case class VecCell(value: List[Cell])           extends Cell
final case class StructCell(value: Map[String, Cell]) extends Cell

object StructCell:
  def apply(values: (String, Cell)*): Cell =
    StructCell(values.toMap)

object Cell:

  val nothing: Cell =
    NothingCell

  val void: Cell =
    VoidCell

  def bool(value: Boolean): Cell =
    BoolCell(value)

  def i32(value: Int): Cell =
    IntCell(value)

  def i64(value: Long): Cell =
    LongCell(value)

  def f32(value: Float): Cell =
    FloatCell(value)

  def f64(value: Double): Cell =
    DoubleCell(value)

  def decimal(value: BigDecimal): Cell =
    DecimalCell(value)

  def str(value: String): Cell =
    StrCell(value)

  def date(value: LocalDate): Cell =
    DateCell(value)

  def datetime(value: OffsetDateTime): Cell =
    DateTimeCell(value)

  def vec(value: Seq[Cell]): Cell =
    VecCell(value.toList)

  def struct(value: Map[String, Cell]): Cell =
    StructCell(value)

  // APPLY

  def apply(value: Boolean): Cell =
    bool(value)

  def apply(value: Int): Cell =
    i32(value)

  def apply(value: Long): Cell =
    i64(value)

  def apply(value: Float): Cell =
    f32(value)

  def apply(value: Double): Cell =
    f64(value)

  def apply(value: BigDecimal): Cell =
    decimal(value)

  def apply(value: String): Cell =
    str(value)

  def apply(value: LocalDate): Cell =
    date(value)

  def apply(value: OffsetDateTime): Cell =
    datetime(value)

  def apply(value: Seq[Cell]): Cell =
    vec(value.toList)

  def apply(value: Map[String, Cell]): Cell =
    struct(value)

  /**
   * Checks whether two cells have the same type.
   */
  def haveSameType(a: Cell, b: Cell): Boolean =
    a.getClass.getSimpleName == b.getClass.getSimpleName

  /**
   * Deep-merges two cells, replacing values in the left cell with values from the right cell.
   */
  def merge(a: Cell, b: Cell): Either[Throwable, Cell] =
    if !(haveSameType(a, b)) then Left(new MemoryException(s"Cannot merge cells that have different types: $a, $b"))
    else
      (a, b) match
        case (StructCell(ma), StructCell(mb)) =>
          mb.foldLeft(Right(ma): Either[Throwable, Map[String, Cell]]) { case (acc, (k, v)) =>
            acc match
              case Left(e) => Left(e)
              case Right(mx) =>
                for y <- mx.get(k).fold(Right(v))(x => merge(x, v))
                yield mx + (k -> v)
          }.map(m => StructCell(m))
        case (_, _) =>
          Right(b)

  /**
   * Implicit Call Operations
   */
  implicit class CellOps(cell: Cell):

    def asStructCell: Either[Throwable, StructCell] = cell match
      case struct: StructCell => Right(struct)
      case other              => Left(new MemoryException(s"Cannot convert ${other} to StructCell"))

    def asAny: Either[Throwable, Any] = cell match
      case _: NothingCell.type => Right(???) // NOTE: it will throw an exception, Nothing is really Nothing
      case _: VoidCell.type    => Right(().asInstanceOf[Any])
      case BoolCell(value)     => Right(value.asInstanceOf[Any])
      case IntCell(value)      => Right(value.asInstanceOf[Any])
      case LongCell(value)     => Right(value.asInstanceOf[Any])
      case FloatCell(value)    => Right(value.asInstanceOf[Any])
      case DoubleCell(value)   => Right(value.asInstanceOf[Any])
      case DecimalCell(value)  => Right(value.asInstanceOf[Any])
      case StrCell(value)      => Right(value.asInstanceOf[Any])
      case DateCell(value)     => Right(value.asInstanceOf[Any])
      case DateTimeCell(value) => Right(value.asInstanceOf[Any])
      case VecCell(value)      => Right(value.asInstanceOf[Any])
      case StructCell(value)   => Right(value.asInstanceOf[Any])

    def asBoolean: Either[Throwable, Boolean] = cell match
      case BoolCell(x) => Right(x)
      case other       => Left(new MemoryException(s"Cannot convert ${other} to Boolean"))

  implicit val cellShow: Show[Cell] = new Show[Cell]:
    override def show(a: Cell): String = a match
      case _: NothingCell.type => s"\"nothing\""
      case _: VoidCell.type    => s"\"void\""
      case BoolCell(value)     => s"\"bool(${value})\""
      case IntCell(value)      => s"\"i32(${value})\""
      case LongCell(value)     => s"\"i64(${value})\""
      case FloatCell(value)    => s"\"f32(${value})\""
      case DoubleCell(value)   => s"\"f64(${value})\""
      case DecimalCell(value)  => s"\"dec(${value})\""
      case StrCell(value)      => s"\"str(${value})\""
      case DateCell(value)     => s"\"date(${value.toString})\""
      case DateTimeCell(value) => s"\"datetime(${value.toString})\""
      case VecCell(value) =>
        val lineLines = value.map(it => LineOps.split(show(it)))
        val lines     = LineOps.wrap("[", "]", LineOps.wrapEmpty(LineOps.padLines(2, LineOps.joinVAll(", ", lineLines))))
        LineOps.join(lines)
      case StructCell(value) =>
        val lineLines = value.toList.map { case (k, v) =>
          val vLines  = LineOps.split(show(v))
          val kvLines = LineOps.joinCR(": ", Seq(s"\"${k}\""), vLines)
          kvLines
        }
        val lines = LineOps.wrap("{", "}", LineOps.wrapEmpty(LineOps.padLines(2, LineOps.joinVAll(",", lineLines))))
        LineOps.join(lines)
