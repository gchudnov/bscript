package com.github.gchudnov.bscript.interpreter.memory

import com.github.gchudnov.bscript.lang.util.Show
import com.github.gchudnov.bscript.lang.util.LineOps

import java.time.{ LocalDate, OffsetDateTime }

sealed trait Cell

object Cell:

  case object Nothing                               extends Cell
  case object Void                                  extends Cell
  final case class Bool(value: Boolean)             extends Cell
  final case class I32(value: Int)                  extends Cell
  final case class I64(value: Long)                extends Cell
  final case class F32(value: Float)              extends Cell
  final case class F64(value: Double)            extends Cell
  final case class Dec(value: BigDecimal)       extends Cell
  final case class Str(alue: String)               extends Cell
  final case class Date(value: LocalDate)           extends Cell
  final case class DateTime(value: OffsetDateTime)  extends Cell
  final case class Vec(value: List[Cell])           extends Cell
  final case class Struct(value: Map[String, Cell]) extends Cell
  final case class Method(value: List[Cell] => Either[Throwable, Cell]) extends Cell

  object Struct:
    def apply(values: (String, Cell)*): Cell =
      Struct(values.toMap)


  lazy val nothing: Cell =
    Nothing

  lazy val void: Cell =
    Void

  def bool(value: Boolean): Cell =
    Bool(value)

  def i32(value: Int): Cell =
    I32(value)

  def i64(value: Long): Cell =
    I64(value)

  def f32(value: Float): Cell =
    F32(value)

  def f64(value: Double): Cell =
    F64(value)

  def decimal(value: BigDecimal): Cell =
    Dec(value)

  def str(value: String): Cell =
    Str(value)

  def date(value: LocalDate): Cell =
    Date(value)

  def datetime(value: OffsetDateTime): Cell =
    DateTime(value)

  def vec(value: List[Cell]): Cell =
    Vec(value.toList)

  def vec(value: Cell*): Cell =
    Vec(value.toList)

  def struct(value: Map[String, Cell]): Cell =
    Struct(value)

  def struct(values: (String, Cell)*): Cell =
    Struct(values.toMap)

  /**
   * Checks whether two cells have the same type.
   */
  def isSameType(a: Cell, b: Cell): Boolean =
    a.getClass.getSimpleName == b.getClass.getSimpleName

  /**
   * Deep-merges two cells, replacing values in the left cell with values from the right cell.
   */
  def merge(a: Cell, b: Cell): Either[Throwable, Cell] =
    if !(isSameType(a, b)) then Left(new MemoryException(s"Cannot merge cells that have different types: $a, $b"))
    else
      (a, b) match
        case (Struct(ma), Struct(mb)) =>
          mb.foldLeft(Right(ma): Either[Throwable, Map[String, Cell]]) { case (acc, (k, v)) =>
            acc match
              case Left(e) => Left(e)
              case Right(mx) =>
                for y <- mx.get(k).fold(Right(v))(x => merge(x, v))
                yield mx + (k -> v)
          }.map(m => Struct(m))
        case (_, _) =>
          Right(b)

  /**
   * Calculate the difference between two cells.
   */
  def diff(name: String, before: Option[Cell], after: Option[Cell]): List[Diff.Change[Path, Cell]] =
    iterateDiff(Path(List(name)), before, after)

  /**
    * Caclulate the difference between two lists of cells.
    *
    * @param ns Path
    * @param b List of cells
    * @param a List of cells
    * @return The list of changes.
    */
  private def diffList(ns: Path, b: List[Cell], a: List[Cell]): List[Diff.Change[Path, Cell]] =
    val rs = List.newBuilder[Diff.Change[Path, Cell]]

    (b.zip(a)).zipWithIndex.foreach { case ((vb, va), i) =>
      rs ++= iterateDiff(ns.append(s"${i}"), Some(vb), Some(va))
    }

    if b.size > a.size then
      val bTail = b.drop(a.size)
      bTail.zipWithIndex.foreach { case (vb, i) => rs ++= iterateDiff(ns.append(s"${(i + a.size)}"), Some(vb), None) }
    else if a.size > b.size then
      val aTail = a.drop(b.size)
      aTail.zipWithIndex.foreach { case (va, i) => rs ++= iterateDiff(ns.append(s"${i + b.size}"), None, Some(va)) }

    rs.result()


  /**
    * Calculate the difference between two maps of cells.
    *
    * @param ns Path
    * @param b Map of cells
    * @param a Map of cells
    * @return The list of changes.
    */
  private def diffMap(ns: Path, b: Map[String, Cell], a: Map[String, Cell]): List[Diff.Change[Path, Cell]] =
    val rs = List.newBuilder[Diff.Change[Path, Cell]]

    b.foreach { case (k, vb) =>
      val va = a.get(k)
      rs ++= iterateDiff(ns.append(k), Some(vb), va)
    }

    a.foreach { case (k, va) =>
      val vb = b.get(k)
      if vb.isEmpty then rs ++= iterateDiff(ns.append(k), vb, Some(va))
    }

    rs.result()

  /**
    * Calculate the difference between two cells.
    *
    * @param ns Path
    * @param b Maybe Cell
    * @param a Maybe Cell
    * @return The list of changes.
    */
  private def iterateDiff(ns: Path, b: Option[Cell], a: Option[Cell]): List[Diff.Change[Path, Cell]] =
    (b, a) match
      case (Some(Struct(ba)), Some(Struct(aa))) =>
        diffMap(ns, ba, aa)

      case (Some(Vec(ba)), Some(Vec(aa))) =>
        diffList(ns, ba, aa)

      case (Some(x), Some(y)) =>
        if x != y then List(Diff.Updated(ns, x, y))
        else List.empty[Diff.Change[Path, Cell]]

      case (Some(x), None) =>
        List(Diff.Removed(ns, x))

      case (None, Some(x)) =>
        List(Diff.Added(ns, x))

      case (None, None) =>
        List.empty[Diff.Change[Path, Cell]]

  /**
   * Extension Call Operations
   */
  extension (cell: Cell)

    def asStruct: Either[Throwable, Struct] = cell match
      case struct: Struct => Right(struct)
      case other              => Left(new MemoryException(s"Cannot convert ${other} to a Cell.Struct"))

    def asBool: Either[Throwable, Bool] = cell match
      case bool: Bool => Right(bool)
      case other       => Left(new MemoryException(s"Cannot convert ${other} to a Cell.Bool"))

    def asAny: Either[Throwable, Any] = cell match
      case _: Nothing.type => Right(???) // NOTE: it will throw an exception, Nothing is really Nothing
      case _: Void.type    => Right(().asInstanceOf[Any])
      case Bool(value)     => Right(value.asInstanceOf[Any])
      case I32(value)      => Right(value.asInstanceOf[Any])
      case I64(value)     => Right(value.asInstanceOf[Any])
      case F32(value)    => Right(value.asInstanceOf[Any])
      case F64(value)   => Right(value.asInstanceOf[Any])
      case Dec(value)  => Right(value.asInstanceOf[Any])
      case Str(value)      => Right(value.asInstanceOf[Any])
      case Date(value)     => Right(value.asInstanceOf[Any])
      case DateTime(value) => Right(value.asInstanceOf[Any])
      case Vec(value)      => Right(value.asInstanceOf[Any])
      case Struct(value)   => Right(value.asInstanceOf[Any])
      case Method(value)   => Right(value.asInstanceOf[Any])

  given Show[Cell] with
    extension (a: Cell)
      def show: String = a match
        case _: Nothing.type => s"\"nothing\""
        case _: Void.type    => s"\"void\""
        case Bool(value)     => s"\"bool(${value})\""
        case I32(value)      => s"\"i32(${value})\""
        case I64(value)     => s"\"i64(${value})\""
        case F32(value)    => s"\"f32(${value})\""
        case F64(value)   => s"\"f64(${value})\""
        case Dec(value)  => s"\"dec(${value})\""
        case Str(value)      => s"\"str(${value})\""
        case Date(value)     => s"\"date(${value.toString})\""
        case DateTime(value) => s"\"datetime(${value.toString})\""
        case Vec(value) =>
          val lineLines = value.map(it => LineOps.split(it.show))
          val lines     = LineOps.wrap("[", "]", LineOps.wrapEmpty(LineOps.padLines(2, LineOps.joinVAll(", ", lineLines))))
          LineOps.join(lines)
        case Struct(value) =>
          val lineLines = value.toList.map { case (k, v) =>
            val vLines  = LineOps.split(v.show)
            val kvLines = LineOps.joinCR(": ", Seq(s"\"${k}\""), vLines)
            kvLines
          }
          val lines = LineOps.wrap("{", "}", LineOps.wrapEmpty(LineOps.padLines(2, LineOps.joinVAll(",", lineLines))))
          LineOps.join(lines)
        case Method(value) => s"\"method(${value})\""
