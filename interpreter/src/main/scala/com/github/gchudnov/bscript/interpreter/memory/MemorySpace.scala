package com.github.gchudnov.bscript.interpreter.memory

import com.github.gchudnov.bscript.lang.util.Show
import com.github.gchudnov.bscript.lang.util.LineOps
import com.github.gchudnov.bscript.interpreter.memory.StructCell

/**
 * Immutable Memory Area
 */
case class MemorySpace(name: String, members: Map[String, Cell], parent: Option[MemorySpace]):

  def get(id: String): Option[Cell] =
    members.get(id).orElse(parent.flatMap(_.get(id)))

  def tryGet(id: String): Either[Throwable, Cell] =
    get(id)
      .toRight(new MemoryException(s"Cannot find the Cell for: '${id}'"))

  def fetch(path: CellPath): Either[Throwable, Cell] =
    def iterate(ps: List[String], where: Cell): Either[Throwable, Cell] = ps match
      case h :: tail =>
        where match
          case sc: StructCell =>
            sc.value
              .get(h)
              .toRight(new MemoryException(s"Cannot find field '${h}' in ${sc}"))
              .flatMap(c => iterate(tail, c))
          case other =>
            Left(new MemoryException(s"Cell ${other} doesn't have fields to fetch field '${h}'"))
      case Nil =>
        Right(where)

    val parts = path.split
    if parts.isEmpty then Left(new MemoryException(s"Path to fetch a Cell is empty"))
    else
      val (h, tail) = (parts.head, parts.tail)
      get(h)
        .toRight(new MemoryException(s"Cannot find MemorySpace with variable '${h}'"))
        .flatMap(c => iterate(tail, c))

  def put(id: String, value: Cell): MemorySpace =
    MemorySpace(name, members + (id -> value), parent)

  def update(id: String, value: Cell): Option[MemorySpace] =
    members
      .get(id)
      .map(_ => MemorySpace(name, members = members + (id -> value), parent))
      .orElse(parent.flatMap(_.update(id, value).map(updParent => MemorySpace(name, members, Some(updParent)))))

  def tryUpdate(id: String, value: Cell): Either[Throwable, MemorySpace] =
    update(id, value)
      .toRight(new MemoryException(s"Cannot find MemorySpace for: '${id}'"))

  def patch(path: CellPath, value: Cell): Either[Throwable, MemorySpace] =
    def iterate(ps: List[String], where: Cell): Either[Throwable, Cell] = ps match
      case h :: tail =>
        where match
          case sc: StructCell =>
            sc.value
              .get(h)
              .toRight(new MemoryException(s"Cannot find field '${h}' in ${sc}"))
              .flatMap(c => iterate(tail, c).map(uc => StructCell(sc.value.updated(h, uc))))
          case other =>
            Left(new MemoryException(s"Cell ${other} doesn't have fields to fetch field '${h}'"))
      case Nil =>
        Right(value)

    val parts = path.split
    if parts.isEmpty then Left(new MemoryException(s"Path to update a Cell is empty"))
    else
      val (h, tail) = (parts.head, parts.tail)
      get(h)
        .toRight(new MemoryException(s"Cannot find MemorySpace with variable '${h}'"))
        .flatMap(c => iterate(tail, c).flatMap(uc => update(h, uc).toRight(new MemoryException(s"Cannot update MemorySpace with the updated cell ${uc} at '${h}'"))))

  def pop(): Either[Throwable, MemorySpace] =
    parent.toRight(new MemoryException(s"Cannot pop a memory space, getting a parent one."))

  override def toString: String =
    val pairs = members.map(it => s"${it._1}: ${it._2}")
    s"[${name}]${pairs.mkString("{", ", ", "}")}"

object MemorySpace:

  private val sep: String = ":"

  def apply(name: String): MemorySpace =
    new MemorySpace(name = name, members = Map.empty[String, Cell], parent = None)

  def apply(name: String, members: Map[String, Cell]): MemorySpace =
    new MemorySpace(name = name, members = members, parent = None)

  def apply(name: String, parent: Option[MemorySpace]): MemorySpace =
    new MemorySpace(name = name, members = Map.empty[String, Cell], parent = parent)

  /**
   * Returns an *unordered* diff between two memory spaces
   */
  def diff(before: MemorySpace, after: MemorySpace): Either[Throwable, List[Diff.Change[String, Cell]]] =

    def iterate(ns: List[String], a: MemorySpace, b: MemorySpace): Either[Throwable, List[Diff.Change[String, Cell]]] =
      if a.name != b.name then Left(new MemoryException(s"Cannot calculate the diff between unrelated memory spaces '${a.name}' and '${b.name}'"))
      else
        val ns1 = ns :+ a.name
        val errOrParentDiff = (a.parent, b.parent) match
          case (Some(x), None) =>
            iterate(ns1, x, MemorySpace(name = x.name))
          case (None, Some(x)) =>
            iterate(ns1, MemorySpace(name = x.name), x)
          case (Some(x), Some(y)) =>
            iterate(ns1, x, y)
          case (None, None) =>
            Right(List.empty[Diff.Change[String, Cell]])

        errOrParentDiff.map(_ ++ Diff.calc(a.members, b.members).map(appendNamePrefix((ns :+ a.name).mkString(sep), _)))

    iterate(List.empty[String], before, after)

  private[memory] def appendNamePrefix[V](prefix: String, change: Diff.Change[String, V]): Diff.Change[String, V] =
    change match
      case Diff.Removed(k, v)    => Diff.Removed(s"${prefix}${sep}${k}", v)
      case Diff.Added(k, v)      => Diff.Added(s"${prefix}${sep}${k}", v)
      case Diff.Updated(k, b, a) => Diff.Updated(s"${prefix}${sep}${k}", b, a)

  implicit val memorySpaceShow: Show[MemorySpace] = new Show[MemorySpace]:
    import Cell.*
    import Show.*

    override def show(a: MemorySpace): String =

      def iterate(d: Int, ms: MemorySpace): Seq[String] =
        val parentLines = ms.parent.fold(Seq.empty[String])(p => iterate(d - 1, p))

        val membersCell: Cell = StructCell(ms.members)
        val membersCellLines  = LineOps.split(membersCell.show())

        val nameLines       = Seq(s"\"name\": \"${ms.name}\"")
        val depthLines      = Seq(s"\"depth\": ${d}")
        val parentNameLines = Seq(s"\"parent\": ${ms.parent.map(it => LineOps.quote(it.name)).getOrElse("null")}")
        val membersLines    = LineOps.joinCR(": ", Seq("\"members\""), membersCellLines)

        val lineLines = Seq(
          nameLines,
          depthLines,
          parentNameLines,
          membersLines
        )

        val objLines = LineOps.wrap("{", "}", LineOps.wrapEmpty(LineOps.padLines(2, LineOps.joinVAll(",", lineLines))))
        LineOps.joinVAll(",", Seq(parentLines, objLines))

      LineOps.join(LineOps.wrap("[", "]", iterate(0, a)))
