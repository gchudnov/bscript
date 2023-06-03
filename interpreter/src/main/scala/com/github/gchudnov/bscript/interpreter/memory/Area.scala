package com.github.gchudnov.bscript.interpreter.memory

import com.github.gchudnov.bscript.lang.util.Show
import com.github.gchudnov.bscript.lang.util.LineOps
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.interpreter.memory.Path

/**
 * Immutable Memory Area
 */
case class Area(name: String, members: Map[String, Cell], parent: Option[Area]):

  def get(id: String): Option[Cell] =
    members.get(id).orElse(parent.flatMap(_.get(id)))

  def tryGet(id: String): Either[Throwable, Cell] =
    get(id)
      .toRight(new MemoryException(s"Cannot find the Cell for: '${id}'"))

  def fetch(path: Path): Option[Cell] =
    def iterate(ps: Path, where: Cell): Option[Cell] = ps match
      case Path(h, tail) =>
        where match
          case sc: Cell.Struct =>
            sc.value
              .get(h)
              .flatMap(c => iterate(tail, c))
          case other =>
            None
      case _ =>
        Some(where)

    val parts = path
    if parts.isEmpty then None
    else
      val (h, tail) = (parts.head, parts.tail)
      get(h)
        .flatMap(c => iterate(tail, c))

  def tryFetch(path: Path): Either[Throwable, Cell] =
    def iterate(ps: Path, where: Cell): Either[Throwable, Cell] = ps match
      case Path(h, tail) =>
        where match
          case sc: Cell.Struct =>
            sc.value
              .get(h)
              .toRight(new MemoryException(s"Cannot find field '${h}' in ${sc}"))
              .flatMap(c => iterate(tail, c))
          case other =>
            Left(new MemoryException(s"Cell ${other} doesn't have fields to fetch field '${h}'"))
      case _ =>
        Right(where)

    if path.isEmpty then Left(new MemoryException(s"Path to fetch a Cell is empty"))
    else
      val (h, tail) = (path.head, path.tail)
      get(h)
        .toRight(new MemoryException(s"Cannot find Area with variable '${h}'"))
        .flatMap(c => iterate(tail, c))

  def put(id: String, value: Cell): Area =
    Area(name, members + (id -> value), parent)

  def update(id: String, value: Cell): Option[Area] =
    members
      .get(id)
      .map(_ => Area(name, members = members + (id -> value), parent))
      .orElse(parent.flatMap(_.update(id, value).map(updParent => Area(name, members, Some(updParent)))))

  def tryUpdate(id: String, value: Cell): Either[Throwable, Area] =
    update(id, value)
      .toRight(new MemoryException(s"Cannot find Area for: '${id}'"))

  def tryPatch(path: Path, value: Cell): Either[Throwable, Area] =
    def iterate(ps: Path, where: Cell): Either[Throwable, Cell] = ps match
      case Path(h, tail) =>
        where match
          case sc: Cell.Struct =>
            sc.value
              .get(h)
              .toRight(new MemoryException(s"Cannot find field '${h}' in ${sc}"))
              .flatMap(c => iterate(tail, c).map(uc => Cell.Struct(sc.value.updated(h, uc))))
          case other =>
            Left(new MemoryException(s"Cell ${other} doesn't have fields to fetch field '${h}'"))
      case _ =>
        Right(value)

    if path.isEmpty then Left(new MemoryException(s"Path to update a Cell is empty"))
    else
      val (h, tail) = (path.head, path.tail)
      get(h)
        .toRight(new MemoryException(s"Cannot find Area with variable '${h}'"))
        .flatMap(c => iterate(tail, c).flatMap(uc => update(h, uc).toRight(new MemoryException(s"Cannot update Area with the updated cell ${uc} at '${h}'"))))

  def tryPop(): Either[Throwable, Area] =
    parent.toRight(new MemoryException(s"Cannot pop a memory space, getting a parent one."))

  override def toString: String =
    val pairs = members.map(it => s"${it._1}: ${it._2}")
    s"[${name}]${pairs.mkString("{", ", ", "}")}"

object Area:

  private val sep: String = "/"

  def apply(name: String): Area =
    new Area(name = name, members = Map.empty[String, Cell], parent = None)

  def apply(name: String, members: Map[String, Cell]): Area =
    new Area(name = name, members = members, parent = None)

  def apply(name: String, parent: Option[Area]): Area =
    new Area(name = name, members = Map.empty[String, Cell], parent = parent)

  /**
   * Returns an *unordered* diff between two memory spaces
   */
  def diff(before: Area, after: Area): Either[Throwable, List[Diff.Change[Path, Cell]]] =

    def iterate(ns: Path, a: Area, b: Area): Either[Throwable, List[Diff.Change[Path, Cell]]] =
      if a.name != b.name then Left(new MemoryException(s"Cannot calculate the diff between unrelated memory spaces '${a.name}' and '${b.name}'"))
      else
        val ns1 = ns.append(a.name)
        val errOrParentDiff = (a.parent, b.parent) match
          case (Some(x), None) =>
            iterate(ns1, x, Area(name = x.name))
          case (None, Some(x)) =>
            iterate(ns1, Area(name = x.name), x)
          case (Some(x), Some(y)) =>
            iterate(ns1, x, y)
          case (None, None) =>
            Right(List.empty[Diff.Change[Path, Cell]])

        errOrParentDiff.map(_ ++ Diff.calc(a.members, b.members).map(it => withPrefix(ns.append(a.name), it)))

    iterate(Path.empty, before, after)

  private[memory] def withPrefix[V](prefix: Path, change: Diff.Change[String, V]): Diff.Change[Path, V] =
    change match
      case Diff.Removed(k, v)    => Diff.Removed(prefix.append(k), v)
      case Diff.Added(k, v)      => Diff.Added(prefix.append(k), v)
      case Diff.Updated(k, b, a) => Diff.Updated(prefix.append(k), b, a)

  given Show[Area] with
    import Cell.{ *, given }

    extension (a: Area)
      def show: String =

        def iterate(d: Int, ms: Area): Seq[String] =
          val parentLines = ms.parent.fold(Seq.empty[String])(p => iterate(d - 1, p))

          val membersCell: Cell = Cell.Struct(ms.members)
          val membersCellLines  = LineOps.split(membersCell.show)

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