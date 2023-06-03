package com.github.gchudnov.bscript.interpreter.memory

/**
 * Path to a cell in a structure
 *
 * {{{
 *   struct A {
 *     int x;
 *     B b;
 *   };
 *   struct B { int y; };
 *
 *   A a;
 *
 *   a.b.y = 2; // here 'a.b.y' is a path
 * }}}
 */
final case class Path(elems: List[String]):
  import Path.*

  def head: String =
    elems.head

  def tail: Path =
    Path(elems.tail)

  def concat(other: Path): Path =
    Path(elems ++ other.elems)

  def prepend(elem: String): Path =
    Path(elem :: elems)

  def append(elem: String): Path =
    Path(elems :+ elem)

  def asString: String = 
    elems.mkString(sep)

  def isEmpty: Boolean =
    elems.isEmpty


object Path:
  private val sep: String   = "."
  private val rxSep: String = "\\."

  def parse(p: String, regex: String = rxSep): Path =
    if p.isEmpty then 
      empty
    else
      Path(p.split(regex).toList)

  def unapply(path: Path): Option[(String, Path)] =
    if path.isEmpty then None
    else Some((path.head, path.tail))

  val empty: Path =
    Path(List.empty[String])

  def make(ps: Iterable[String]): Path =
    Path(ps.toList)
