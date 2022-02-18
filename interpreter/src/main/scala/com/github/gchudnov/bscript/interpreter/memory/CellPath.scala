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
final case class CellPath(value: String):
  import CellPath.*

  def split: List[String] =
    if value.isEmpty then List.empty[String]
    else value.split(sepRx).toList

object CellPath:
  val sep: String   = "."
  private val sepRx = "\\."

  def make(ps: Seq[String]): CellPath =
    CellPath(ps.mkString(sep))
