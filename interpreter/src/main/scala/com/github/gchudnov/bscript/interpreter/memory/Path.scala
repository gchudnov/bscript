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
final case class Path(value: String):
  import Path.*

  def split: List[String] =
    if value.isEmpty then List.empty[String]
    else value.split(sepRx).toList

  def isEmpty: Boolean =
    value.isEmpty

object Path:
  val sep: String   = "."
  private val sepRx = "\\."

  val empty: Path =
    Path("")

  def make(ps: Seq[String]): Path =
    Path(ps.mkString(sep))
