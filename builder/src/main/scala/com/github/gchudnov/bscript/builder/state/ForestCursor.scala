package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.util.Base26

/**
 * Forest Cursor
 *
 * @param forest
 * @param current
 * @param counter
 * @param level
 *   current level
 * @param aFactory
 */
final case class ForestCursor[A <: AnyRef](
  forest: Forest[A],
  current: Option[A],
  counter: Vector[Int],
  level: Int,
  aFactory: (String) => A,
):
  import ForestCursor.*

  /**
   * Push a new element to the forest
   *
   * @return
   *   A new forest cursor
   */
  def push(): ForestCursor[A] =
    val c    = updateCounter(this.level)
    val name = c.take(level + 1).map(k => Base26.encode(k)).mkString(sep) // new name is a concatenation of all counters
    val a    = aFactory(name)
    val fa   = forest.add(a)
    this.copy(
      forest = current.fold(fa)(c => fa.linkParent(a, c)),
      current = Some(a),
      counter = c,
      level = this.level + 1,
    )

  /**
   * Pop the current element from the forest
   *
   * @return
   *   A new forest cursor
   */
  def pop(): ForestCursor[A] =
    this.copy(
      current = this.current.flatMap(c => this.forest.parentOf(c)),
      level = this.level - 1,
    )

  private def updateCounter(n: Int): Vector[Int] =
    assert(counter.size >= n, s"counter = ${counter.toList}, n = ${n}: trying to increment counter out of bounds, this ia bug in code")
    if this.counter.size == n then counter.appended(0) else counter.updated(n, counter(n) + 1)

object ForestCursor:

  private lazy val sep = "."

  /**
   * Create an empty forest
   *
   * @param aFactory
   *   Takes a name and returns an instance of A, al element of the forest
   * @return
   *   A new forest cursor
   */
  def empty[A <: AnyRef](aFactory: (String) => A): ForestCursor[A] =
    ForestCursor(
      forest = Forest.empty[A],
      current = None,
      counter = Vector.empty[Int],
      level = 0,
      aFactory = aFactory,
    )
