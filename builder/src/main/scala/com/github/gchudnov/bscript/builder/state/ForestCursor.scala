package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.util.Base26

/**
  * Forest Cursor
  *
  * @param forest
  * @param current
  * @param counter
  * @param level
  * @param aFactory
  */
final case class ForestCursor[A <: AnyRef](
  forest: Forest[A],
  current: Option[A],
  counter: Vector[Int],
  level: Int,
  aFactory: (String) => A
):
  def push(): ForestCursor[A] =
    val c    = updateCounter(this.level)
    val name = c.take(level + 1).map(k => Base26.encode(k)).mkString(".")
    val a    = aFactory(name)
    this.copy(
      forest = forest.add(a).maybeLink(a, current),
      current = Some(a),
      counter = c,
      level = this.level + 1
    )

  def pop(): ForestCursor[A] =
    this.copy(
      current = this.current.flatMap(c => this.forest.parentOf(c)),
      level = this.level - 1
    )

  private def updateCounter(n: Int): Vector[Int] =
    assert(counter.size >= n, s"counter = ${counter.toList}, n = ${n}: trying to increment counter out of bounds, this ia bug in code")
    if this.counter.size == n then counter.appended(0) else counter.updated(n, counter(n) + 1)

object ForestCursor:
  def empty[A <: AnyRef](aFactory: (String) => A): ForestCursor[A] =
    ForestCursor(
      forest = Forest.empty[A],
      current = None,
      counter = Vector.empty[Int],
      level = 0,
      aFactory = aFactory
    )
