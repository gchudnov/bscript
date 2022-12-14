package com.github.gchudnov.bscript.builder.util

final case class ForestCursor[A <: AnyRef](
  forest: Forest[A],
  current: Option[A],
  counter: Vector[Int],
  level: Int,
  aFactory: (String) => A
):
  def push(): ForestCursor[A] =
    val a = aFactory("REPLACE-ME")
    this.copy(
      forest = forest.add(a),
      current = Some(a),
      counter = incCounter(this.level + 1),
      level = this.level + 1
    )

  def pop(): ForestCursor[A] =
    ???

  private def incCounter(n: Int): Vector[Int] =
    assert(counter.size <= n, "the counter increment is too steep, this is a bug in code")
    if this.counter.size == n then counter.appended(0) else counter.updated(n, counter(n) + 1)

  private def setLink(): Forest[A] =
    ???

object ForestCursor:
  def empty[A <: AnyRef](aFactory: (String) => A): ForestCursor[A] =
    ForestCursor(
      forest = Forest.empty[A],
      current = None,
      counter = Vector.empty[Int],
      level = -1,
      aFactory = aFactory
    )
