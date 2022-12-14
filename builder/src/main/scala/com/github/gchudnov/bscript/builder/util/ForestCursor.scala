package com.github.gchudnov.bscript.builder.util

final case class ForestCursor[A <: AnyRef](
  forest: Forest[A],
  parent: Option[A],
  counter: Vector[Int],
  level: Int
):
  def push(): ForestCursor[A] =
    ???

  def pop(): ForestCursor[A] =
    ???

object ForestCursor:
  def empty[A <: AnyRef]: ForestCursor[A] =
    ForestCursor(
      forest = Forest.empty[A],
      parent = None,
      counter = Vector.empty[Int],
      level = -1
    )
