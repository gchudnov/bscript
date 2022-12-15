package com.github.gchudnov.bscript.builder.state

import scala.annotation.tailrec

/**
 * Abstract Forest
 *
 * Used to build scope trees.
 *
 * @param vertices
 *   all available nodes
 * @param edges
 *   (child -> parent) links.
 */
final case class Forest[A <: AnyRef](vertices: Set[A], edges: Map[A, A]):

  def size: Int =
    vertices.size

  def isEmpty: Boolean =
    vertices.isEmpty

  /**
   * Gets a parent for the given node
   */
  def parentOf(x: A): Option[A] =
    edges.get(x)

  /**
   * Adds a new node to the tree
   */
  def add(x: A): Forest[A] =
    this.copy(vertices = vertices + x)

  /**
   * Adds link from node `from` to the node `to`
   */
  def link(from: A, to: A): Forest[A] =
    assert(vertices.contains(from) && vertices.contains(from), "Cannot link nodes that are not added to the forest")
    this.copy(edges = edges + (from -> to))

  /**
   * Adds a link only if `to` node is specified
   */
  def maybeLink(from: A, to: Option[A]): Forest[A] =
    to.fold(this)(b => link(from, b))

  /**
   * Replaces a node
   */
  def replace(prev: A, next: A): Forest[A] =
    assert(vertices.contains(prev), s"Cannot replace ${prev} with ${next}: node not found.")

    val newVertices = vertices - prev + next
    val newEdges    = edges - prev ++ edges.get(prev).fold(Map.empty[A, A])(other => Map(next -> other))

    this.copy(vertices = newVertices, edges = newEdges)

  /**
   * Returns the complete path to root
   */
  def path(x: A): List[A] =
    @tailrec
    def iterate(acc: List[A], y: A): List[A] =
      parentOf(y) match
        case Some(p) =>
          iterate(acc :+ p, p)
        case None =>
          acc

    iterate(List(x), x)

object Forest:

  def empty[A <: AnyRef]: Forest[A] =
    Forest[A](
      vertices = Set.empty[A],
      edges = Map.empty[A, A]
    )
