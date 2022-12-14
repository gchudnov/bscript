package com.github.gchudnov.bscript.builder.util

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
final case class Forest[A <: AnyRef](vertices: Set[Ptr[A]], edges: Map[Ptr[A], A]):

  def size: Int =
    vertices.size

  def isEmpty: Boolean =
    vertices.isEmpty

  /**
   * Gets a parent for the given node
   */
  def parentOf(x: A): Option[A] =
    edges.get(Ptr(x))

  /**
   * Adds a new node to the tree
   */
  def add(x: A): Forest[A] =
    this.copy(vertices = vertices + Ptr(x))

  /**
   * Adds link from node `from` to the node `to`
   */
  def link(from: A, to: A): Forest[A] =
    assert(vertices.contains(Ptr(from)) && vertices.contains(Ptr(from)), "Cannot link nodes that are not added to the forest")

    this.copy(edges = edges + (Ptr(from) -> to))

  /**
   * Replaces a node
   */
  def replace(prev: A, next: A): Forest[A] =
    assert(vertices.contains(Ptr(prev)), s"Cannot replace ${prev} with ${next}: node not found.")

    val newVertices = vertices - Ptr(prev) + Ptr(next)
    val newEdges    = edges - Ptr(prev) ++ edges.get(Ptr(prev)).fold(Map.empty[Ptr[A], A])(other => Map(Ptr(next) -> other))

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
      vertices = Set.empty[Ptr[A]],
      edges = Map.empty[Ptr[A], A]
    )
