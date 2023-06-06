package com.github.gchudnov.bscript.builder.state

import scala.annotation.tailrec

/**
 * Abstract Tree
 *
 * Used to build scope trees.
 *
 * NOTE: it might be used to build a forst as well
 */
sealed trait Tree[A]:
  /**
   * Get the number of vertices in the tree
   */
  def vertexSize: Int

  /**
   * Checks whether the tree is empty
   *
   * @return
   *   true if the tree is empty and false otherwise
   */
  def isEmpty: Boolean

  /**
   * Checks whether the tree is not empty
   *
   * @return
   *   true if the tree is not empty and false otherwise
   */
  def nonEmpty: Boolean =
    !isEmpty

  /**
   * Gets a parent for the given node
   *
   * @param x
   *   node
   * @return
   *   parent node if exists
   */
  def parentOf(x: A): Option[A]

  /**
   * Checks whether the tree contains the given node
   *
   * @param x
   *   node
   * @return
   *   true if the tree contains the given node and false otherwise
   */
  def contains(x: A): Boolean

  /**
   * Adds a new node to the tree
   *
   * @param x
   *   node
   * @return
   *   new tree
   */
  def add(x: A): Tree[A]

  /**
   * Adds link from node `from` to the node `to` (child -> parent relationship)
   *
   * @param from
   *   node
   * @param parent
   *   parent node
   * @return
   *   new tree
   */
  def link(from: A, parent: A): Tree[A]

  /**
   * Replaces a node
   *
   * @param a
   *   node to replace
   * @param b
   *   new node
   */
  def replace(a: A, b: A): Tree[A]

  /**
   * Returns the complete path to root
   */
  def path(x: A): List[A]

/**
 * A Tree implementation
 *
 * @param vertices
 *   all available nodes
 * @param edges
 *   (child -> parent) links.
 */
final case class BasicTree[A <: AnyRef](vertices: Set[A], edges: Map[A, A]) extends Tree[A]:

  /**
   * Get the number of vertices in the tree
   */
  override def vertexSize: Int =
    vertices.size

  /**
   * Checks whether the tree is empty
   * @return
   *   true if the tree is empty and false otherwise
   */
  override def isEmpty: Boolean =
    vertices.isEmpty

  /**
   * Gets a parent for the given node
   */
  override def parentOf(x: A): Option[A] =
    edges.get(x)

  /**
   * Checks whether the tree contains the given node
   *
   * @param x
   *   node
   * @return
   *   true if the tree contains the given node and false otherwise
   */
  override def contains(x: A): Boolean =
    vertices.contains(x)

  /**
   * Adds a new node to the tree
   */
  override def add(x: A): Tree[A] =
    this.copy(vertices = vertices + x)

  /**
   * Adds link from node `from` to the node `to` (child -> parent relationship)
   */
  override def link(from: A, parent: A): Tree[A] =
    assert(vertices.contains(from) && vertices.contains(from), "Cannot link nodes that are not added to the forest")
    this.copy(edges = edges + (from -> parent))

  /**
   * Replaces a node
   *
   * @param a
   *   node to replace
   * @param b
   *   new node
   */
  override def replace(a: A, b: A): Tree[A] =
    assert(vertices.contains(a), s"Cannot replace ${a} with ${b}: node not found.")

    val newVertices = vertices - a + b
    val newEdges    = edges - a ++ edges.get(a).fold(Map.empty[A, A])(other => Map(b -> other))

    this.copy(vertices = newVertices, edges = newEdges)

  /**
   * Returns the complete path to root
   *
   * @param x
   *   node
   */
  override def path(x: A): List[A] =
    @tailrec
    def iterate(acc: List[A], y: A): List[A] =
      parentOf(y) match
        case Some(p) =>
          iterate(acc :+ p, p)
        case None =>
          acc

    iterate(List(x), x)

object Tree:

  def empty[A <: AnyRef]: Tree[A] =
    from(
      vertices = Set.empty[A],
      edges = Map.empty[A, A],
    )

  def from[A <: AnyRef](vertices: Set[A], edges: Map[A, A]): Tree[A] =
    BasicTree[A](
      vertices = vertices,
      edges = edges,
    )
