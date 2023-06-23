package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.util.Base26
import com.github.gchudnov.bscript.builder.util.Tree

/**
 * Cursors that tracks the current position in the tree
 *
 * {{{
 *      b0
 *     /  \
 *    b1  b2
 *         \
 *          b3 <--- `.at`
 * }}}
 */
sealed trait TreeCursor[A]:
  /**
   * Expand the tree by adding a new element
   *
   * @return
   *   A new tree cursor
   */
  def push(): TreeCursor[A]

  /**
   * Backoff the current element, so that the cursor points to the parent
   *
   * NOTE: when we backoff, the tree is preserved
   *
   * @return
   *   A new tree cursor
   */
  def pop(): TreeCursor[A]

  /**
   * Get the current value the cursor points to
   *
   * @return
   *   the current value
   */
  def at: Option[A]

  /**
   * Get the tree
   *
   * @return
   *   the tree
   */
  def tree: Tree[A]

/**
 * Tree Cursor
 *
 * @param tree
 *   the tree
 * @param current
 *   the current node the cursor points to
 * @param counter
 *   the vector that contains the number of children at each level
 * @param level
 *   current level the cursor points to
 * @param aFactory
 */
final case class BasicTreeCursor[A <: AnyRef](
  tree: Tree[A],
  current: Option[A],
  counter: Vector[Int],
  level: Int,
  aFactory: (String) => A,
) extends TreeCursor[A]:
  import BasicTreeCursor.*

  /**
   * Push a new element to the tree
   *
   * @return
   *   A new tree cursor
   */
  override def push(): TreeCursor[A] =
    val cs   = incCounter(this.counter, this.level)
    val name = makeName(cs, this.level)
    val a    = aFactory(name)
    val t2   = current.fold(tree.add(a))(c => tree.add(a).link(a, c))
    this.copy(
      tree = t2,
      current = Some(a),
      counter = cs,
      level = this.level + 1,
    )

  /**
   * Pop the current element from the tree
   *
   * @return
   *   A new tree cursor
   */
  override def pop(): TreeCursor[A] =
    this.copy(
      current = this.current.flatMap(c => this.tree.parentOf(c)),
      level = this.level - 1,
    )

  /**
   * Get the current value the cursor points to
   *
   * @return
   *   the current value
   */
  override def at: Option[A] =
    this.current

object BasicTreeCursor:
  private val sep = "."

  /**
   * Make a name to use for a node
   *
   * @param cs
   *   counter of nodes per level
   * @param n
   *   level
   */
  private def makeName(cs: Vector[Int], n: Int): String =
    cs.take(n + 1).map(k => Base26.encode(k)).mkString(sep)

  /**
   * Increment the counter at the given level
   *
   * @param cs
   *   Counter of nodes per level
   * @param n
   *   Level
   * @return
   *   A new counter
   */
  private def incCounter(cs: Vector[Int], n: Int): Vector[Int] =
    require(cs.size >= n, s"counter = ${cs.toList}, n = ${n}: trying to increment counter out of bounds, this ia bug in code")
    if cs.size == n then cs.appended(0) else cs.updated(n, cs(n) + 1)

object TreeCursor:
  /**
   * Create an empty tree
   *
   * @param aFactory
   *   Takes a name and returns an instance of A, al element of the tree
   * @return
   *   A new tree cursor
   */
  def empty[A <: AnyRef](aFactory: (String) => A): TreeCursor[A] =
    BasicTreeCursor(
      tree = Tree.empty[A],
      current = None,
      counter = Vector.empty[Int],
      level = 0,
      aFactory = aFactory,
    )
