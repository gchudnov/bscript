package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.lang.symbols.Scope
import com.github.gchudnov.bscript.lang.util.Show
import com.github.gchudnov.bscript.builder.util.EqWrap

/**
 * Scope Tree
 *
 * @param vertices
 *   all available Scopes
 * @param edges
 *   child -> parent links. Allows to traverse the tree from leaves up to the root.
 */
final case class ScopeTree(vertices: Set[EqWrap[Scope]], edges: Map[EqWrap[Scope], Scope]):

  /**
   * Gets a parent for the given scope
   */
  def parent(n: Scope): Option[Scope] =
    edges.get(EqWrap(n))

  /**
   * Adds a new scope to the tree without linking (e.g. to create a root)
   */
  def add(n: Scope): ScopeTree =
    this.copy(vertices = vertices + EqWrap(n))

  /**
   * Adds a new scope and links it to a parent
   */
  def link(n: Scope, parent: Scope): ScopeTree =
    val newVertices = vertices + EqWrap(n) + EqWrap(parent)
    val newEdges    = edges + (EqWrap(n) -> parent)
    this.copy(vertices = newVertices, edges = newEdges)

  /**
   * Replaces a Scope. If the replaced scope is linked to the parent, that link will be updated
   */
  def replace(from: Scope, to: Scope): ScopeTree =
    assert(vertices.contains(EqWrap(from)), s"Scope ${from} cannot be not found in vertices to replace it with ${to}")
    val newVertices = vertices - EqWrap(from) + EqWrap(to)
    val newEdges    = edges - EqWrap(from) ++ edges.get(EqWrap(from)).fold(Map.empty[EqWrap[Scope], Scope])(parent => Map(EqWrap(to) -> parent))
    this.copy(vertices = newVertices, edges = newEdges)

  /**
   * Get the scope by its name
   */
  def get(name: String): Option[Scope] =
    vertices.find(_.value.name == name).map(_.value)

  /**
   * Get the root only if one node is present
   */
  def root: Option[Scope] =
    if vertices.size == 1 then vertices.headOption.map(_.value)
    else None

object ScopeTree:
  val empty: ScopeTree =
    ScopeTree(
      vertices = Set.empty[EqWrap[Scope]],
      edges = Map.empty[EqWrap[Scope], Scope]
    )

  implicit val scopeTreeShow: Show[ScopeTree] = new Show[ScopeTree]:

    override def show(a: ScopeTree): String =
      val sb = new StringBuilder
      sb.append("{\n")

      val vs = listStr(
        a.vertices.toList
          .map(_.value.name)
          .sorted
          .map(it => quote(it))
      )

      val es = listStr(
        a.edges.toList
          .sortBy(kv => s"${kv._1.value.name};${kv._2.name}")
          .map { case (c, p) =>
            listStr(List(quote(c.value.name), quote(p.name)))
          }
      )

      sb.append(s"""${spaced(1)}"vertices": ${vs},\n""")
      sb.append(s"""${spaced(1)}"edges": ${es}\n""")

      sb.append("}")
      sb.toString()

    private def spaced(depth: Int): String =
      "  ".repeat(depth)

    private def quote(s: String): String =
      s"\"${s}\""

    private def listStr(xs: List[String]): String =
      xs.mkString("[", ",", "]")
