package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type
import com.github.gchudnov.bscript.lang.symbols.TypeRef

/**
 * Collection (Vector)
 *
 * {{{
 *   [1, 2, 3, 4, 5]
 * }}}
 *
 * @param elements
 *   Elements of the collection
 */
final case class Vec(elements: Seq[Expr], elementType: Type) extends Expr with HasElementType:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

object Vec:
  def apply(): Vec =
    new Vec(elements = Seq.empty[Expr], elementType = TypeRef.auto)

  def apply(elements: Seq[Expr]): Vec =
    new Vec(elements = elements, elementType = TypeRef.auto)
