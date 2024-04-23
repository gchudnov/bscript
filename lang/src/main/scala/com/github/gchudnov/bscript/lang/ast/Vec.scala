package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type
import scala.collection.immutable.Seq

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
final case class Vec(elements: Seq[Expr], elementType: Type, evalType: Type, promoteToType: Option[Type]) extends Expr with HasElementType:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Vec = copy(promoteToType = t)

object Vec:
  def apply(): Vec =
    new Vec(elements = Seq.empty[Expr], elementType = Type.Undefined, evalType = Type.Undefined, promoteToType = None)

  def apply(elements: Seq[Expr]): Vec =
    new Vec(elements = elements, elementType = Type.Undefined, evalType = Type.Undefined, promoteToType = None)

  def apply(elements: Seq[Expr], evalType: Type): Vec =
    new Vec(elements = elements, elementType = Type.Undefined, evalType = evalType, promoteToType = None)
