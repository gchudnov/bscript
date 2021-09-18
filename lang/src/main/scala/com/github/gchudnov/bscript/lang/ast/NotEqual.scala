package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Not Equality
 *
 * {{{
 *   !=
 * }}}
 */
final case class NotEqual(lhs: Expr, rhs: Expr, evalType: Type, promoteToType: Option[Type]) extends EqOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): NotEqual = copy(promoteToType = t)

object NotEqual:
  def apply(lhs: Expr, rhs: Expr): NotEqual =
    new NotEqual(lhs = lhs, rhs = rhs, evalType = Type.Undefined, promoteToType = None)

  def apply(lhs: Expr, rhs: Expr, evalType: Type): NotEqual =
    new NotEqual(lhs = lhs, rhs = rhs, evalType = evalType, promoteToType = None)
