package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Equality
 *
 * {{{
 *   ==
 * }}}
 */
final case class Equal(lhs: Expr, rhs: Expr, evalType: Type, promoteToType: Option[Type]) extends EqOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Equal = copy(promoteToType = t)

object Equal:
  def apply(lhs: Expr, rhs: Expr): Equal =
    new Equal(lhs = lhs, rhs = rhs, evalType = Type.Undefined, promoteToType = None)

  def apply(lhs: Expr, rhs: Expr, evalType: Type): Equal =
    new Equal(lhs = lhs, rhs = rhs, evalType = evalType, promoteToType = None)
