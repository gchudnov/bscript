package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Logic Or
 *
 * {{{
 *   a OR b;
 * }}}
 */
final case class Or(lhs: Expr, rhs: Expr, evalType: Type, promoteToType: Option[Type]) extends LogicOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Or = copy(promoteToType = t)

object Or:
  def apply(lhs: Expr, rhs: Expr): Or =
    new Or(lhs = lhs, rhs = rhs, evalType = Type.Undefined, promoteToType = None)

  def apply(lhs: Expr, rhs: Expr, evalType: Type): Or =
    new Or(lhs = lhs, rhs = rhs, evalType = evalType, promoteToType = None)
