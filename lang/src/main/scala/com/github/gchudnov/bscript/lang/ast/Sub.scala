package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Subtract
 *
 * {{{
 *   a - b;
 * }}}
 */
final case class Sub(lhs: Expr, rhs: Expr, evalType: Type, promoteToType: Option[Type]) extends BinOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Sub = copy(promoteToType = t)

object Sub:
  def apply(lhs: Expr, rhs: Expr): Sub =
    new Sub(lhs = lhs, rhs = rhs, evalType = Type.Undefined, promoteToType = None)

  def apply(lhs: Expr, rhs: Expr, evalType: Type): Sub =
    new Sub(lhs = lhs, rhs = rhs, evalType = evalType, promoteToType = None)
