package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Greater-Than OR Equal
 *
 * {{{
 *   >=
 * }}}
 */
final case class GreaterEqual(lhs: Expr, rhs: Expr, evalType: Type, promoteToType: Option[Type]) extends RelOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): GreaterEqual = copy(promoteToType = t)

object GreaterEqual:
  def apply(lhs: Expr, rhs: Expr): GreaterEqual =
    new GreaterEqual(lhs = lhs, rhs = rhs, evalType = Type.Undefined, promoteToType = None)

  def apply(lhs: Expr, rhs: Expr, evalType: Type): GreaterEqual =
    new GreaterEqual(lhs = lhs, rhs = rhs, evalType = evalType, promoteToType = None)
