package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Less-Than OR Equal
 *
 * {{{
 *   <=
 * }}}
 */
final case class LessEqual(lhs: Expr, rhs: Expr, evalType: Type, promoteToType: Option[Type]) extends RelOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): LessEqual = copy(promoteToType = t)

object LessEqual:
  def apply(lhs: Expr, rhs: Expr): LessEqual =
    new LessEqual(lhs = lhs, rhs = rhs, evalType = Type.Undefined, promoteToType = None)

  def apply(lhs: Expr, rhs: Expr, evalType: Type): LessEqual =
    new LessEqual(lhs = lhs, rhs = rhs, evalType = evalType, promoteToType = None)
