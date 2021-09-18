package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Greater-Than
 *
 * {{{
 *   >
 * }}}
 */
final case class Greater(lhs: Expr, rhs: Expr, evalType: Type, promoteToType: Option[Type]) extends RelOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Greater = copy(promoteToType = t)

object Greater:
  def apply(lhs: Expr, rhs: Expr): Greater =
    new Greater(lhs = lhs, rhs = rhs, evalType = Type.Undefined, promoteToType = None)

  def apply(lhs: Expr, rhs: Expr, evalType: Type): Greater =
    new Greater(lhs = lhs, rhs = rhs, evalType = evalType, promoteToType = None)
