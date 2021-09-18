package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Not
 *
 * {{{
 *   ! expr;
 * }}}
 */
final case class Not(expr: Expr, evalType: Type, promoteToType: Option[Type]) extends UnLogicOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Not = copy(promoteToType = t)

object Not:
  def apply(expr: Expr): Not =
    new Not(expr = expr, evalType = Type.Undefined, promoteToType = None)

  def apply(expr: Expr, evalType: Type): Not =
    new Not(expr = expr, evalType = evalType, promoteToType = None)
