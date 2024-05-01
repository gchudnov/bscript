package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Return
 *
 * {{{
 *   return expr;
 * }}}
 */
final case class Return(expr: Expr, evalType: Type, promoteToType: Option[Type]) extends Expr:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Return = copy(promoteToType = t)

object Return:
  def apply(expr: Expr): Return =
    new Return(expr = expr, evalType = Type.Undefined, promoteToType = None)

  def apply(expr: Expr, evalType: Type): Return =
    new Return(expr = expr, evalType = evalType, promoteToType = None)
