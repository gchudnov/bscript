package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Unary Minus
 *
 * {{{
 *   -10
 * }}}
 */
final case class UnaryMinus(expr: Expr, evalType: Type, promoteToType: Option[Type]) extends UnOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): UnaryMinus = copy(promoteToType = t)

object UnaryMinus:
  def apply(expr: Expr): UnaryMinus =
    new UnaryMinus(expr = expr, evalType = Type.Undefined, promoteToType = None)

  def apply(expr: Expr, evalType: Type): UnaryMinus =
    new UnaryMinus(expr = expr, evalType = evalType, promoteToType = None)
