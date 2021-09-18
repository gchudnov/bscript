package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Division
 *
 * {{{
 *   a / b
 * }}}
 */
final case class Div(lhs: Expr, rhs: Expr, evalType: Type, promoteToType: Option[Type]) extends BinOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Div = copy(promoteToType = t)

object Div:
  def apply(lhs: Expr, rhs: Expr): Div =
    new Div(lhs = lhs, rhs = rhs, evalType = Type.Undefined, promoteToType = None)

  def apply(lhs: Expr, rhs: Expr, evalType: Type): Div =
    new Div(lhs = lhs, rhs = rhs, evalType = evalType, promoteToType = None)
