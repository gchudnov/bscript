package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Logic And
 *
 * {{{
 *   a AND b;
 * }}}
 */
final case class And(lhs: Expr, rhs: Expr, evalType: Type, promoteToType: Option[Type]) extends LogicOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): And = copy(promoteToType = t)

object And:
  def apply(lhs: Expr, rhs: Expr): And =
    new And(lhs = lhs, rhs = rhs, evalType = Type.Undefined, promoteToType = None)

  def apply(lhs: Expr, rhs: Expr, evalType: Type): And =
    new And(lhs = lhs, rhs = rhs, evalType = evalType, promoteToType = None)
