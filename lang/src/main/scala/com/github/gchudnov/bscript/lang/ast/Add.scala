package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Addition
 *
 * {{{
 *   a + b;
 * }}}
 */
final case class Add(lhs: Expr, rhs: Expr, evalType: Type, promoteToType: Option[Type]) extends BinOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Add = copy(promoteToType = t)

object Add:
  def apply(lhs: Expr, rhs: Expr): Add =
    new Add(lhs = lhs, rhs = rhs, evalType = Type.Undefined, promoteToType = None)

  def apply(lhs: Expr, rhs: Expr, evalType: Type): Add =
    new Add(lhs = lhs, rhs = rhs, evalType = evalType, promoteToType = None)
