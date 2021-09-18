package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Multiplication
 *
 * {{{
 *   a * b
 * }}}
 */
final case class Mul(lhs: Expr, rhs: Expr, evalType: Type, promoteToType: Option[Type]) extends BinOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Mul = copy(promoteToType = t)

object Mul:
  def apply(lhs: Expr, rhs: Expr): Mul =
    new Mul(lhs = lhs, rhs = rhs, evalType = Type.Undefined, promoteToType = None)

  def apply(lhs: Expr, rhs: Expr, evalType: Type): Mul =
    new Mul(lhs = lhs, rhs = rhs, evalType = evalType, promoteToType = None)
