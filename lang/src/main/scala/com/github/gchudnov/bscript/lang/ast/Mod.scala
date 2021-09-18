package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Modulo
 *
 * {{{
 *   a % b
 * }}}
 */
final case class Mod(lhs: Expr, rhs: Expr, evalType: Type, promoteToType: Option[Type]) extends BinOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Mod = copy(promoteToType = t)

object Mod:
  def apply(lhs: Expr, rhs: Expr): Mod =
    new Mod(lhs = lhs, rhs = rhs, evalType = Type.Undefined, promoteToType = None)

  def apply(lhs: Expr, rhs: Expr, evalType: Type): Mod =
    new Mod(lhs = lhs, rhs = rhs, evalType = evalType, promoteToType = None)
