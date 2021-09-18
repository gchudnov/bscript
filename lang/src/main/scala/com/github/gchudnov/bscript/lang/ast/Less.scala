package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Less-Than
 *
 * {{{
 *   <
 * }}}
 */
final case class Less(lhs: Expr, rhs: Expr, evalType: Type, promoteToType: Option[Type]) extends RelOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Less = copy(promoteToType = t)

object Less:
  def apply(lhs: Expr, rhs: Expr): Less =
    new Less(lhs = lhs, rhs = rhs, evalType = Type.Undefined, promoteToType = None)

  def apply(lhs: Expr, rhs: Expr, evalType: Type): Less =
    new Less(lhs = lhs, rhs = rhs, evalType = evalType, promoteToType = None)
