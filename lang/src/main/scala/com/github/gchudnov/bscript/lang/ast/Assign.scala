package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Assigns a value to the variable that already exists.
 *
 * {{{
 *   int x; // defined before
 *
 *   x = 10 + 2; // assignment here
 * }}}
 */
final case class Assign(id: LValue, expr: Expr, evalType: Type, promoteToType: Option[Type]) extends Expr:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Assign = copy(promoteToType = t)

object Assign:

  def apply(id: LValue, expr: Expr): Assign =
    new Assign(id = id, expr = expr, evalType = Type.Undefined, promoteToType = None)

  def apply(id: LValue, expr: Expr, evalType: Type): Assign =
    new Assign(id = id, expr = expr, evalType = evalType, promoteToType = None)
