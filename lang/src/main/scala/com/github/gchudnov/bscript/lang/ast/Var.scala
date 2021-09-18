package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }

/**
 * Reference to a variable
 *
 * {{{
 *   x;
 * }}}
 */
final case class Var(symbol: Symbol, evalType: Type, promoteToType: Option[Type]) extends LValue:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Var = copy(promoteToType = t)

object Var:
  def apply(symbol: Symbol): Var =
    new Var(symbol = symbol, evalType = Type.Undefined, promoteToType = None)

  def apply(symbol: Symbol, evalType: Type): Var =
    new Var(symbol = symbol, evalType = evalType, promoteToType = None)
