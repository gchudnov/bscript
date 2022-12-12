package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Logic Or
 *
 * {{{
 *   a OR b;
 * }}}
 */
final case class Or(lhs: Expr, rhs: Expr) extends LogicOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)
