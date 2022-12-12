package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Not Equality
 *
 * {{{
 *   !=
 * }}}
 */
final case class NotEqual(lhs: Expr, rhs: Expr) extends EqOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)
