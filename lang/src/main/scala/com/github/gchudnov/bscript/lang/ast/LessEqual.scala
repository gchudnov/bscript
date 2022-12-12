package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Less-Than OR Equal
 *
 * {{{
 *   <=
 * }}}
 */
final case class LessEqual(lhs: Expr, rhs: Expr) extends RelOp:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)
