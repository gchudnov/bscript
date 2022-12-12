package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Double Literal
 *
 * {{{
 *   12.34
 * }}}
 */
final case class DoubleVal(value: Double) extends ConstVal:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)
