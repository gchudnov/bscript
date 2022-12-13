package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Subtract
 *
 * {{{
 *   a - b;
 * }}}
 */
final case class Sub(lhs: Expr, rhs: Expr) extends BinOp