package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Equality
 *
 * {{{
 *   ==
 * }}}
 */
final case class Equal(lhs: Expr, rhs: Expr) extends EqOp