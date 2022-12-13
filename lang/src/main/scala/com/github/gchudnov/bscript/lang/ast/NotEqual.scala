package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Not Equality
 *
 * {{{
 *   !=
 * }}}
 */
final case class NotEqual(lhs: Expr, rhs: Expr) extends EqOp