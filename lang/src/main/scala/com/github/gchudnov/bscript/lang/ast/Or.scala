package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Logic Or
 *
 * {{{
 *   a OR b;
 * }}}
 */
final case class Or(lhs: Expr, rhs: Expr) extends LogicOp