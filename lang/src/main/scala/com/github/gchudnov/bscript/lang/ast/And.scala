package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Logic And
 *
 * {{{
 *   a AND b;
 * }}}
 */
final case class And(lhs: Expr, rhs: Expr) extends LogicOp