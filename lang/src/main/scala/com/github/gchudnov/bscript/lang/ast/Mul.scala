package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Multiplication
 *
 * {{{
 *   a * b
 * }}}
 */
final case class Mul(lhs: Expr, rhs: Expr) extends BinOp