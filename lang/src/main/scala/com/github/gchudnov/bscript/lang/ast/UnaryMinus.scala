package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Unary Minus
 *
 * {{{
 *   -10
 * }}}
 */
final case class UnaryMinus(expr: Expr) extends UnOp