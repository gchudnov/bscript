package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Division
 *
 * {{{
 *   a / b
 * }}}
 */
final case class Div(lhs: Expr, rhs: Expr) extends BinOp