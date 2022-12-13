package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Modulo
 *
 * {{{
 *   a % b
 * }}}
 */
final case class Mod(lhs: Expr, rhs: Expr) extends BinOp