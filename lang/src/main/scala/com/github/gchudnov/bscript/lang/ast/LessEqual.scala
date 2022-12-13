package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Less-Than OR Equal
 *
 * {{{
 *   <=
 * }}}
 */
final case class LessEqual(lhs: Expr, rhs: Expr) extends RelOp