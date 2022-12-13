package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Less-Than
 *
 * {{{
 *   <
 * }}}
 */
final case class Less(lhs: Expr, rhs: Expr) extends RelOp