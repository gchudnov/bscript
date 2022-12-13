package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Greater-Than OR Equal
 *
 * {{{
 *   >=
 * }}}
 */
final case class GreaterEqual(lhs: Expr, rhs: Expr) extends RelOp