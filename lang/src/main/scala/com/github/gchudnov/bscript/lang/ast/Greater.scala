package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Greater-Than
 *
 * {{{
 *   >
 * }}}
 */
final case class Greater(lhs: Expr, rhs: Expr) extends RelOp