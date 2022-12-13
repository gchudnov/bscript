package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Not
 *
 * {{{
 *   ! expr;
 * }}}
 */
final case class Not(expr: Expr) extends UnLogicOp