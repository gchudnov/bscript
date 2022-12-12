package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Logic Operation
 *
 * {{{
 *   AND, OR
 * }}}
 */
abstract class LogicOp extends Expr:
  def lhs: Expr
  def rhs: Expr
