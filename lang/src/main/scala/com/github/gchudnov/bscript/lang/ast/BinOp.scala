package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Binary Operation
 *
 * {{{
 *   +, -, *, /
 * }}}
 */
abstract class BinOp extends Expr:
  def lhs: Expr
  def rhs: Expr
