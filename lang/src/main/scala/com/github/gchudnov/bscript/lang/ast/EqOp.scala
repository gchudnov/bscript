package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Equality Operation
 *
 * {{{
 *   !=, ==
 * }}}
 */
abstract class EqOp extends Expr:
  val lhs: Expr
  val rhs: Expr
