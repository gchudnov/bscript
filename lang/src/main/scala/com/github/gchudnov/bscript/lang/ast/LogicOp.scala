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
  val lhs: Expr
  val rhs: Expr
  val evalType: Type
  val promoteToType: Option[Type]
