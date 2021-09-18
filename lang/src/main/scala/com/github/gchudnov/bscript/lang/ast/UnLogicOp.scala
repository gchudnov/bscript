package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Unary Logic Operation
 */
abstract class UnLogicOp extends Expr:
  val expr: Expr
  val evalType: Type
  val promoteToType: Option[Type]
