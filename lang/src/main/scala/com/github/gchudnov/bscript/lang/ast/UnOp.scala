package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Unary Operation
 */
abstract class UnOp extends Expr:
  val expr: Expr
