package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Any Constant Variable
 */
abstract class ConstVal extends Expr:
  val evalType: Type
  val promoteToType: Option[Type]
