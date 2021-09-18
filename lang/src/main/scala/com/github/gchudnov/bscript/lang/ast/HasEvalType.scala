package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Reference to an evalType
 *
 * The type of the value computed by the expression
 */
trait HasEvalType:
  def evalType: Type
