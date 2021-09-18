package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Used for collections
 */
trait HasElementType:
  def elementType: Type
