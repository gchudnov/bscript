package com.github.gchudnov.bscript.lang.symbols.types

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * And / Or Type
 */
trait AndOrType extends Type:
  def left: Type
  def right: Type
