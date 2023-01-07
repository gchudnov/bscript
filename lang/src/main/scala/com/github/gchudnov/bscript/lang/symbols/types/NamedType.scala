package com.github.gchudnov.bscript.lang.symbols.types

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Type that has a name
 */
trait NamedType extends Type:
  def name: String
