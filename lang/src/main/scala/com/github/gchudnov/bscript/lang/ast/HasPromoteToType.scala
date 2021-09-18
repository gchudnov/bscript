package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Reference to `promoteToType`
 *
 * The type we need to promote current type to before evaluation. If None - the promotion is not needed.
 */
trait HasPromoteToType:
  def promoteToType: Option[Type]
