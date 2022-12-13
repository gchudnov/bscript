package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Boolean Literal
 *
 * {{{
 *   true, false
 * }}}
 */
final case class BoolVal(value: Boolean) extends ConstVal