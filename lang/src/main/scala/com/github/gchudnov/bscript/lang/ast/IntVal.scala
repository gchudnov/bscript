package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Int Literal
 *
 * {{{
 *   12
 * }}}
 */
final case class IntVal(value: Int) extends ConstVal