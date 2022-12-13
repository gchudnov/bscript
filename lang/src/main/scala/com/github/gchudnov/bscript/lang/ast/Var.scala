package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }

/**
 * Reference to a variable
 *
 * {{{
 *   x;
 * }}}
 */
final case class Var(symbol: Symbol) extends LValue