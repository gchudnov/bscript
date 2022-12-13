package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Assigns a value to the variable that already exists.
 *
 * {{{
 *   int x; // defined before
 *
 *   x = 10 + 2; // assignment here
 * }}}
 */
final case class Assign(id: LValue, expr: Expr) extends Expr