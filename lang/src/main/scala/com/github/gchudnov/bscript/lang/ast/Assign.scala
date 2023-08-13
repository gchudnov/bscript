package com.github.gchudnov.bscript.lang.ast
import com.github.gchudnov.bscript.lang.ast.refs.Ref

/**
 * Assigns a value to the variable that already exists.
 *
 * {{{
 *   int x; // defined before
 *
 *   x = 10 + 2; // assignment here
 * }}}
 */
final case class Assign(lhs: Ref, rhs: Expr) extends Expr
