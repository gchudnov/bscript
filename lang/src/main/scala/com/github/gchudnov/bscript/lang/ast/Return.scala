package com.github.gchudnov.bscript.lang.ast

/**
 * Return Statement
 *
 * Used to return a value from a function.
 *
 * {{{
 *   return 10;
 * }}}
 */
final case class Return(expr: Expr) extends Expr
