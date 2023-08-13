package com.github.gchudnov.bscript.lang.ast

/**
 * Initialization Node
 *
 * Used to init variable to the default value (wildcard).
 *
 * {{{
 *   int x; // here in AST, Init() is used to initialize the int var to 0.
 * }}}
 */
final case class Init() extends Expr
