package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.lit.*

/**
 * KeyValue
 *
 * Can be used for: Named Argument to a function or to initialize a map
 *
 * {{{
 *   void myFunc(x: i8, s: str) { ... }
 *
 *   myFunc(x = 10, s = "abc")
 * }}}
 */
final case class KeyValue(key: ConstLit, value: Expr) extends Expr
