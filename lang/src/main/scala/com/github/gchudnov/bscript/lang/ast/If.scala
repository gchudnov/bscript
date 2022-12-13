package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * If Condition
 *
 * {{{
 *   if(cond) then { ... } else { ... }
 *   if(cond) then { ... }
 *
 *   int x = if(conf) then { 1+2; } else { 2*5; }
 * }}}
 */
final case class If(cond: Expr, then1: Expr, else1: Expr) extends Expr
