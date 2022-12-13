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
final case class If(cond: Expr, then1: Expr, else1: Option[Expr]) extends Expr

object If:
  def apply(cond: Expr, then1: Expr): If =
    new If(cond = cond, then1 = then1, else1 = None)
