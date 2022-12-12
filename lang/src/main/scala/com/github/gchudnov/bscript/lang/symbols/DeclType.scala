package com.github.gchudnov.bscript.lang.symbols

import com.github.gchudnov.bscript.lang.ast.Expr

/**
 * Allows to reference types of other variables and expressions.
 *
 * example #1:
 * {{{
 *   int x = 10;
 *   decltype(x) y = 20;  // DeclType(Var(SymbolRef("x")))
 * }}}
 *
 * example #2:
 * {{{
 *   int x = 10;
 *   long y = 20L;
 *
 *   decltype(x + y) z = 30; // DeclType(Add(Var(SymbolRef("x")), Var(SymbolRef("y"))))
 * }}}
 */
final case class DeclType(expr: Expr) extends Type:
  override def name: String = 
    //expr.evalType.name
    ???
