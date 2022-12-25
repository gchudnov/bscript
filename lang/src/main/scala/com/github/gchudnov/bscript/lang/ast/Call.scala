package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }

/**
 * Method (Function) Call
 *
 * {{{
 *   f(i);
 * }}}
 *
 * @param id
 *   Symbol of the method to call
 * @param args
 *   A list of arguments to pass
 */
final case class Call(id: Symbol, args: List[Expr]) extends Expr