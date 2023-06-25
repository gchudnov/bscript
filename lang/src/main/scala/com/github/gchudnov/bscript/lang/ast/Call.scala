package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.{ Symbol }

/**
 * Method (Function) Call
 *
 * {{{
 *   f(i);
 * }}}
 *
 * @param id
 *   function to call
 * @param args
 *   A list of arguments to pass
 */
final case class Call(id: Ref, args: List[Expr]) extends Expr
