package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
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
final case class Call(id: Symbol, args: Seq[Expr]) extends Expr:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)
