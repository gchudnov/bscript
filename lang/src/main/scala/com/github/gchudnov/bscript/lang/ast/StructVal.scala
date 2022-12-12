package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }

/**
 * Struct Initializer
 *
 * {{{
 *   struct A {
 *     a: int
 *     b: string
 *   };
 *
 *   A = { a: 1, b: "hello" };
 * }}}
 */
final case class StructVal(sType: Type, value: Map[String, Expr]) extends ConstVal:

  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

object StructVal:

  def apply(sType: Type): StructVal =
    new StructVal(sType = sType, value = Map.empty[String, Expr])
