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
final case class StructVal(sType: Type, value: Map[String, Expr], symbol: Symbol, evalType: Type, promoteToType: Option[Type]) extends ConstVal:

  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): StructVal = copy(promoteToType = t)

object StructVal:

  def apply(sType: Type): StructVal =
    new StructVal(sType = sType, value = Map.empty[String, Expr], symbol = Symbol.Undefined, evalType = Type.Undefined, promoteToType = None)

  def apply(sType: Type, value: Map[String, Expr]): StructVal =
    new StructVal(sType = sType, value = value, symbol = Symbol.Undefined, evalType = Type.Undefined, promoteToType = None)

  def apply(sType: Type, value: Map[String, Expr], symbol: Symbol): StructVal =
    new StructVal(sType = sType, value = value, symbol = symbol, evalType = Type.Undefined, promoteToType = None)

  def apply(sType: Type, value: Map[String, Expr], symbol: Symbol, evalType: Type): StructVal =
    new StructVal(sType = sType, value = value, symbol = symbol, evalType = evalType, promoteToType = None)
