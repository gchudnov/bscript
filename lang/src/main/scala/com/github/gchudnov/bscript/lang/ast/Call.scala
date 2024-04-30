package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }
import scala.collection.immutable.Seq

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
final case class Call(id: Symbol, args: Seq[Expr], evalType: Type, promoteToType: Option[Type]) extends Expr:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Call = copy(promoteToType = t)

object Call:
  def apply(id: Symbol, exprs: Seq[Expr]): Call =
    new Call(id = id, args = exprs, evalType = Type.Undefined, promoteToType = None)

  def apply(id: Symbol, exprs: Seq[Expr], evalType: Type): Call =
    new Call(id = id, args = exprs, evalType = evalType, promoteToType = None)
