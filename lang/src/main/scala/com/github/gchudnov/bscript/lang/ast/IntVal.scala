package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Int Literal
 *
 * {{{
 *   12
 * }}}
 */
final case class IntVal(value: Int, evalType: Type, promoteToType: Option[Type]) extends ConstVal:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): IntVal = copy(promoteToType = t)

object IntVal:
  def apply(value: Int): IntVal =
    new IntVal(value = value, evalType = Type.Undefined, promoteToType = None)

  def apply(value: Int, evalType: Type): IntVal =
    new IntVal(value = value, evalType = evalType, promoteToType = None)
