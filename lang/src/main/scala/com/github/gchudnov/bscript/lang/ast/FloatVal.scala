package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Float Literal
 *
 * {{{
 *   12.34f
 * }}}
 */
final case class FloatVal(value: Float, evalType: Type, promoteToType: Option[Type]) extends ConstVal:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): FloatVal = copy(promoteToType = t)

object FloatVal:
  def apply(value: Float): FloatVal =
    new FloatVal(value = value, evalType = Type.Undefined, promoteToType = None)

  def apply(value: Float, evalType: Type): FloatVal =
    new FloatVal(value = value, evalType = evalType, promoteToType = None)
