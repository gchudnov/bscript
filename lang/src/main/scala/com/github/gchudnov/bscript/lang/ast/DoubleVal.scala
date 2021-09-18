package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Double Literal
 *
 * {{{
 *   12.34
 * }}}
 */
final case class DoubleVal(value: Double, evalType: Type, promoteToType: Option[Type]) extends ConstVal:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): DoubleVal = copy(promoteToType = t)

object DoubleVal:
  def apply(value: Double): DoubleVal =
    new DoubleVal(value = value, evalType = Type.Undefined, promoteToType = None)

  def apply(value: Double, evalType: Type): DoubleVal =
    new DoubleVal(value = value, evalType = evalType, promoteToType = None)
