package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Decimal Literal
 *
 * {{{
 *   3.3
 * }}}
 */
final case class DecimalVal(value: BigDecimal, evalType: Type, promoteToType: Option[Type]) extends ConstVal:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): DecimalVal = copy(promoteToType = t)

object DecimalVal:
  def apply(value: BigDecimal): DecimalVal =
    new DecimalVal(value = value, evalType = Type.Undefined, promoteToType = None)

  def apply(value: BigDecimal, evalType: Type): DecimalVal =
    new DecimalVal(value = value, evalType = evalType, promoteToType = None)
