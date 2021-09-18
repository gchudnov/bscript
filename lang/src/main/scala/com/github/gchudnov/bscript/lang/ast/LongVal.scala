package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Long Literal
 *
 * {{{
 *   12
 * }}}
 */
final case class LongVal(value: Long, evalType: Type, promoteToType: Option[Type]) extends ConstVal:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): LongVal = copy(promoteToType = t)

object LongVal:
  def apply(value: Long): LongVal =
    new LongVal(value = value, evalType = Type.Undefined, promoteToType = None)

  def apply(value: Long, evalType: Type): LongVal =
    new LongVal(value = value, evalType = evalType, promoteToType = None)
