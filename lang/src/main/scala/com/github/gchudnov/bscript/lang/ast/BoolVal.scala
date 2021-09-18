package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Boolean Literal
 *
 * {{{
 *   true, false
 * }}}
 */
final case class BoolVal(value: Boolean, evalType: Type, promoteToType: Option[Type]) extends ConstVal:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): BoolVal = copy(promoteToType = t)

object BoolVal:
  def apply(value: Boolean): BoolVal =
    new BoolVal(value = value, evalType = Type.Undefined, promoteToType = None)

  def apply(value: Boolean, evalType: Type): BoolVal =
    new BoolVal(value = value, evalType = evalType, promoteToType = None)
