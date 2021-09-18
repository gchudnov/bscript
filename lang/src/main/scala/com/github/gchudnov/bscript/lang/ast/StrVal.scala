package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * String Literal
 *
 * {{{
 *   "a"
 * }}}
 */
final case class StrVal(value: String, evalType: Type, promoteToType: Option[Type]) extends ConstVal:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): StrVal = copy(promoteToType = t)

object StrVal:
  def apply(value: String): StrVal =
    new StrVal(value = value, evalType = Type.Undefined, promoteToType = None)

  def apply(value: String, evalType: Type): StrVal =
    new StrVal(value = value, evalType = evalType, promoteToType = None)
