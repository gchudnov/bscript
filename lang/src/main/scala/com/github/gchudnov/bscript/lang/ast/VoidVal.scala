package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Void Literal
 */
case class VoidVal(evalType: Type, promoteToType: Option[Type]) extends ConstVal:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): VoidVal = copy(promoteToType = t)

object VoidVal:
  def apply(): VoidVal =
    new VoidVal(evalType = Type.Undefined, promoteToType = None)

  def apply(evalType: Type): VoidVal =
    new VoidVal(evalType = evalType, promoteToType = None)
