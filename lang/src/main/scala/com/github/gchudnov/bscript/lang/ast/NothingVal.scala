package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

final case class NothingVal(evalType: Type, promoteToType: Option[Type]) extends ConstVal:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): NothingVal = copy(promoteToType = t)

object NothingVal:
  def apply(): NothingVal =
    new NothingVal(evalType = Type.Undefined, promoteToType = None)

  def apply(evalType: Type): NothingVal =
    new NothingVal(evalType = evalType, promoteToType = None)
