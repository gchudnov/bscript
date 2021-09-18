package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Initialization Node
 *
 * Used to init variable to the default value.
 *
 * {{{
 *   int x; // here in AST, Init() is used to initialize the int var to 0.
 * }}}
 */
final case class Init(iType: Type, evalType: Type, promoteToType: Option[Type]) extends Expr:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Init = copy(promoteToType = t)

object Init:
  def apply(): Init =
    new Init(iType = Type.Undefined, evalType = Type.Undefined, promoteToType = None)

  def apply(iType: Type): Init =
    new Init(iType = iType, evalType = Type.Undefined, promoteToType = None)

  def apply(iType: Type, evalType: Type): Init =
    new Init(iType = iType, evalType = evalType, promoteToType = None)
