package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type
import com.github.gchudnov.bscript.lang.symbols.TypeRef

/**
 * Initialization Node
 *
 * Used to init variable to the default value.
 *
 * {{{
 *   int x; // here in AST, Init() is used to initialize the int var to 0.
 * }}}
 */
final case class Init(iType: Type) extends Expr:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

object Init:
  def apply(): Init =
    new Init(iType = TypeRef.auto)
