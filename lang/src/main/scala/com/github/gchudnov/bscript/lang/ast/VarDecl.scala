package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }

/**
 * Variable Declaration
 *
 * The AST for int i; would have VARDECL at the root and int and i as children.
 *
 * {{{
 *   int x;
 *   float y = 10;
 * }}}
 */
final case class VarDecl(vType: Type, name: String, expr: Expr, symbol: Symbol, evalType: Type, promoteToType: Option[Type]) extends Decl:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): VarDecl = copy(promoteToType = t)

object VarDecl:
  def apply(vType: Type, name: String, expr: Expr): VarDecl =
    new VarDecl(vType = vType, name = name, expr = expr, symbol = Symbol.Undefined, evalType = Type.Undefined, promoteToType = None)

  def apply(vType: Type, name: String, expr: Expr, symbol: Symbol): VarDecl =
    new VarDecl(vType = vType, name = name, expr = expr, symbol = symbol, evalType = Type.Undefined, promoteToType = None)

  def apply(vType: Type, name: String, expr: Expr, symbol: Symbol, evalType: Type): VarDecl =
    new VarDecl(vType = vType, name = name, expr = expr, symbol = symbol, evalType = evalType, promoteToType = None)
