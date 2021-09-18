package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }

/**
 * Argument Declaration
 */
final case class ArgDecl(aType: Type, name: String, symbol: Symbol, evalType: Type, promoteToType: Option[Type]) extends Decl:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): ArgDecl = copy(promoteToType = t)

object ArgDecl:
  def apply(aType: Type, name: String): ArgDecl =
    new ArgDecl(aType = aType, name = name, symbol = Symbol.Undefined, evalType = Type.Undefined, promoteToType = None)

  def apply(aType: Type, name: String, symbol: Symbol): ArgDecl =
    new ArgDecl(aType = aType, name = name, symbol = symbol, evalType = Type.Undefined, promoteToType = None)

  def apply(aType: Type, name: String, symbol: Symbol, evalType: Type): ArgDecl =
    new ArgDecl(aType = aType, name = name, symbol = symbol, evalType = evalType, promoteToType = None)
