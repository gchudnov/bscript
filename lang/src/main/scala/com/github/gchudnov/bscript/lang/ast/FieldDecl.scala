package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }

/**
 * Field Declaration in a Struct.
 *
 * {{{
 *   struct X {
 *     int x;
 *     float y;
 *   }
 * }}}
 */
final case class FieldDecl(fType: Type, name: String, symbol: Symbol, evalType: Type, promoteToType: Option[Type]) extends Decl:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): FieldDecl = copy(promoteToType = t)

object FieldDecl:
  def apply(fType: Type, name: String): FieldDecl =
    new FieldDecl(fType = fType, name = name, symbol = Symbol.Undefined, evalType = Type.Undefined, promoteToType = None)

  def apply(fType: Type, name: String, symbol: Symbol): FieldDecl =
    new FieldDecl(fType = fType, name = name, symbol = symbol, evalType = Type.Undefined, promoteToType = None)

  def apply(fType: Type, name: String, symbol: Symbol, evalType: Type): FieldDecl =
    new FieldDecl(fType = fType, name = name, symbol = symbol, evalType = evalType, promoteToType = None)
