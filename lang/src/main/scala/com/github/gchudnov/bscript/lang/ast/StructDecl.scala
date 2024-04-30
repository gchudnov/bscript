package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }
import scala.collection.immutable.Seq

/**
 * Struct Declaration
 */
final case class StructDecl(name: String, fields: Seq[FieldDecl], symbol: Symbol, evalType: Type, promoteToType: Option[Type]) extends Decl:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): StructDecl = copy(promoteToType = t)

object StructDecl:
  def apply(name: String, fields: Seq[FieldDecl]): StructDecl =
    new StructDecl(name = name, fields = fields, symbol = Symbol.Undefined, evalType = Type.Undefined, promoteToType = None)

  def apply(name: String, fields: Seq[FieldDecl], symbol: Symbol): StructDecl =
    new StructDecl(name = name, fields = fields, symbol = symbol, evalType = Type.Undefined, promoteToType = None)

  def apply(name: String, fields: Seq[FieldDecl], symbol: Symbol, evalType: Type): StructDecl =
    new StructDecl(name = name, fields = fields, symbol = symbol, evalType = evalType, promoteToType = None)
