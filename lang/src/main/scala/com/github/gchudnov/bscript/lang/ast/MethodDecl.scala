package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }

/**
 * Method Declaration
 */
final case class MethodDecl(retType: Type, name: String, params: Seq[ArgDecl], body: Block, symbol: Symbol, evalType: Type, promoteToType: Option[Type], annotations: Seq[Ann])
    extends Decl:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): MethodDecl = copy(promoteToType = t)

object MethodDecl:
  def apply(retType: Type, name: String, params: Seq[ArgDecl], body: Block): MethodDecl =
    new MethodDecl(
      retType = retType,
      name = name,
      params = params,
      body = body,
      symbol = Symbol.Undefined,
      evalType = Type.Undefined,
      promoteToType = None,
      annotations = Seq.empty[Ann]
    )

  def apply(retType: Type, name: String, params: Seq[ArgDecl], body: Block, symbol: Symbol): MethodDecl =
    new MethodDecl(retType = retType, name = name, params = params, body = body, symbol = symbol, evalType = Type.Undefined, promoteToType = None, annotations = Seq.empty[Ann])

  def apply(retType: Type, name: String, params: Seq[ArgDecl], body: Block, symbol: Symbol, evalType: Type): MethodDecl =
    new MethodDecl(retType = retType, name = name, params = params, body = body, symbol = symbol, evalType = evalType, promoteToType = None, annotations = Seq.empty[Ann])

  // with annotations
  def apply(retType: Type, name: String, params: Seq[ArgDecl], body: Block, annotations: Seq[Ann]): MethodDecl =
    new MethodDecl(
      retType = retType,
      name = name,
      params = params,
      body = body,
      symbol = Symbol.Undefined,
      evalType = Type.Undefined,
      promoteToType = None,
      annotations = annotations
    )

  def apply(retType: Type, name: String, params: Seq[ArgDecl], body: Block, symbol: Symbol, annotations: Seq[Ann]): MethodDecl =
    new MethodDecl(retType = retType, name = name, params = params, body = body, symbol = symbol, evalType = Type.Undefined, promoteToType = None, annotations = annotations)

  def apply(retType: Type, name: String, params: Seq[ArgDecl], body: Block, symbol: Symbol, evalType: Type, annotations: Seq[Ann]): MethodDecl =
    new MethodDecl(retType = retType, name = name, params = params, body = body, symbol = symbol, evalType = evalType, promoteToType = None, annotations = annotations)
