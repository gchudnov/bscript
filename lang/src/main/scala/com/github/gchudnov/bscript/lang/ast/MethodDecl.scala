package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }

/**
 * Method Declaration
 */
final case class MethodDecl(retType: Type, name: String, params: Seq[ArgDecl], body: Block, annotations: Seq[Ann])
    extends Decl:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

object MethodDecl:
  def apply(retType: Type, name: String, params: Seq[ArgDecl], body: Block): MethodDecl =
    new MethodDecl(
      retType = retType,
      name = name,
      params = params,
      body = body,
      annotations = Seq.empty[Ann]
    )
