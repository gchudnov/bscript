package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type, Ann }

/**
 * Method Declaration
 */
final case class MethodDecl(retType: Type, name: String, params: List[VarDecl], body: Block)
    extends Decl

object MethodDecl:
  def apply(retType: Type, name: String, params: List[VarDecl], body: Block): MethodDecl =
    new MethodDecl(
      retType = retType,
      name = name,
      params = params,
      body = body
    )
