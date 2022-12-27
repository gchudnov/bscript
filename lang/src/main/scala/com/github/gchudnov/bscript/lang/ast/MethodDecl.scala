package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.{ Ann, Symbol, Type }
import com.github.gchudnov.bscript.lang.ast.types.TypeAST

/**
 * Method Declaration
 */
final case class MethodDecl(name: String, tparams: List[TypeDecl], params: List[VarDecl], retType: TypeAST, body: Block) extends Decl

object MethodDecl:
  def apply(name: String, params: List[VarDecl], retType: TypeAST, body: Block): MethodDecl =
    MethodDecl(
      name = name,
      tparams = List.empty[TypeDecl],
      params = params,
      retType = retType,
      body = body
    )

/*

      def apply(symbol: Symbol, rhsFn: List[List[Tree]] => Option[Term]): DefDef
      def copy(original: Tree)(name: String, paramss: List[ParamClause], tpt: TypeTree, rhs: Option[Term]): DefDef
      def unapply(ddef: DefDef): (String, List[ParamClause], TypeTree, Option[Term])

 */
