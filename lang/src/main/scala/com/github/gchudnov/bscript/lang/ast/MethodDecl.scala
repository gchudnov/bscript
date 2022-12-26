package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.{ Ann, Symbol, Type }
import com.github.gchudnov.bscript.lang.types.TypeAST

/**
 * Method Declaration
 */
final case class MethodDecl(retType: TypeAST, name: String, tparams: List[TypeDecl], params: List[VarDecl], body: Block) extends Decl

object MethodDecl:
  def apply(retType: TypeAST, name: String, params: List[VarDecl], body: Block): MethodDecl =
    MethodDecl(
      retType = retType,
      name = name,
      tparams = List.empty[TypeDecl],
      params = params,
      body = body
    )

/*

      def apply(symbol: Symbol, rhsFn: List[List[Tree]] => Option[Term]): DefDef
      def copy(original: Tree)(name: String, paramss: List[ParamClause], tpt: TypeTree, rhs: Option[Term]): DefDef
      def unapply(ddef: DefDef): (String, List[ParamClause], TypeTree, Option[Term])

 */
