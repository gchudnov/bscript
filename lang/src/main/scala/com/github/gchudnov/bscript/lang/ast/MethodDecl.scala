package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type, Ann }

/**
 * Method Declaration
 */
final case class MethodDecl(retType: Type, name: String, params: List[VarDecl], body: Block)
    extends Decl

/*

      def apply(symbol: Symbol, rhsFn: List[List[Tree]] => Option[Term]): DefDef
      def copy(original: Tree)(name: String, paramss: List[ParamClause], tpt: TypeTree, rhs: Option[Term]): DefDef
      def unapply(ddef: DefDef): (String, List[ParamClause], TypeTree, Option[Term])

*/