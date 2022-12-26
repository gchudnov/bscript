package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }
import com.github.gchudnov.bscript.lang.types.TypeAST

/**
 * Variable Declaration
 *
 * used as well for:
 *   - Argument Declaration
 *   - Field Declaration in a Struct
 *
 * {{{
 *   int x;
 *   float y = 10;
 * }}}
 */
final case class VarDecl(vType: TypeAST, name: String, expr: Expr) extends Decl

/*
ValDef("a", TypeIdent("Int"), Some(Wildcard())))

def unapply(vdef: ValDef): (String, TypeTree, Option[Term])
*/