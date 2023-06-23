package com.github.gchudnov.bscript.lang.ast.decls

import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.ast.*

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
final case class VarDecl(name: String, vType: TypeAST, expr: Expr) extends Decl:
  override def fullName: String =
    s"${name}@var"



object VarDecl:
  def apply(name: String, vType: TypeAST): VarDecl =
    VarDecl(name = name, vType = vType, expr = Init())

/*
ValDef("a", TypeIdent("Int"), Some(Wildcard())))

def unapply(vdef: ValDef): (String, TypeTree, Option[Term])
 */
