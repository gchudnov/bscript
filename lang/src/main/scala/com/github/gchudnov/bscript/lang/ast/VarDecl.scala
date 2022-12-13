package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }

/**
 * Variable Declaration
 *
 * The AST for int i; would have VARDECL at the root and int and i as children.
 *
 * {{{
 *   int x;
 *   float y = 10;
 * }}}
 */
final case class VarDecl(vType: Type, name: String, expr: Expr) extends Decl