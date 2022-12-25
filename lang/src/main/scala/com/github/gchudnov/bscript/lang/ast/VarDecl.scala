package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }

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
final case class VarDecl(vType: Type, name: String, expr: Expr) extends Decl