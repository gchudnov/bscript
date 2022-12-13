package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }

/**
 * Field Declaration in a Struct.
 *
 * {{{
 *   struct X {
 *     int x;
 *     float y;
 *   }
 * }}}
 */
final case class FieldDecl(fType: Type, name: String) extends Decl