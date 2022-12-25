package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }

/**
 * Struct Declaration
 */
final case class StructDecl(name: String, fields: List[FieldDecl]) extends Decl