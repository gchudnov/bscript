package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }

/**
 * Struct Declaration
 */
final case class StructDecl(name: String, tfields: List[TypeDecl], fields: List[VarDecl]) extends Decl

object StructDecl:
  def apply(name: String, fields: List[VarDecl]): StructDecl =
    StructDecl(
      name = name,
      tfields = List.empty[TypeDecl],
      fields = fields
    )
