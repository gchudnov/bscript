package com.github.gchudnov.bscript.lang.ast.decls

import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.ast.*

/**
 * Type Definition for a built-in type
 *
 * {{{
 *   Int
 *   Float
 *   String
 * }}}
 */
final case class BuiltInDecl(name: String, aType: BuiltInType) extends Decl

object BuiltInDecl:
  def apply(name: String): BuiltInDecl =
    BuiltInDecl(name = name, aType = BuiltInType(name))
