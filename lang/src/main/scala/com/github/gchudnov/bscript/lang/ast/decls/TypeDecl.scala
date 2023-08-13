package com.github.gchudnov.bscript.lang.ast.decls

import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.ast.*

/**
 * Type Definition for a generic type
 *
 * {{{
 *   T in `def func[T](...)`
 * }}}
 */
final case class TypeDecl(name: String, aType: GenericType) extends Decl

object TypeDecl:
  def apply(name: String): TypeDecl =
    TypeDecl(name = name, aType = GenericType(name))
