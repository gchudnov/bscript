package com.github.gchudnov.bscript.lang.ast.decls

import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.ast.*

/**
 * Struct Declaration
 */
final case class StructDecl(name: String, sType: StructType) extends Decl:
  override def fullName: String = 
    s"${name}@${sType.asString}"
