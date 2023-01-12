package com.github.gchudnov.bscript.lang.ast.decls

import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.ast.*

/**
 * Method Declaration
 */
final case class MethodDecl(name: String, mType: MethodType, body: Block) extends Decl
