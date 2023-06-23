package com.github.gchudnov.bscript.lang.ast.types

import com.github.gchudnov.bscript.lang.ast.AST

/**
 * A Type in the AST
 */
abstract class TypeAST extends AST:
  def asString: String
