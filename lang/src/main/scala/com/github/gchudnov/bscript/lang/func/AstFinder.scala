package com.github.gchudnov.bscript.lang.func

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.types.*

/**
  * AST Finder
  * 
 * Usage:
 * {{{
 *
 * }}}
  */
trait AstFinder {

  def findAST(ast: AST): Option[AST]

}