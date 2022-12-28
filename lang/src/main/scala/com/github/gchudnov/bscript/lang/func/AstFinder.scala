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
trait AstFinder extends AstFolder[Option[AST]]:

  def findAST(ast: AST): Option[AST]

  override def foldAST(a: Option[AST], ast: AST): Option[AST] =
    a match
      case Some(_) =>
        a
      case None =>
        findAST(ast)
