package com.github.gchudnov.bscript.lang.func

import com.github.gchudnov.bscript.lang.ast.*

/**
 * AST Finder
 *
 * Usage:
 * {{{
 *   // find a Const node
 *   val finder = new ASTFinder {
 *      override def findAST(ast: AST): Option[AST] =
 *        ast match {
 *          case _: Const => Some(ast)
 *          case _        => None
 *        }
 *    }
 * 
 *    // entry-point for search
 *    val node = finder.foldAST(None, t.ast)
 *    println(node)
 * }}}
 */
trait ASTFinder extends ASTFolder[Option[AST]]:

  /**
   * Find an AST node
   *
   * Override this method to implement a custom search logic
   *
   * @param ast
   *   AST node
   * @return
   *   Some(ast) if found, None otherwise
   */
  def findAST(ast: AST): Option[AST]

  /**
    * Fold an AST
    */
  override def foldAST(a: Option[AST], ast: AST): Option[AST] =
    a match
      case Some(_) =>
        a
      case None =>
        foldOverAST(findAST(ast), ast)
