package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.decls.TypeDecl
/**
 * Annotation attached to some Expr
 *
 * Annotation is a function `f` that called with the provided `expr` and additional parameters:
 *
 * {{{
 *   @f(...params)
 *   expr
 * }}}
 *
 * is equivalent to:
 *
 * {{{
 *   f(expr, ...params)
 * }}}
 *
 * that produces:
 *
 * {{{
 *   expr1
 * }}}
 *
 * And replaces the original `expr` during compilation.
 *
 * The annotation is evaluated on the compile time.
 */
final case class Annotated(expr: Expr, id: Ref, tparams: List[TypeDecl], params: List[Expr]) extends Expr

object Annotated:

  def apply(expr: Expr, id: Ref): Annotated =
    Annotated(expr = expr, id = id, tparams = List.empty[TypeDecl], params = List.empty[Expr])

  /**
   * Annotation to mark part of the standard library
   */
  def std(expr: Expr): Annotated =
    Annotated(expr, Id("Std"))
