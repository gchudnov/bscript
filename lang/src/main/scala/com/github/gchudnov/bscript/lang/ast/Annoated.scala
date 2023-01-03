package com.github.gchudnov.bscript.lang.ast

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
  *   is equivalent to:

  * {{{
  *   f(expr, ...params)
  * }}}
  * 
  *   that produces:

  * {{{
  *   expr1
  * }}}
  * 
  * And replaces the original `expr` during compilation.
  * 
  * The annotation is evaluated on the compile time.
  * 
  * @param expr Code the annotation attached to
  * @param id Annotation reference
  * @param params Parameters passed to annotation
  */
final case class Annotated(expr: Expr, id: Ref, tparams: List[TypeDecl], params: List[Expr]) extends Expr