package com.github.gchudnov.bscript.inspector

import com.github.gchudnov.bscript.lang.ast.AST

object Inspector:

  def init(): Unit =
    ()

  /**
   * Adds memory tracing capabilities to the given AST.
   */
  def memWatch(ast0: AST): Either[Throwable, AST] =
    ???
