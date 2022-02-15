package com.github.gchudnov.bscript.rewriter

import com.github.gchudnov.bscript.lang.ast.*

object Predicates:

  def hasStdAnn(n: AST): Boolean =
    n match
      case m: MethodDecl => m.annotations.exists(_.isInstanceOf[StdAnn])
      case _             => false
