package com.github.gchudnov.bscript.rewriter

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.rewriter.internal.{ FilterVisitor, FilterState, MapState }

object Rewriter:

  def filter(pred: (AST) => Boolean): TreeVisitor[FilterState, AST] =
    FilterVisitor.make(pred)

  def map(f: (AST) => AST): TreeVisitor[MapState, AST] =
    ???
