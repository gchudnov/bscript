package com.github.gchudnov.bscript.rewriter.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.util.{ Casting, Transform }

/**
 * Maps AST-node to a different AST-node
 */
private[internal] final class MapVisitor(f: (AST) => AST) {
  
  // TODO: impl it

}


private[rewriter] object MapVisitor:

  def make(f: (AST) => AST): MapVisitor =
    new MapVisitor(f)
