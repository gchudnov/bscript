package com.github.gchudnov.bscript.rewriter

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.rewriter.internal.{ FilterState, FilterVisitor, MapState, MapVisitor }

object Rewriter:

  def filter(ast: AST, pred: (AST) => Boolean): Either[Throwable, AST] =
    val filterVisitor = FilterVisitor.make(pred)
    val filterState   = FilterState.make()
    ast.visit(filterState, filterVisitor)

  def map(ast: AST, f: (AST) => AST): Either[Throwable, AST] =
    val mapVisitor = MapVisitor.make(f)
    val mapState   = MapState.make()
    ast.visit(mapState, mapVisitor)
