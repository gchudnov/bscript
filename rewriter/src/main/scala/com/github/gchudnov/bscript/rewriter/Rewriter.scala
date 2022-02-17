package com.github.gchudnov.bscript.rewriter

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.rewriter.internal.{ FilterState, FilterVisitor, FindState, FindVisitor, MapState, MapVisitor }

object Rewriter:

  /**
   * FILTER: Includes all nodes in the AST that match the given predicate, otherwise excludes them.
   */
  def filter(ast: AST, pred: (AST) => Boolean): Either[Throwable, Option[AST]] =
    val filterVisitor = FilterVisitor.make(pred)
    val filterState   = FilterState.make()
    ast.visit(filterState, filterVisitor)

  /**
   * MAP: Maps all nodes in the AST using the given function.
   */
  def map(ast: AST, f: (AST) => AST): Either[Throwable, AST] =
    val mapVisitor = MapVisitor.make(f)
    val mapState   = MapState.make()
    ast.visit(mapState, mapVisitor)

  /**
   * FIND: Finds the first node in the AST that matches the given predicate.
   */
  def find(ast: AST, pred: (AST) => Boolean): Either[Throwable, Option[AST]] =
    val findVisitor = FindVisitor.make(pred)
    val findState   = FindState.make()
    ast.visit(findState, findVisitor)
