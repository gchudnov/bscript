package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.util.Ptr

final class ScopeAsts(protected val keyValues: Map[Scope, Set[Ptr[AST]]], protected val valueKey: Map[Ptr[AST], Scope]) extends Directory[Scope, AST, ScopeAsts]:
  override def clone(keyValues: Map[Scope, Set[Ptr[AST]]], valueKey: Map[Ptr[AST], Scope]): ScopeAsts =
    ScopeAsts(keyValues = keyValues, valueKey = valueKey)

  def addScope(scope: Scope): ScopeAsts =
    addKey(scope)

  def scope(ast: AST): Option[Scope] =
    key(ast)

  def asts(scope: Scope): List[AST] =
    values(scope)

object ScopeAsts:
  val empty: ScopeAsts =
    ScopeAsts(keyValues = Map.empty[Scope, Set[Ptr[AST]]], valueKey = Map.empty[Ptr[AST], Scope])
