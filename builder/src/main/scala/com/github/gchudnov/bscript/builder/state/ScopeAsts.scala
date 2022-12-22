package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.util.Ptr

final case class ScopeAsts(keyValues: Map[Scope, Set[Ptr[AST]]], valueKey: Map[Ptr[AST], Scope]) extends Dict[Scope, Ptr[AST], ScopeAsts]:
  override def clone(keyValues: Map[Scope, Set[Ptr[AST]]], valueKey: Map[Ptr[AST], Scope]): ScopeAsts =
    ScopeAsts(keyValues = keyValues, valueKey = valueKey)

  def addScope(scope: Scope): ScopeAsts =
    addKey(scope)

  def link(scope: Scope, ast: AST): ScopeAsts =
    set(scope, Ptr(ast))

  def scope(ast: AST): Option[Scope] =
    key(Ptr(ast))

  def asts(scope: Scope): List[AST] =
    values(scope).map(_.value)

object ScopeAsts:
  val empty: ScopeAsts =
    ScopeAsts(keyValues = Map.empty[Scope, Set[Ptr[AST]]], valueKey = Map.empty[Ptr[AST], Scope])
