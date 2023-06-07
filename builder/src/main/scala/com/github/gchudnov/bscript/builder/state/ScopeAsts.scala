package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.util.Dict
import com.github.gchudnov.bscript.builder.state.Scope
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.util.Ptr

/**
 * A dictionary of Scope -> AST mappings
 */
sealed trait ScopeAsts:
  def addScope(scope: Scope): ScopeAsts
  def link(scope: Scope, ast: AST): ScopeAsts
  def scope(ast: AST): Option[Scope]
  def asts(scope: Scope): List[AST]

object ScopeAsts:
  lazy val empty: ScopeAsts =
    BasicScopeAsts(keyValues = Map.empty[Scope, Set[Ptr[AST]]], valueKey = Map.empty[Ptr[AST], Scope])

/**
 * A Basic Scope-Ast Dictionary
 *
 * @param keyValues
 * @param valueKey
 */
private[state] final case class BasicScopeAsts(keyValues: Map[Scope, Set[Ptr[AST]]], valueKey: Map[Ptr[AST], Scope]) extends Dict[Scope, Ptr[AST], BasicScopeAsts] with ScopeAsts:
  override protected def clone(keyValues: Map[Scope, Set[Ptr[AST]]], valueKey: Map[Ptr[AST], Scope]): BasicScopeAsts =
    BasicScopeAsts(keyValues = keyValues, valueKey = valueKey)

  def addScope(scope: Scope): ScopeAsts =
    addKey(scope)

  def link(scope: Scope, ast: AST): ScopeAsts =
    set(scope, Ptr(ast))

  def scope(ast: AST): Option[Scope] =
    key(Ptr(ast))

  def asts(scope: Scope): List[AST] =
    values(scope).map(_.value)
