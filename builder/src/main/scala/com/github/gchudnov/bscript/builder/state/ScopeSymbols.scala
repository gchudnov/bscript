package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.util.Dict
import com.github.gchudnov.bscript.builder.state.Scope
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.builder.util.Ptr
import com.github.gchudnov.bscript.builder.util.Tree

sealed trait ScopeSymbols {
  def addScope(scope: Scope): ScopeSymbols
  def link(scope: Scope, sym: Symbol): ScopeSymbols
  def scope(ast: Symbol): Option[Scope]
  def symbols(scope: Scope): List[Symbol]
  def symbolsByName(name: String): List[Symbol]

  def resolveIn(name: String, in: Scope): Option[Symbol]
  def resolveUp(name: String, start: Scope, scopeTree: Tree[Scope]): Option[Symbol]
}

object ScopeSymbols:
  lazy val empty: ScopeSymbols =
    BasicScopeSymbols(keyValues = Map.empty[Scope, Set[Ptr[Symbol]]], valueKey = Map.empty[Ptr[Symbol], Scope])

/**
  * Scope-Symbol Dictionary
  *
  * @param keyValues
  * @param valueKey
  */
private[state] final case class BasicScopeSymbols(keyValues: Map[Scope, Set[Ptr[Symbol]]], valueKey: Map[Ptr[Symbol], Scope]) extends Dict[Scope, Ptr[Symbol], BasicScopeSymbols] with ScopeSymbols:
  override def clone(keyValues: Map[Scope, Set[Ptr[Symbol]]], valueKey: Map[Ptr[Symbol], Scope]): BasicScopeSymbols =
    BasicScopeSymbols(keyValues = keyValues, valueKey = valueKey)

  def addScope(scope: Scope): ScopeSymbols =
    addKey(scope)

  def link(scope: Scope, sym: Symbol): ScopeSymbols =
    set(scope, Ptr(sym))

  def scope(ast: Symbol): Option[Scope] =
    key(Ptr(ast))

  def symbols(scope: Scope): List[Symbol] =
    values(scope).map(_.value)

  def symbolsByName(name: String): List[Symbol] =
    valueKey.keySet.toList.collect { case Ptr[Symbol](sym) if sym.name == name => sym }

  override def resolveIn(name: String, in: Scope): Option[Symbol] =
    symbols(in)
      .find(_.name == name)


  override def resolveUp(name: String, start: Scope, scopeTree: Tree[Scope]): Option[Symbol] =
    symbols(start)
      .find(_.name == name)
      .orElse(scopeTree.parentOf(start).flatMap(parent => resolveUp(name, parent, scopeTree)))
