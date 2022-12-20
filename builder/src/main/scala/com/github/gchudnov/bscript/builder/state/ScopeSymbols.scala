package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.builder.util.Ptr

final class ScopeSymbols(protected val keyValues: Map[Scope, Set[Ptr[Symbol]]], protected val valueKey: Map[Ptr[Symbol], Scope]) extends Directory[Scope, Symbol, ScopeSymbols]:
  override def clone(keyValues: Map[Scope, Set[Ptr[Symbol]]], valueKey: Map[Ptr[Symbol], Scope]): ScopeSymbols =
    ScopeSymbols(keyValues = keyValues, valueKey = valueKey)

  def addScope(scope: Scope): ScopeSymbols =
    addKey(scope)

  def scope(ast: Symbol): Option[Scope] =
    key(ast)

  def symbols(scope: Scope): List[Symbol] =
    values(scope)

  def symbolsByName(name: String): List[Symbol] =
    valueKey.keySet.toList.collect { case Ptr[Symbol](sym) if sym.name == name => sym }

object ScopeSymbols:
  val empty: ScopeSymbols =
    ScopeSymbols(keyValues = Map.empty[Scope, Set[Ptr[Symbol]]], valueKey = Map.empty[Ptr[Symbol], Scope])
