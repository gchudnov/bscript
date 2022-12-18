package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.builder.util.Ptr

final case class ScopeSymbols(keyValues: Map[Scope, Set[Ptr[Symbol]]], valueKey: Map[Ptr[Symbol], Scope]) extends Directory[Scope, Symbol, ScopeSymbols]:
  override def clone(keyValues: Map[Scope, Set[Ptr[Symbol]]], valueKey: Map[Ptr[Symbol], Scope]): ScopeSymbols =
    ScopeSymbols(keyValues = keyValues, valueKey = valueKey)

  def addScope(scope: Scope): ScopeSymbols =
    addKey(scope)

object ScopeSymbols:
  val empty: ScopeSymbols =
    ScopeSymbols(keyValues = Map.empty[Scope, Set[Ptr[Symbol]]], valueKey = Map.empty[Ptr[Symbol], Scope])
