package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.util.Dict
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.builder.util.Ptr

/**
  * Scope-Symbol Dictionary
  *
  * @param keyValues
  * @param valueKey
  */
final case class ScopeSymbols(keyValues: Map[Scope, Set[Ptr[Symbol]]], valueKey: Map[Ptr[Symbol], Scope]) extends Dict[Scope, Ptr[Symbol], ScopeSymbols]:
  override def clone(keyValues: Map[Scope, Set[Ptr[Symbol]]], valueKey: Map[Ptr[Symbol], Scope]): ScopeSymbols =
    ScopeSymbols(keyValues = keyValues, valueKey = valueKey)

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

object ScopeSymbols:
  val empty: ScopeSymbols =
    ScopeSymbols(keyValues = Map.empty[Scope, Set[Ptr[Symbol]]], valueKey = Map.empty[Ptr[Symbol], Scope])
