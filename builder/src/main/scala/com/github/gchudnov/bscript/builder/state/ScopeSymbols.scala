package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.builder.util.Ptr

/**
 * A dictionary on Symbols and Scopes they are attached to
 *
 * @param scopeSymbols
 *   maps a scope to a collection of symbols
 * @param symbolScopes
 *   maps symbol to the scope it resides in
 */
case class ScopeSymbols(
  scopeSymbols: Map[Scope, Set[Symbol]],
  symbolScopes: Map[Ptr[Symbol], Scope]
):

  def addScope(scope: Scope): ScopeSymbols =
    this.copy(scopeSymbols = this.scopeSymbols + (scope -> Set.empty[Symbol]))

  def link(symbol: Symbol, scope: Scope): ScopeSymbols =
    val ss = scopeSymbols.getOrElse(scope, Set.empty[Symbol])

    assert(!ss.contains(symbol), s"Symbol ${symbol.name} already exists in the Scope ${scope.name}.")

    val newScopeSymbols = scopeSymbols + (scope       -> (ss + symbol))
    val newSymbolScopes = symbolScopes + (Ptr(symbol) -> scope)

    this.copy(
      scopeSymbols = newScopeSymbols,
      symbolScopes = newSymbolScopes
    )

object ScopeSymbols:
  val empty: ScopeSymbols =
    ScopeSymbols(
      scopeSymbols = Map.empty[Scope, Set[Symbol]],
      symbolScopes = Map.empty[Ptr[Symbol], Scope]
    )
