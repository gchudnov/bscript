package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.builder.util.Ptr

/**
 * A dictionary on Entries and Scopes they are attached to
 *
 * @param scopeEntries
 *   maps a scope to a collection of entries
 * @param entryScopes
 *   maps an entry to the scope it resides in
 */
case class ScopeDirectory[A <: AnyRef](
  scopeEntries: Map[Scope, Set[Ptr[A]]],
  entryScopes: Map[Ptr[A], Scope]
):

  def addScope(scope: Scope): ScopeDirectory[A] =
    this.copy(scopeEntries = this.scopeEntries + (scope -> Set.empty[Ptr[A]]))

  def link(entry: A, scope: Scope): ScopeDirectory[A] =
    val ss = scopeEntries.getOrElse(scope, Set.empty[Ptr[A]])

    assert(!ss.contains(Ptr(entry)), s"Entry ${entry} is already linked to the Scope ${scope.name}, cannot link it twice.")

    val newScopeEntries = scopeEntries + (scope     -> (ss + Ptr(entry)))
    val newEntryScopes  = entryScopes + (Ptr(entry) -> scope)

    this.copy(
      scopeEntries = newScopeEntries,
      entryScopes = newEntryScopes
    )

object ScopeDirectory:
  def empty[A <: AnyRef]: ScopeDirectory[A] =
    ScopeDirectory(
      scopeEntries = Map.empty[Scope, Set[Ptr[A]]],
      entryScopes = Map.empty[Ptr[A], Scope]
    )
