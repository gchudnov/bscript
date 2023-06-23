package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.util.Dict
import com.github.gchudnov.bscript.builder.state.Scope
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.builder.util.Ptr
import com.github.gchudnov.bscript.builder.util.Tree
import com.github.gchudnov.bscript.builder.util.Show
import ScopeSymbols.given

/**
 * Scope-Symbol Dictionary Interface
 *
 *   - A scope can have multiple symbols
 *   - A symbol can belong to only one scope
 */
sealed trait ScopeSymbols:
  def link(scope: Scope, sym: Symbol): ScopeSymbols
  
  def scope(ast: Symbol): Option[Scope]
  def scopes: List[Scope]
  def symbols(scope: Scope): List[Symbol]
  def symbols: List[Symbol]

  def resolveIn(name: String, in: Scope): Option[Symbol]
  def resolveUp(name: String, start: Scope, scopeTree: Tree[Scope]): Option[Symbol]

  def asString: String

object ScopeSymbols:
  lazy val empty: ScopeSymbols =
    BasicScopeSymbols(keyValues = Map.empty[Scope, Set[Ptr[Symbol]]], valueKey = Map.empty[Ptr[Symbol], Scope])

  given showSymbol: Show[Symbol] = new Show[Symbol] {
    override def show(a: Symbol): String =
      s"symbol(${a.toString})"
  }

  given showPtrSymbol: Show[Ptr[Symbol]] = new Show[Ptr[Symbol]] {
    override def show(a: Ptr[Symbol]): String =
      s"ptr(${showSymbol.show(a.value)})"
  }

/**
  * Scope-Symbol Dictionary Implementation
  */
private[state] final case class BasicScopeSymbols(keyValues: Map[Scope, Set[Ptr[Symbol]]], valueKey: Map[Ptr[Symbol], Scope])
    extends Dict[Scope, Ptr[Symbol], BasicScopeSymbols]
    with ScopeSymbols:
  override def clone(keyValues: Map[Scope, Set[Ptr[Symbol]]], valueKey: Map[Ptr[Symbol], Scope]): BasicScopeSymbols =
    BasicScopeSymbols(keyValues = keyValues, valueKey = valueKey)

  override def link(scope: Scope, sym: Symbol): ScopeSymbols =
    set(scope, Ptr(sym))

  override def scope(ast: Symbol): Option[Scope] =
    key(Ptr(ast))

  override def scopes: List[Scope] =
    keyValues.keySet.toList

  override def symbols(scope: Scope): List[Symbol] =
    values(scope).map(_.value)

  override def symbols: List[Symbol] =
    valueKey.keySet.toList.map(_.value)

  override def resolveIn(name: String, in: Scope): Option[Symbol] =
    symbols(in)
      .find(_.name == name)

  override def resolveUp(name: String, start: Scope, scopeTree: Tree[Scope]): Option[Symbol] =
    symbols(start)
      .find(_.name == name)
      .orElse(scopeTree.parentOf(start).flatMap(parent => resolveUp(name, parent, scopeTree)))

  override def asString: String =
    show
