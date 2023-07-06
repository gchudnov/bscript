package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.util.Dict
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.builder.util.Ptr
import com.github.gchudnov.bscript.builder.util.Tree
import com.github.gchudnov.bscript.builder.util.ReadTree
import com.github.gchudnov.bscript.builder.util.WriteTree
import com.github.gchudnov.bscript.lang.util.Show
import com.github.gchudnov.bscript.builder.state.ScopeSymbols.given

/**
 * Read Scope Symbols
 */
sealed trait ReadScopeSymbols:
  def isEmpty: Boolean
  def size: Int

  def hasLink(scope: Scope, sym: Symbol): Boolean

  def scopes: List[Scope]
  def symbols(scope: Scope): List[Symbol]
  def symbols: List[Symbol]

  def resolveIn(name: String, in: Scope): Option[Symbol]
  def resolveUp(name: String, start: Scope, scopeTree: ReadTree[Scope]): Option[ScopeSymbol]

  def asString: String

/**
 * Write ScopeSymbols
 */
sealed trait WriteScopeSymbols:
  def link(scope: Scope, sym: Symbol): ScopeSymbols

/**
 * Scope-Symbol Dictionary Interface
 *
 *   - A scope can have multiple symbols
 *   - A symbol can belong to only one scope
 */
sealed trait ScopeSymbols extends ReadScopeSymbols with WriteScopeSymbols

object ScopeSymbols:
  lazy val empty: ScopeSymbols =
    BasicScopeSymbols(keyValues = Map.empty[Scope, Set[Symbol]])

  given showSymbol: Show[Symbol] = new Show[Symbol]:
    override def show(a: Symbol): String =
      s"symbol(${a.toString})"

  given showPtrSymbol: Show[Ptr[Symbol]] = new Show[Ptr[Symbol]]:
    override def show(a: Ptr[Symbol]): String =
      s"ptr(${showSymbol.show(a.value)})"

/**
 * Scope-Symbol Dictionary Implementation
 */
private final case class BasicScopeSymbols(keyValues: Map[Scope, Set[Symbol]]) extends Dict[Scope, Symbol, BasicScopeSymbols] with ScopeSymbols:

  override def isEmpty: Boolean =
    keyValues.isEmpty

  override def size: Int =
    keyValues.size

  override def clone(keyValues: Map[Scope, Set[Symbol]]): BasicScopeSymbols =
    BasicScopeSymbols(keyValues = keyValues)

  override def link(scope: Scope, sym: Symbol): ScopeSymbols =
    set(scope, sym)

  /**
   * Checks whether the given scope contains the given Symbol.
   */
  override def hasLink(scope: Scope, sym: Symbol): Boolean =
    contains(scope, sym)

  override def scopes: List[Scope] =
    keyValues.keySet.toList

  override def symbols(scope: Scope): List[Symbol] =
    values(scope)

  override def symbols: List[Symbol] =
    keyValues.values.flatMap(_.toList).toList

  override def resolveIn(name: String, in: Scope): Option[Symbol] =
    symbols(in)
      .find(_.name == name)

  override def resolveUp(name: String, start: Scope, scopeTree: ReadTree[Scope]): Option[ScopeSymbol] =
    resolveIn(name, start)
      .map(sym => ScopeSymbol(start, sym))
      .orElse(scopeTree.parentOf(start).flatMap(parent => resolveUp(name, parent, scopeTree)))

  override def asString: String =
    show
