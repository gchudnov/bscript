package com.github.gchudnov.bscript.builder.internal

import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.builder.state.ForestCursor
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.lang.symbols.Symbol


/**
 * BasicScopeBuilder
 */
final class BasicScopeBuilder(cursor: ForestCursor[Scope], scopeSymbols: ScopeSymbols) extends ScopeBuilder:
  import Meta.*

  override def push(): ScopeBuilder =
    new BasicScopeBuilder(cursor = cursor.push(), scopeSymbols)

  override def pop(): ScopeBuilder =
    new BasicScopeBuilder(cursor = cursor.pop(), scopeSymbols)

  override def define(symbol: Symbol): ScopeBuilder =
    cursor.current match {
      case Some(scope) =>
        new BasicScopeBuilder(cursor = cursor, scopeSymbols = scopeSymbols.addScope(scope).link(symbol, scope))
      case None =>
        throw new BuilderException(s"Cannot define '${symbol}' symbol without any scope. Run .push() first to create a scope")
    }

  override def result: Meta =
    Meta(
      forest = cursor.forest,
      scopeSymbols = scopeSymbols
    )
