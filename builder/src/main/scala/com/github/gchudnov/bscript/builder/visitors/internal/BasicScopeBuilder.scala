package com.github.gchudnov.bscript.builder.internal

import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.builder.state.ForestCursor
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.builder.visitors.ScopeBuilder
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.visitors.ScopeResolver

/**
 * BasicScopeBuilder
 */
final class BasicScopeBuilder(cursor: ForestCursor[Scope], scopeSymbols: ScopeSymbols, scopeAsts: ScopeAsts) extends ScopeBuilder:
  import Meta.*

  override def push(): ScopeBuilder =
    new BasicScopeBuilder(cursor = cursor.push(), scopeSymbols, scopeAsts)

  override def pop(): ScopeBuilder =
    new BasicScopeBuilder(cursor = cursor.pop(), scopeSymbols, scopeAsts)

  override def define(symbol: Symbol): ScopeBuilder =
    cursor.current match
      case Some(scope) =>
        new BasicScopeBuilder(cursor = cursor, scopeSymbols = scopeSymbols.addScope(scope).link(scope, symbol), scopeAsts)
      case None =>
        throw new BuilderException(s"Cannot define '${symbol}' without any scope. Invoke .push() to create a scope first.")

  override def bind(ast: AST): ScopeBuilder =
    cursor.current match
      case Some(scope) =>
        new BasicScopeBuilder(cursor = cursor, scopeSymbols = scopeSymbols, scopeAsts = scopeAsts.addScope(scope).link(scope, ast))
      case None =>
        throw new BuilderException(s"Cannot define '${ast}' without any scope. Invoke .push() to create a scope first.")

  override def toResolver: ScopeResolver =
    ScopeResolver.make(cursor.forest, scopeSymbols, scopeAsts)

  override def result: Meta =
    Meta(
      forest = cursor.forest,
      scopeSymbols = scopeSymbols,
      scopeAsts = scopeAsts
    )
