package com.github.gchudnov.bscript.builder.pass.scopebuilder

import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.builder.state.ForestCursor
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.lang.ast.AST

final case class ScopeBuildState(cursor: ForestCursor[Scope], scopeSymbols: ScopeSymbols, scopeAsts: ScopeAsts):

  def push(): ScopeBuildState =
    this.copy(cursor = cursor.push())

  def pop(): ScopeBuildState =
    this.copy(cursor = cursor.pop())

  def define(symbol: Symbol): ScopeBuildState =
    cursor.current match
      case Some(scope) =>
        this.copy(scopeSymbols = scopeSymbols.addScope(scope).link(scope, symbol))
      case None =>
        throw new BuilderException(s"Cannot define '${symbol}' without any scope. Invoke .push() to create a scope first.")

  def bind(ast: AST): ScopeBuildState =
    cursor.current match
      case Some(scope) =>
        this.copy(scopeAsts = scopeAsts.addScope(scope).link(scope, ast))
      case None =>
        throw new BuilderException(s"Cannot define '${ast}' without any scope. Invoke .push() to create a scope first.")

object ScopeBuildState:

  def from(s: ScopeBuildInState): ScopeBuildState =
    val cursor       = ForestCursor.empty[Scope](it => ScopeRef(it))
    val scopeSymbols = ScopeSymbols.empty
    val scopeAsts    = ScopeAsts.empty

    ScopeBuildState(
      cursor, 
      scopeSymbols, 
      scopeAsts
    )

  def to(ast: AST, s: ScopeBuildState): ScopeBuildOutState =
    ScopeBuildOutState(ast)
