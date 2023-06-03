package com.github.gchudnov.bscript.builder.pass.scopebuild

import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.builder.state.Scope
import com.github.gchudnov.bscript.builder.state.ScopeRef
import com.github.gchudnov.bscript.builder.pass.scopebuild.InState
import com.github.gchudnov.bscript.builder.pass.scopebuild.OutState
import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.builder.state.ForestCursor
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.symbols.Symbol

/**
  * Pass State
  *
  * @param cursor
  * @param scopeSymbols
  * @param scopeAsts
  */
final case class PassState(cursor: ForestCursor[Scope], scopeSymbols: ScopeSymbols, scopeAsts: ScopeAsts):

  def push(): PassState =
    this.copy(cursor = cursor.push())

  def pop(): PassState =
    this.copy(cursor = cursor.pop())

  def define(symbol: Symbol): PassState =
    cursor.current match
      case Some(scope) =>
        this.copy(scopeSymbols = scopeSymbols.addScope(scope).link(scope, symbol))
      case None =>
        throw new BuilderException(s"Cannot define '${symbol}' without any scope. Invoke .push() to create a scope first.")

  def bind(ast: AST): PassState =
    cursor.current match
      case Some(scope) =>
        this.copy(scopeAsts = scopeAsts.addScope(scope).link(scope, ast))
      case None =>
        throw new BuilderException(s"Cannot define '${ast}' without any scope. Invoke .push() to create a scope first.")

object PassState:

  lazy val empty: PassState =
    val cursor       = ForestCursor.empty[Scope](it => ScopeRef(it))
    val scopeSymbols = ScopeSymbols.empty
    val scopeAsts    = ScopeAsts.empty

    PassState(
      cursor,
      scopeSymbols,
      scopeAsts
    )

  def from(s: InState): PassState =
    empty

  def into(s: PassState, ast: AST): OutState =
    OutState(
      ast = ast,
      forest = s.cursor.forest,
      scopeSymbols = s.scopeSymbols,
      scopeAsts = s.scopeAsts
    )
