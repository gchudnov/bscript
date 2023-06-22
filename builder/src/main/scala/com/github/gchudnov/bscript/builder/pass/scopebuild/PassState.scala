package com.github.gchudnov.bscript.builder.pass.scopebuild

import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.builder.state.Scope
import com.github.gchudnov.bscript.builder.state.ScopeRef
import com.github.gchudnov.bscript.builder.pass.scopebuild.InState
import com.github.gchudnov.bscript.builder.pass.scopebuild.OutState
import com.github.gchudnov.bscript.builder.util.Tree
import com.github.gchudnov.bscript.builder.state.TreeCursor
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.symbols.Symbol

/**
 * Pass State
 *
 * @param scopeCursor
 *   forest cursor, icnluding the current scope
 * @param scopeSymbols
 *   scope symbols
 * @param scopeAsts
 *   scope ASTs
 */
private[scopebuild] final case class PassState(
  scopeCursor: TreeCursor[Scope], 
  scopeSymbols: ScopeSymbols, 
  scopeAsts: ScopeAsts,
):

  def push(): PassState =
    this.copy(scopeCursor = scopeCursor.push())

  def pop(): PassState =
    this.copy(scopeCursor = scopeCursor.pop())

  def define(symbol: Symbol): PassState =
    scopeCursor.at match
      case Some(scope) =>
        this.copy(scopeSymbols = scopeSymbols.addScope(scope).link(scope, symbol))
      case None =>
        throw new BuilderException(s"Cannot define Symbol '${symbol}' without a current scope. Invoke .push() to create a scope first.")

  def bind(ast: AST): PassState =
    scopeCursor.at match
      case Some(scope) =>
        this.copy(scopeAsts = scopeAsts.addScope(scope).link(scope, ast))
      case None =>
        throw new BuilderException(s"Cannot bind AST '${ast}' to Scope without a current scope. Invoke .push() to create a scope first.")

object PassState:

  lazy val empty: PassState =
    val cursor       = TreeCursor.empty[Scope](it => ScopeRef(it))
    val scopeSymbols = ScopeSymbols.empty
    val scopeAsts    = ScopeAsts.empty

    PassState(
      cursor,
      scopeSymbols,
      scopeAsts,
    )

  def from(s: InState): PassState =
    empty

  def into(s: PassState, ast: AST): OutState =
    OutState(
      ast = ast,
      scopeTree = s.scopeCursor.tree,
      scopeSymbols = s.scopeSymbols,
      scopeAsts = s.scopeAsts,
    )
