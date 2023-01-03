package com.github.gchudnov.bscript.builder.internal

import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.builder.state.ForestCursor
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.builder.internal.BasicScopeBuilder
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.lang.ast.AST

import com.github.gchudnov.bscript.builder.internal.scopebuilder.BasicScopeBuilder

import com.github.gchudnov.bscript.builder.internal.ScopeResolver
/**
 * ScopeBuilder
 */
trait ScopeBuilder:
  def push(): ScopeBuilder
  def pop(): ScopeBuilder

  def define(symbol: Symbol): ScopeBuilder
  def bind(ast: AST): ScopeBuilder

  def result: Meta

  def toResolver: ScopeResolver

/**
 * ScopeBuilder
 */
object ScopeBuilder:

  def make(): ScopeBuilder =
    val cursor = ForestCursor.empty[Scope](a => ScopeRef(a))
    val scopeSymbols = ScopeSymbols.empty
    val scopeAsts = ScopeAsts.empty
    
    new BasicScopeBuilder(cursor, scopeSymbols, scopeAsts)
