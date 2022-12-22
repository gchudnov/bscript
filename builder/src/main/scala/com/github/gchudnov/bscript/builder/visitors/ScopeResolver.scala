package com.github.gchudnov.bscript.builder.visitors

import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.visitors.internal.BasicScopeResolver
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.lang.symbols.Type
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.Symbol

/**
  * ScopeResolver
  */
trait ScopeResolver:
  def defineVar(scope: ScopeRef, name: String, vType: TypeRef): ScopeResolver

  def scopeFor(ast: AST): Option[Scope]
  def resolve(sym: SymbolRef, start: Scope): Option[Symbol]

  def result: Meta

/**
 * ScopeResolver
 */
object ScopeResolver:
  def make(forest: Forest[Scope], scopeSymbols: ScopeSymbols, scopeAsts: ScopeAsts): ScopeResolver =
    new BasicScopeResolver(forest, scopeSymbols, scopeAsts)
