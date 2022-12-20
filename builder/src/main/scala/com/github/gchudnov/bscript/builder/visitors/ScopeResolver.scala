package com.github.gchudnov.bscript.builder.visitors

import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.visitors.internal.BasicScopeResolver
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.lang.symbols.Type

/**
  * ScopeResolver
  */
trait ScopeResolver:
  def resolve(symbol: Symbol): Option[Symbol]
  def resolveMember(symbol: Symbol): Option[Symbol]

  def scope(ast: AST): Option[Scope]

  def defineVar(scope: Scope, name: String, vType: Type): ScopeResolver

  def result: Meta

/**
 * ScopeResolver
 */
object ScopeResolver:
  def make(scopeAsts: ScopeAsts): ScopeResolver =
    new BasicScopeResolver(scopeAsts)

  def fromMeta(meta: Meta): ScopeResolver =
    new BasicScopeResolver(meta.scopeAsts)
