package com.github.gchudnov.bscript.builder.visitors

import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.visitors.internal.BasicScopeResolver

/**
  * ScopeResolver
  */
trait ScopeResolver:
  def resolve(symbol: Symbol): Option[Symbol]
  def resolveMember(symbol: Symbol): Option[Symbol]

/**
 * ScopeResolver
 */
object ScopeResolver:
  def make(scopeAsts: ScopeAsts): ScopeResolver =
    
    new BasicScopeResolver(scopeAsts)
