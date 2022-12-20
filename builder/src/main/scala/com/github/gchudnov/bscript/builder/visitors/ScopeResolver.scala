package com.github.gchudnov.bscript.builder.visitors

import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.visitors.internal.BasicScopeResolver
import com.github.gchudnov.bscript.builder.Meta

/**
  * ScopeResolver
  */
trait ScopeResolver:
  def resolve(symbol: Symbol): Option[Symbol]
  def resolveMember(symbol: Symbol): Option[Symbol]

  def result: Meta

/**
 * ScopeResolver
 */
object ScopeResolver:
  def make(scopeAsts: ScopeAsts): ScopeResolver =
    new BasicScopeResolver(scopeAsts)

  def fromMeta(meta: Meta): ScopeResolver =
    new BasicScopeResolver(meta.scopeAsts)
