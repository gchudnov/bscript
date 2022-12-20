package com.github.gchudnov.bscript.builder.visitors.internal

import com.github.gchudnov.bscript.builder.visitors.ScopeResolver
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.Meta


final class BasicScopeResolver(scopeAsts: ScopeAsts) extends ScopeResolver {

  override def resolve(symbol: Symbol): Option[Symbol] =
    ???

  override def resolveMember(symbol: Symbol): Option[Symbol] =
    ???

  override def result: Meta =
    Meta(
      forest = ???,
      scopeSymbols = ???,
      scopeAsts = scopeAsts
    )



}
