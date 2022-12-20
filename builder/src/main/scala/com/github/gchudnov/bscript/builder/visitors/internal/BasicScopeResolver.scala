package com.github.gchudnov.bscript.builder.visitors.internal

import com.github.gchudnov.bscript.builder.visitors.ScopeResolver
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.lang.symbols.Type

final class BasicScopeResolver(scopeAsts: ScopeAsts) extends ScopeResolver {

  override def resolve(symbol: Symbol): Option[Symbol] =
    ???

  override def resolveMember(symbol: Symbol): Option[Symbol] =
    ???

  override def scope(ast: AST): Option[Scope] =
    ???

  override def defineVar(scope: Scope, name: String, vType: Type): ScopeResolver =
    ???


  override def result: Meta =
    Meta(
      forest = ???,
      scopeSymbols = ???,
      scopeAsts = scopeAsts
    )



}
