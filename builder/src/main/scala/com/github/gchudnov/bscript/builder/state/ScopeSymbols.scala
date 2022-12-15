package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.builder.util.Ptr

case class ScopeSymbols(
  scopeSymbols: Map[Scope, List[Symbol]],
  symbolScopes: Map[Ptr[Symbol], Scope]
) {

}

object ScopeSymbols {
  val empty: ScopeSymbols =
    ScopeSymbols(
      scopeSymbols = Map.empty[Scope, List[Symbol]],
      symbolScopes = Map.empty[Ptr[Symbol], Scope]
    )
}
