package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.lang.symbols.Symbol

/**
 * Scope-Symbol Pair
 *
 * @param scope
 *   scope
 * @param symbol
 *   symbol
 */
final case class ScopeSymbol(scope: Scope, symbol: Symbol)
