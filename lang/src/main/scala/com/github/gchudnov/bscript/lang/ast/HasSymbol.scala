package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Symbol

/**
 * Reference to a symbol
 */
trait HasSymbol:
  def symbol: Symbol // Definition in the symbol table
