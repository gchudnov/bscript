package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }

/**
 * Argument Declaration
 * 
 * HasSymbol
 */
final case class ArgDecl(aType: Type, name: String) extends Decl