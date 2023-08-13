package com.github.gchudnov.bscript.lang.symbols

/**
  * A struct symbol
  *
  * {{{
  *   NAME
  * }}}
  * 
  * @param name
  *   name
  */
final case class SStruct(name: String) extends Symbol
