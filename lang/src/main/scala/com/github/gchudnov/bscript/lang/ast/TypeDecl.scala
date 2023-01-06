package com.github.gchudnov.bscript.lang.ast

/**
  * Type Definition, including built-in types
  * 
  * {{{
  *   Int
  *   Float
  *   String
  * }}}
  */
final case class TypeDecl(name: String) extends Decl
