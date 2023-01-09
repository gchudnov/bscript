package com.github.gchudnov.bscript.lang.ast

/**
  * Named Argument to a function
  * 
  * {{{
  *   void myFunc(x: i8, s: str) { ... }
  * 
  *   myFunc(x = 10, s = "abc")
  * }}}
  */
final case class NamedArg(name: String, arg: Expr) extends Expr
