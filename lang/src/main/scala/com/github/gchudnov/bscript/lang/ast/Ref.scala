package com.github.gchudnov.bscript.lang.ast

/**
  * Identifier, Id or Access
  */
abstract class Ref extends Expr:
  def path: List[String]
