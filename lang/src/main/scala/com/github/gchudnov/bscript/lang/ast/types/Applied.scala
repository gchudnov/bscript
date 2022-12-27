package com.github.gchudnov.bscript.lang.ast.types

/**
  * Type Application
  */
final case class Applied(aType: TypeAST, args: List[TypeAST]) extends TypeAST
