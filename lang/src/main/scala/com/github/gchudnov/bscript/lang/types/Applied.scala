package com.github.gchudnov.bscript.lang.types

/**
  * Type Application
  */
final case class Applied(aType: TypeAST, args: List[TypeAST]) extends TypeAST
