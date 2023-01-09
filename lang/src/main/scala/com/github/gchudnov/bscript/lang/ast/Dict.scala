package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.types.TypeAST
import com.github.gchudnov.bscript.lang.const.Const

/**
  * Map (Dictionary)
  *
  * @param m key-value pairs
  * @param keyType type of a key
  * @param valType type of a value
  */
final case class Dict(m: Map[Const, Expr], keyType: TypeAST, valType: TypeAST) extends Col
