package com.github.gchudnov.bscript.lang.ast.lit

import com.github.gchudnov.bscript.lang.const.Const

/**
  * A Constant Literal
  *
  *
  * @param const
  */
final case class ConstLit(const: Const) extends Lit
