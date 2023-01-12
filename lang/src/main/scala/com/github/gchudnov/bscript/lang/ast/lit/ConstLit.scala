package com.github.gchudnov.bscript.lang.ast.lit

import com.github.gchudnov.bscript.lang.const.Const

final case class ConstLit(const: Const) extends Lit
