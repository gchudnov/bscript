package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.const.Const

final case class Literal(const: Const) extends Expr
