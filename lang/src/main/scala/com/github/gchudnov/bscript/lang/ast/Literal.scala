package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.const.ConstVal

final case class Literal(const: ConstVal) extends Expr
