package com.github.gchudnov.bscript.lang.ast.lit

import com.github.gchudnov.bscript.lang.ast.types.MethodType
import com.github.gchudnov.bscript.lang.ast.Block

/**
  * A Method Literal
  *
  * @param mType
  * @param body
  */
final case class MethodLit(mType: MethodType, body: Block) extends Lit
