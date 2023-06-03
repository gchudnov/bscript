package com.github.gchudnov.bscript.builder.pass.typecheck

import com.github.gchudnov.bscript.lang.ast.AST

private[builder] final case class InState(
  ast: AST
)

private[builder] object InState:
  def from(ast: AST): InState =
    InState(ast)
