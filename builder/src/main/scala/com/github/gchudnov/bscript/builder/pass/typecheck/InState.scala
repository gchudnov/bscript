package com.github.gchudnov.bscript.builder.pass.typecheck

import com.github.gchudnov.bscript.lang.ast.AST

final case class InState(
  ast: AST
)

object InState:
  def from(ast: AST): InState =
    InState(ast)
