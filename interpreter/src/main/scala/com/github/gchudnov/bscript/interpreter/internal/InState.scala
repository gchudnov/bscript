package com.github.gchudnov.bscript.interpreter.internal

import com.github.gchudnov.bscript.lang.ast.AST

final case class InState(
  ast: AST
)

object InState {
  
    def from(ast: AST): InState =
      InState(
        ast = ast
      )
}