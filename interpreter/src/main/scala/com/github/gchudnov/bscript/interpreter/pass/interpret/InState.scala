package com.github.gchudnov.bscript.interpreter.pass.interpret

import com.github.gchudnov.bscript.lang.ast.AST

private[interpreter] final case class InState(
  ast: AST
)

private[interpreter] object InState {
  
    def from(ast: AST): InState =
      InState(
        ast = ast
      )
}