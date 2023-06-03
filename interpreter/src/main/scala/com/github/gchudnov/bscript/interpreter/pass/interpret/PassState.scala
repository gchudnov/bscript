package com.github.gchudnov.bscript.interpreter.pass.interpret

import com.github.gchudnov.bscript.lang.ast.AST

private[interpret] final case class PassState(

)

object PassState:

  def from(in: InState): PassState =
    PassState()

  def into(state: PassState, ast: AST): OutState =
    OutState(
      ast = ast
    )
