package com.github.gchudnov.bscript.interpreter.pass.interpret

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.interpreter.memory.*

private[interpret] final case class PassState(
  retValue: Cell
)

object PassState:

  def from(in: InState): PassState =
    PassState(
      retValue = VoidCell
    )

  def into(state: PassState): OutState =
    OutState(
      retValue = state.retValue
    )
