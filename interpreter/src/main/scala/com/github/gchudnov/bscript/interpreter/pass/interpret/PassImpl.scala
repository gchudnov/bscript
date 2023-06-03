package com.github.gchudnov.bscript.interpreter.pass.interpret

import com.github.gchudnov.bscript.interpreter.pass.Pass

/**
  * Interpret Pass
  */
private[interpreter] final class PassImpl() extends Pass:

  type In  = InState
  type Out = OutState

  override def run(in: InState): OutState =
    val folder = Folder.make()

    val state0         = PassState.from(in)
    val state1         = folder.foldAST(state0, in.ast)

    val out = PassState.into(state1)

    out
