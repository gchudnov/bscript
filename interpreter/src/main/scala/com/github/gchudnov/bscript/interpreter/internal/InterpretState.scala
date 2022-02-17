package com.github.gchudnov.bscript.interpreter.internal

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.interpreter.memory.*

final case class InterpretState(meta: Meta, stash: Stash, memSpace: MemorySpace, retValue: Cell)

object InterpretState:
  def make(meta: Meta, stash: Stash, memSpace: MemorySpace, retValue: Cell): InterpretState =
    InterpretState(
      meta = meta,
      stash = stash,
      memSpace = memSpace,
      retValue = retValue
    )
