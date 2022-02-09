package com.github.gchudnov.bscript.interpreter.internal

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.interpreter.memory.*

final case class InterpretState(meta: Meta, memSpace: MemorySpace, retValue: Cell)

object InterpretState:
  def make(meta: Meta, memSpace: MemorySpace, retValue: Cell): InterpretState =
    InterpretState(
      meta = meta,
      memSpace = memSpace,
      retValue = retValue
    )
