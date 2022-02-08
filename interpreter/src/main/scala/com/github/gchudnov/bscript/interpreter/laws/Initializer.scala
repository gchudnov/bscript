package com.github.gchudnov.bscript.interpreter.laws

import com.github.gchudnov.bscript.interpreter.memory.{ Cell, MemoryException }
import com.github.gchudnov.bscript.lang.symbols.Type

trait Initializer:
  def init(toType: Type): Either[MemoryException, Cell]
