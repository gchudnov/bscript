package com.github.gchudnov.bscript.interpreter.internal

import com.github.gchudnov.bscript.lang.calc.{ Arithmetic, BoolArithmetic, Initializer }
import com.github.gchudnov.bscript.lang.memory.Comparator

final case class InterpretLaws(
  mathLaws: Arithmetic,
  boolLaws: BoolArithmetic,
  cmpLaws: Comparator,
  initLaws: Initializer
)
