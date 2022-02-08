package com.github.gchudnov.bscript.interpreter

import com.github.gchudnov.bscript.lang.calc.{ Arithmetic, BoolArithmetic, Initializer }
import com.github.gchudnov.bscript.lang.memory.Comparator

trait InterpretLaws:
  def mathLaws: Arithmetic
  def boolLaws: BoolArithmetic
  def cmpLaws: Comparator
  def initLaws: Initializer
