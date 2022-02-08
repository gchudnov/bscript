package com.github.gchudnov.bscript.interpreter

import com.github.gchudnov.bscript.interpreter.laws.{ Arithmetic, BoolArithmetic, Initializer, Comparator, TypeCaster }

trait InterpretLaws:
  def mathLaws: Arithmetic
  def boolLaws: BoolArithmetic
  def cmpLaws: Comparator
  def initLaws: Initializer
  def typeCaster: TypeCaster
