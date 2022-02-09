package com.github.gchudnov.bscript.interpreter

import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.interpreter.laws.*

final case class IInterpretLaws(
  mathLaws: Arithmetic,
  boolLaws: BoolArithmetic,
  cmpLaws: Comparator,
  initLaws: Initializer,
  typeCaster: TypeCaster
) extends InterpretLaws

object IInterpretLaws:
  def make(types: Types, meta: Meta): InterpretLaws = IInterpretLaws(
    mathLaws = new IBasicArithmetic(),
    boolLaws = new IBasicBoolArithmetic(),
    cmpLaws = new IBasicComparator(),
    initLaws = new IBasicInitializer(types, meta),
    typeCaster = new IBasicTypeCaster(types)
  )
