package com.github.gchudnov.bscript.interpreter

import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.lang.symbols.state.Meta

final case class IInterpretLaws(
  mathLaws: Arithmetic,
  boolLaws: BoolArithmetic,
  cmpLaws: Comparator,
  initLaws: Initializer,
  typeCaster: TypeCaster
) extends InterpretLaws

object IInterpretLaws:
  def make(types: Types, meta: Meta): InterpretLaws = IInterpretLaws(
    mathLaws = new BuiltInArithmetic(),
    boolLaws = new BuiltInBoolArithmetic(),
    cmpLaws = new BuiltInComparator(),
    initLaws = new BasicInitializer(types, meta)
  )
