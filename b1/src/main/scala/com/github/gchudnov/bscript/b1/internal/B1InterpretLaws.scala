package com.github.gchudnov.bscript.b1

import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.b1.internal.laws.*
import com.github.gchudnov.bscript.interpreter.laws.*
import com.github.gchudnov.bscript.interpreter.InterpretLaws

object B1InterpretLaws:
  def make(types: Types, meta: Meta): InterpretLaws = new InterpretLaws {
    override val mathLaws: Arithmetic = new B1BasicArithmetic()
    override val boolLaws: BoolArithmetic = new B1BasicBoolArithmetic()
    override val cmpLaws: Comparator = new B1BasicComparator()
    override val initLaws: Initializer = new B1BasicInitializer(types, meta)
    override val typeCaster: TypeCaster = new B1BasicTypeCaster(types)
  }
