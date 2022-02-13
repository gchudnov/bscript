package com.github.gchudnov.bscript.b1.internal

import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.b1.internal.laws.*
import com.github.gchudnov.bscript.interpreter.laws.*
import com.github.gchudnov.bscript.interpreter.InterpreterLaws

object B1InterpreterLaws:
  def make(types: Types, meta: Meta): InterpreterLaws = new InterpreterLaws:
    override val mathLaws: Arithmetic     = new B1Arithmetic()
    override val boolLaws: BoolArithmetic = new B1BoolArithmetic()
    override val cmpLaws: Comparator      = new B1Comparator()
    override val initLaws: Initializer    = new B1Initializer(types, meta)
    override val typeCaster: TypeCaster   = new B1TypeCaster(types)
