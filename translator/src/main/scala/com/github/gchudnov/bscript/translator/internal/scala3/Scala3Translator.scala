package com.github.gchudnov.bscript.translator.internal.scala3

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.translator.TranslateLaws
import com.github.gchudnov.bscript.translator.internal.ScalaState
import com.github.gchudnov.bscript.translator.internal.ScalaTranslator
import com.github.gchudnov.bscript.translator.laws.TypeInit

object Scala3Translator:
  def make(meta: Meta, typeNames: TypeNames): ScalaTranslator =
    val typeInit = Scala3TypeInit
    val laws     = Scala3TranslateLaws.make(typeNames, typeInit, meta)
    val state    = Scala3State.make(meta)
    new ScalaTranslator(laws, state)
