package com.github.gchudnov.bscript.translator.into.scalax.scala3j

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.translator.TranslateLaws
import com.github.gchudnov.bscript.translator.into.scalax.{ScalaState, ScalaTranslator}
import com.github.gchudnov.bscript.translator.laws.TypeInit

object Scala3JTranslator:
  def make(meta: Meta, typeNames: TypeNames): ScalaTranslator =
    val typeInit = Scala3JTypeInit
    val laws     = Scala3JTranslateLaws.make(typeNames, typeInit, meta)
    val state    = Scala3JState.make(meta)
    new ScalaTranslator(laws, state)
