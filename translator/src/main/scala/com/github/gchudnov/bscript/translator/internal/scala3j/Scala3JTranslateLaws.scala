package com.github.gchudnov.bscript.translator.internal.scala3j

import com.github.gchudnov.bscript.translator.TranslateLaws
import com.github.gchudnov.bscript.translator.laws.{ Initializer, TypeConverter }
import com.github.gchudnov.bscript.translator.internal.scala3j.laws.Scala3JTypeConverter
import com.github.gchudnov.bscript.translator.internal.scala3.laws.ScalaInitializer
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, Type, VectorType }
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.translator.laws.TypeInit

/**
 * Laws used to translate AST to Scala with Java Types.
 */
private[translator] object Scala3JTranslateLaws:
  def make(typeNames: TypeNames, typeInit: TypeInit, meta: Meta): TranslateLaws = new TranslateLaws:
    override def typeConverter: TypeConverter = new Scala3JTypeConverter(typeNames)
    override def initializer: Initializer     = new ScalaInitializer(typeNames, typeInit, meta)
