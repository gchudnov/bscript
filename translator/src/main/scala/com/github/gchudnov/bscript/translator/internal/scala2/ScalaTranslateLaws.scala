package com.github.gchudnov.bscript.translator.internal.scala2

import com.github.gchudnov.bscript.translator.TranslateLaws
import com.github.gchudnov.bscript.translator.laws.{ Initializer, TypeConverter }
import com.github.gchudnov.bscript.translator.internal.scala2.laws.{ ScalaInitializer, ScalaTypeConverter }
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, Type, VectorType }
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.builder.state.Meta

/**
 * Laws used to translate AST to Scala.
 */
private[translator] object ScalaTranslateLaws:
  def make(typeNames: TypeNames, meta: Meta): TranslateLaws = new TranslateLaws:
    override def typeConverter: TypeConverter = new ScalaTypeConverter(typeNames)
    override def initializer: Initializer     = new ScalaInitializer(typeNames, meta)
