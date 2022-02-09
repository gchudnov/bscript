package com.github.gchudnov.bscript.translator.internal.scala2

import com.github.gchudnov.bscript.translator.TranslateLaws
import com.github.gchudnov.bscript.translator.laws.{ Initializer, TypeConverter }
import com.github.gchudnov.bscript.translator.internal.scala2j.laws.{ JavaTypeConverter }
import com.github.gchudnov.bscript.translator.internal.scala2.laws.{ ScalaInitializer }
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, Type, VectorType }
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.symbols.state.Meta

/**
 * Laws used to translate AST to Scala with Java Types.
 */
private[translator] object ScalaJTranslateLaws:
  def make(typeNames: TypeNames, meta: Meta): TranslateLaws = new TranslateLaws:
    override def typeConverter: TypeConverter = new JavaTypeConverter(typeNames)
    override def initializer: Initializer     = new ScalaInitializer(typeNames, meta)