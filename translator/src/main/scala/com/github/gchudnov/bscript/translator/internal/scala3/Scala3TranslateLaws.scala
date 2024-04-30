package com.github.gchudnov.bscript.translator.internal.scala3

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.symbols.DeclType
import com.github.gchudnov.bscript.lang.symbols.Type
import com.github.gchudnov.bscript.lang.symbols.VectorType
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.translator.TranslateLaws
import com.github.gchudnov.bscript.translator.laws.ScalaInitializer
import com.github.gchudnov.bscript.translator.internal.scala3.laws.Scala3TypeConverter
import com.github.gchudnov.bscript.translator.laws.Initializer
import com.github.gchudnov.bscript.translator.laws.TypeConverter
import com.github.gchudnov.bscript.translator.laws.TypeInit

/**
 * Laws used to translate AST to Scala.
 */
private[internal] object Scala3TranslateLaws:
  def make(typeNames: TypeNames, typeInit: TypeInit, meta: Meta): TranslateLaws = new TranslateLaws:
    override def typeConverter: TypeConverter = new Scala3TypeConverter(typeNames)
    override def initializer: Initializer     = new ScalaInitializer(typeNames, typeInit, meta)
