package com.github.gchudnov.bscript.translator.internal.c.laws

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.symbols.{DeclType, Type, VectorType}
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.translator.TranslateLaws
import com.github.gchudnov.bscript.translator.internal.c.CInitializer
import com.github.gchudnov.bscript.translator.laws.{Initializer, TypeConverter, TypeInit}

/**
 * Laws used to translate AST to Scala.
 */
private[internal] object CTranslateLaws:
  def make(typeNames: TypeNames, typeInit: TypeInit, meta: Meta): TranslateLaws = new TranslateLaws:
    override def typeConverter: TypeConverter = new CTypeConverter(typeNames)
    override def initializer: Initializer     = new CInitializer(typeNames, typeInit, meta)
