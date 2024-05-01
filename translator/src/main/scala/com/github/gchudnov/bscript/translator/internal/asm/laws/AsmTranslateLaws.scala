package com.github.gchudnov.bscript.translator.internal.asm.laws

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.symbols.{DeclType, Type, VectorType}
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.translator.TranslateLaws
import com.github.gchudnov.bscript.translator.internal.asm.AsmInitializer
import com.github.gchudnov.bscript.translator.laws.{Initializer, TypeConverter, TypeInit}

/**
 * Laws used to translate AST to Scala.
 */
private[internal] object AsmTranslateLaws:
  def make(typeNames: TypeNames, typeInit: TypeInit, meta: Meta): TranslateLaws = new TranslateLaws:
    override def typeConverter: TypeConverter = new AsmTypeConverter(typeNames)
    override def initializer: Initializer     = new AsmInitializer(typeNames, typeInit, meta)
