package com.github.gchudnov.bscript.translator

import com.github.gchudnov.bscript.lang.symbols.Type
import com.github.gchudnov.bscript.translator.laws.{ Initializer, TypeConverter }

/**
 * Laws used to translate BScript to Scala Types
 */
trait TranslateLaws:
  def typeConverter: TypeConverter
  def initializer: Initializer
