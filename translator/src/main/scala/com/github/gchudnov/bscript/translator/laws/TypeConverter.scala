package com.github.gchudnov.bscript.translator.laws

import com.github.gchudnov.bscript.lang.symbols.Type

trait TypeConverter:
  def toTypeName(t: Type): Either[Throwable, String]

  def trueValue: String
  def falseValue: String
