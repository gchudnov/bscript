package com.github.gchudnov.bscript.lang.ast.translate

import com.github.gchudnov.bscript.lang.symbols.Type

trait TranslateType:
  def toLangTypeName(t: Type): Either[Throwable, String]

  def boolTrue: String
  def boolFalse: String
