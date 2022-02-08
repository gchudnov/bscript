package com.github.gchudnov.bscript.translator.laws

import com.github.gchudnov.bscript.lang.symbols.Type

trait Initializer:
  def init(toType: Type): Either[Throwable, Seq[String]]
