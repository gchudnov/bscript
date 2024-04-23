package com.github.gchudnov.bscript.translator.laws

import com.github.gchudnov.bscript.lang.symbols.Type
import scala.collection.immutable.Seq

trait Initializer:
  def init(toType: Type): Either[Throwable, Seq[String]]
