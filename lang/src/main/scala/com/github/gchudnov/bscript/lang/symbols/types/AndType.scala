package com.github.gchudnov.bscript.lang.symbols.types

import com.github.gchudnov.bscript.lang.symbols.Type

final case class AndType(left: Type, right: Type) extends AndOrType
