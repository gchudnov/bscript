package com.github.gchudnov.bscript.serde

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.serde.internal.{ JSONDeserializer, JSONSerializer }

sealed trait JSONSerde:

  def make(): Serde[SerdeException, AST] =
    Serde(new JSONDeserializer())(new JSONSerializer())

object JSONSerde extends JSONSerde
