package com.github.gchudnov.bscript.serde

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.serde.internal.{ ASTDeserializer, ASTSerializer }

sealed trait ASTSerde:

  def ast: Serde[SerdeException, AST] =
    Serde(new ASTDeserializer())(new ASTSerializer())

object ASTSerde extends ASTSerde
