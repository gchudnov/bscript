package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.serde.SerdeException
import com.github.gchudnov.bscript.serde.{Deserializer, SerdeException}

final class ASTDeserializer extends Deserializer[SerdeException, AST]:
  override def deserialize(data: String): Either[SerdeException, AST] = ???
