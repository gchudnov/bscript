package com.github.gchudnov.bscript.lang.ast.serde.internal

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.ast.serde.{ SerdeException, Serializer }

final class ASTSerializer extends Serializer[SerdeException, AST]:
  override def serialize(value: AST): Either[SerdeException, String] = ???
