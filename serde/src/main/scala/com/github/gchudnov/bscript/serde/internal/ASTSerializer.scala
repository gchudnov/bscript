package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.serde.internal.ASTSerializeVisitor.ASTSerializeState
import com.github.gchudnov.bscript.serde.{ SerdeException, Serializer }
import org.json4s.*
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods.*

final class ASTSerializer extends Serializer[SerdeException, AST]:
  override def serialize(value: AST): Either[SerdeException, String] =
    val v1 = ASTSerializeVisitor.make()
    val s1 = ASTSerializeState.make()

    val errOrRes =
      for state <- value.visit(s1, v1)
      yield compact(render(state.data))

    errOrRes.left.map(t => new SerdeException("Failed to serialize AST", t))
