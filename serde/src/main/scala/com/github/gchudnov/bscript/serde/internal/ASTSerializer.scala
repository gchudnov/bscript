package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.serde.{ SerdeException, Serializer }
import org.json4s.*
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods.*

final class ASTSerializer extends Serializer[SerdeException, AST]:
  override def serialize(value: AST): Either[SerdeException, String] =
    val v1 = ASTSerializeVisitor.make()
    val s1 = ()

    val errOrRes =
      for jValue <- value.visit(s1, v1)
      yield compact(render(jValue))

    errOrRes.left.map {
      case e: SerdeException => e
      case t                 => new SerdeException("Failed to serialize AST", t)
    }
