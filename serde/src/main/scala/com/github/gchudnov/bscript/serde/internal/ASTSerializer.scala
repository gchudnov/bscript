package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.serde.{ SerdeException, Serializer }
import org.json4s.*
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods.*

final class ASTSerializer extends Serializer[SerdeException, AST]:
  override def serialize(value: AST): Either[SerdeException, String] =
    val ser = ASTSerializeVisitor.make()
    val s   = ()

    val errOrRes =
      for jValue <- value.visit(s, ser)
      yield compact(render(jValue))

    errOrRes.left.map {
      case e: SerdeException => e
      case t                 => new SerdeException("Failed to serialize AST", t)
    }
