package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.serde.{ SerdeException, Serializer }
import org.json4s.*
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods.*

private[serde] final class JSONSerializer extends Serializer[SerdeException, AST]:
  override def serialize(value: AST): Either[SerdeException, String] =
    val keeper: KeepASTVisitor = KeepASTVisitor.make(KeepASTVisitor.hasNoStdAnn)
    val ser: JSONSerializeVisitor = JSONSerializeVisitor.make()
    val s: Unit = ()

    val errOrRes =
      for
        filteredAst <- value.visit(s, keeper)
        jValue      <- filteredAst.visit(s, ser)
      yield compact(render(jValue))

    errOrRes.left.map {
      case e: SerdeException => e
      case t                 => new SerdeException("Failed to serialize AST", t)
    }
