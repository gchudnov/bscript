package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.rewriter.{ Predicates, Rewriter }
import com.github.gchudnov.bscript.rewriter.internal.FilterState
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.serde.{ SerdeException, Serializer }
import org.json4s.*
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods.*

private[serde] final class JSONSerializer extends Serializer[SerdeException, AST]:
  override def serialize(value: AST): Either[SerdeException, String] =
    val keeper                    = Rewriter.filter(ast => !Predicates.hasStdAnn(ast))
    val keeperState               = FilterState.make()
    val ser: JSONSerializeVisitor = JSONSerializeVisitor.make()
    val unitState                 = ()

    val errOrRes =
      for
        filteredAst <- value.visit(keeperState, keeper)
        jValue      <- filteredAst.visit(unitState, ser)
      yield compact(render(jValue))

    errOrRes.left.map {
      case e: SerdeException => e
      case t                 => new SerdeException("Failed to serialize AST", t)
    }
