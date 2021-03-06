package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.rewriter.{ Predicates, Rewriter }
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.serde.{ SerdeException, Serializer }
import org.json4s.*
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods.*

private[serde] final class JSONSerializer extends Serializer[SerdeException, AST]:
  override def serialize(value: AST): Either[SerdeException, String] =
    val ser: JSONSerializeVisitor = JSONSerializeVisitor.make()
    val unitState: Unit           = ()

    val errOrRes =
      for
        filteredAst <- Rewriter
                         .filter(value, ast => !Predicates.hasStdAnn(ast))
                         .flatMap(_.toRight(new SerdeException("The whole AST was filtered out. No AST to Serialize")))
        jValue <- filteredAst.visit(unitState, ser)
      yield compact(render(jValue))

    errOrRes.left.map {
      case e: SerdeException => e
      case t                 => new SerdeException("Failed to serialize AST", t)
    }
