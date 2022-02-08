package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.serde.SerdeException
import com.github.gchudnov.bscript.serde.{ Deserializer, SerdeException }

import org.json4s.*
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods.*

import scala.util.control.Exception.allCatch

private[serde] final class JSONDeserializer extends Deserializer[SerdeException, AST]:
  override def deserialize(data: String): Either[SerdeException, AST] =
    val de = JSONDeserializeVisitor.make()

    val errOrAst = for
      jValue <- allCatch.either(parse(data))
      ast    <- de.visitAST(jValue)
    yield ast

    errOrAst.left.map {
      case e: SerdeException => e
      case t                 => new SerdeException("Failed to deserialize AST", t)
    }
