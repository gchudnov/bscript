package com.github.gchudnov.bscript.serde

import com.github.gchudnov.bscript.lang.ast.{ IntVal, VarDecl }
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.serde.internal.JSONSerializer
import com.github.gchudnov.bscript.serde.util.ResourceOps.resourceToString
import com.github.gchudnov.bscript.serde.{ SGlobals, TestSpec }

final class JSONSerdeSpec extends TestSpec:

  private val typeNames: TypeNames = SGlobals.typeNames

  "JSONSerde" when {
    "AST is serialized" should {
      "represent it as a string" in {
        val t = VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0))

        val expected = resourceToString("data/var-decl-ast.json").toTry.get

        val serde    = JSONSerde.make()
        val errOrRes = serde.serialize(t)
        errOrRes match
          case Right(actual) =>
            actual mustBe (expected)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "AST is deserialized" should {
      "convert it back to AST" in {
        val input = resourceToString("data/var-decl-ast.json").toTry.get

        val expected = VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0))

        val serde    = JSONSerde.make()
        val errOrRes = serde.deserialize(input)
        errOrRes match
          case Right(actual) =>
            actual mustBe (expected)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
