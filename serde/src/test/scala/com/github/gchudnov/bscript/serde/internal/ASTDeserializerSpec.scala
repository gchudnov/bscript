package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.{ IntVal, VarDecl }
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.serde.util.ResourceOps.resourceToString
import com.github.gchudnov.bscript.serde.{ SGlobals, TestSpec }

final class ASTDeserializerSpec extends TestSpec:

  private val typeNames: TypeNames = SGlobals.typeNames

  "ASTDeserializer" when {
    "AST is deserialized" should {
      "convert it back to AST if the input is valid" in {
        val input = resourceToString("data/var-decl-ast.json").toTry.get

        val expected = VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0))

        val de       = new ASTDeserializer()
        val errOrRes = de.deserialize(input)
        errOrRes match
          case Right(actual) =>
            actual mustBe (expected)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
