package com.github.gchudnov.bscript.serde

import com.github.gchudnov.bscript.lang.ast.{ IntVal, VarDecl }
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.serde.internal.ASTSerializer
import com.github.gchudnov.bscript.serde.util.ResourceOps.resourceToString
import com.github.gchudnov.bscript.serde.{ Globals, TestSpec }

final class ASTSerdeSpec extends TestSpec:

  private val typeNames: TypeNames = Globals.typeNames

  "ASTSerde" when {
    "AST is serialized" should {
      "represent it as a string" in {
        val t = VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0))

        val expected = resourceToString("data/var-decl-ast.json").toTry.get

        val errOrRes = ASTSerde.ast.serialize(t)
        errOrRes match
          case Right(actual) =>
            actual mustBe (expected)

          case Left(t) => fail("Should be 'right", t)
      }
    }
  }
