package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.{ IntVal, VarDecl }
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.serde.util.ResourceOps.resourceToString
import com.github.gchudnov.bscript.serde.{ Globals, TestSpec }

final class ASTSerializerSpec extends TestSpec:

  private val typeNames: TypeNames = Globals.typeNames

  "ASTSerializer" when {
    "AST is serialized" should {
      "represent var-decl program as a string" in {
        val t = VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0))

        val expected = resourceToString("data/var-decl-ast.json").toTry.get

        val ser      = new ASTSerializer()
        val errOrRes = ser.serialize(t)
        errOrRes match
          case Right(actual) =>
            actual mustBe (expected)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
