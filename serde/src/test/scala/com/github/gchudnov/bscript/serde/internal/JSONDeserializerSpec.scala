package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.{ ArgDecl, Block, ComAnn, IntVal, MethodDecl, VarDecl }
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.serde.util.ResourceOps.resourceToString
import com.github.gchudnov.bscript.serde.{ SGlobals, TestSpec }

final class JSONDeserializerSpec extends TestSpec:

  private val typeNames: TypeNames = SGlobals.typeNames

  "JSONDeserializer" when {
    "AST is deserialized" should {
      "convert it back to AST if the input is valid" in {
        val input = resourceToString("data/var-decl-ast.json").toTry.get

        val expected = VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0))

        val de       = new JSONDeserializer()
        val errOrRes = de.deserialize(input)
        errOrRes match
          case Right(actual) =>
            actual mustBe (expected)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "deserialize annotations as well" in {
        val input = resourceToString("data/method-decl-with-ann.json").toTry.get

        val expected = Block(
          MethodDecl(
            TypeRef(typeNames.voidType),
            "exit",
            List(
              ArgDecl(TypeRef(typeNames.i32Type), "code")
            ),
            Block(
            ),
            Seq(ComAnn("Terminates the application with the provided status code"))
          )
        )

        val de       = new JSONDeserializer()
        val errOrRes = de.deserialize(input)
        errOrRes match
          case Right(actual) =>
            actual mustBe (expected)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
