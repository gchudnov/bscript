package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.serde.util.ResourceOps.resourceToString
import com.github.gchudnov.bscript.serde.{ SGlobals, TestSpec }

final class ASTSerializerSpec extends TestSpec:

  private val typeNames: TypeNames = SGlobals.typeNames

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

      "skip [std] methods" in {
        val t = Block(
          MethodDecl(
            TypeRef(typeNames.i32Type),
            "strLen",
            List(
              ArgDecl(TypeRef(typeNames.strType), "s")
            ),
            Block(
              CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.i32Type))
            ),
            Seq(ComAnn("returns the length of the provided string"), StdAnn())
          ),
          VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0))
        )

        val expected = resourceToString("data/var-decl-block-ast.json").toTry.get

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
