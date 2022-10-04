package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, SymbolRef, TypeRef }
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

      "restore annotations" in {
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

      "parse NULL-ELSE in IF-THEN-ELSE" in {
        val input = resourceToString("data/if-then-else-null.json").toTry.get

        val expected = If(BoolVal(true), Add(IntVal(2), IntVal(5)))

        val de       = new JSONDeserializer()
        val errOrRes = de.deserialize(input)
        errOrRes match
          case Right(actual) =>
            actual mustBe (expected)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "parse absent ELSE in IF-THEN-ELSE" in {
        val input = resourceToString("data/if-then-else.json").toTry.get

        val expected = If(BoolVal(true), Add(IntVal(2), IntVal(5)))

        val de       = new JSONDeserializer()
        val errOrRes = de.deserialize(input)
        errOrRes match
          case Right(actual) =>
            actual mustBe (expected)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "read a collection" in {
        val input = resourceToString("data/vec.json").toTry.get

        val expected = Vec(List(IntVal(1), IntVal(2), IntVal(3)))

        val de       = new JSONDeserializer()
        val errOrRes = de.deserialize(input)
        errOrRes match
          case Right(actual) =>
            actual mustBe (expected)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "should not throw deserialization exception if methods have no annotations" in {
        // NOTE: it was a bug where methods without annotations were throwing exceptions on parsing

        val input = resourceToString("data/ast-example-1.json").toTry.get

        val expected = Block(
          StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef("B"), "b"))),
          StructDecl("B", List(FieldDecl(TypeRef(typeNames.i32Type), "y"))),
          VarDecl(TypeRef("A"), "a", Init(TypeRef("A"))),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "f",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
            Block(
              Assign(
                Access(Access(Var(SymbolRef("a")), Var(SymbolRef("b"))), Var(SymbolRef("y"))),
                Var(SymbolRef("x"))
              )
            )
          ),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "g",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
            Block(
              Assign(
                Access(Var(SymbolRef("a")), Var(SymbolRef("x"))),
                Var(SymbolRef("x"))
              )
            )
          ),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "h",
            List.empty[ArgDecl],
            Block(
            )
          ),
          Call(SymbolRef("f"), List(IntVal(10))),
          Call(SymbolRef("g"), List(IntVal(20))),
          Call(SymbolRef("h"), List.empty[Expr]),
          Var(SymbolRef("a"))
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
