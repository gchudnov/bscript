package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ SymbolRef, TypeRef }
import com.github.gchudnov.bscript.lang.types.{ TypeNames, Types }
import com.github.gchudnov.bscript.serde.internal.ASTSerializeVisitor
import com.github.gchudnov.bscript.serde.util.ResourceOps.resourceToString
import com.github.gchudnov.bscript.serde.{ Globals, TestSpec }
import org.json4s.*
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods.*

import scala.util.control.Exception.allCatch

final class ASTDeserializeVisitorSpec extends TestSpec:

  private val typeNames: TypeNames = Globals.typeNames

  "ASTDeserializeVisitor" when {
    "AST is deserialized" should {
      "convert struct back to AST if the input is valid" in {
        val input = resourceToString("data/struct-program-ast.json").toTry.get

        val expected = Block(
          StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef(typeNames.i64Type), "z"), FieldDecl(TypeRef("B"), "b"))),
          StructDecl("B", List(FieldDecl(TypeRef(typeNames.i32Type), "y"))),
          VarDecl(TypeRef("A"), "a", Init(TypeRef("A"))),
          Assign(
            Access(Var(SymbolRef("a")), Var(SymbolRef("x"))),
            IntVal(5)
          ),
          Assign(
            Access(Var(SymbolRef("a")), Var(SymbolRef("z"))),
            Access(Var(SymbolRef("a")), Var(SymbolRef("x")))
          ),
          Access(Var(SymbolRef("a")), Var(SymbolRef("z")))
        )

        val errOrRes = eval(input)
        errOrRes match
          case Right(actual) =>
            actual.mustBe(expected)
          case Left(t) =>
            print(t)
            fail("Should be 'right", t)
      }
    }
  }

  private def eval(input: String): Either[Throwable, AST] =
    val v1 = ASTDeserializeVisitor.make()

    for
      jValue <- allCatch.either(parse(input))
      ast    <- v1.visitAST(jValue)
    yield ast
