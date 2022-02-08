package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ SymbolRef, TypeRef }
import com.github.gchudnov.bscript.lang.types.{ TypeNames, Types }
import com.github.gchudnov.bscript.serde.internal.JSONSerializeVisitor
import com.github.gchudnov.bscript.serde.util.ResourceOps.resourceToString
import com.github.gchudnov.bscript.serde.{ SGlobals, TestSpec }
import org.json4s.*
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods.*

final class JSONSerializeVisitorSpec extends TestSpec:

  private val typeNames: TypeNames = SGlobals.typeNames

  "JSONSerializeVisitor" when {
    "AST is serialized" should {
      "represent struct program as a string" in {
        val t = Block(
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

        val expected = resourceToString("data/struct-program-ast.json").toTry.get

        val errOrRes = eval(t)
        errOrRes match
          case Right(jValue) =>
            val actual = compact(render(jValue))
            actual.mustBe(expected)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }

  private def eval(ast0: AST): Either[Throwable, JValue] =
    val v1 = JSONSerializeVisitor.make()
    val s1 = ()
    ast0.visit(s1, v1)
