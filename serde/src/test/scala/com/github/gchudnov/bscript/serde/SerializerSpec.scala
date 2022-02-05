package com.github.gchudnov.bscript.serde

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ SymbolRef, TypeRef }
import com.github.gchudnov.bscript.lang.types.{ TypeNames, Types }
import com.github.gchudnov.bscript.serde.internal.ASTSerializeVisitor
import com.github.gchudnov.bscript.serde.internal.ASTSerializeVisitor.ASTSerializeState

final class SerializerSpec extends TestSpec:

  private val typeNames: TypeNames = Globals.typeNames

  "Serialize" when {
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

        val errOrRes = eval(t)
        errOrRes match
          case Right(ASTSerializeState(data)) =>
            print(data)
//            data mustBe "123"
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }

  private def eval(ast0: AST): Either[Throwable, ASTSerializeState] =
    val v1 = ASTSerializeVisitor.make()
    val s1 = ASTSerializeState.make()
    ast0.visit(s1, v1)
