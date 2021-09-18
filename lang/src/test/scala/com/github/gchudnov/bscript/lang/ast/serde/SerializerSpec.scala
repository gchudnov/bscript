package com.github.gchudnov.bscript.lang.ast.serde

import com.github.gchudnov.bscript.lang.ast.serde.internal.ASTSerializeVisitor
import com.github.gchudnov.bscript.lang.ast.serde.internal.ASTSerializeVisitor.ASTSerializeState
import com.github.gchudnov.bscript.lang.ast.visitors.ScopeBuildVisitor.ScopeBuildState
import com.github.gchudnov.bscript.lang.ast.visitors.ScopeResolveVisitor.ScopeResolveState
import com.github.gchudnov.bscript.lang.ast.visitors.TypeCheckVisitor.TypeCheckState
import com.github.gchudnov.bscript.lang.ast.visitors.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ SymbolRef, TypeRef }
import com.github.gchudnov.bscript.lang.types.{ TypeNames, Types }
import com.github.gchudnov.bscript.lang.util.Gen
import com.github.gchudnov.bscript.lang.{ TestSpec, Topology }

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
    val (initMeta, rootScope) = Globals.make()
    val v1                    = ScopeBuildVisitor.make()
    val s1                    = ScopeBuildState.make(ast0, initMeta, rootScope, Gen.empty)

    ast0
      .visit(s1, v1)
      .flatMap { s11 =>
        val v2   = ScopeResolveVisitor.make(typeNames)
        val s2   = ScopeResolveState.make(s11.ast, s11.meta)
        val ast1 = s11.ast

        ast1
          .visit(s2, v2)
          .flatMap { s21 =>
            val ss2  = s21.meta
            val ast2 = s21.ast

            Types
              .make(ss2, typeNames)
              .flatMap { types =>
                val typeCheckTables = TypeCheckTables.make(types)

                val v3 = TypeCheckVisitor.make(types, typeCheckTables)
                val s3 = TypeCheckState.make(ast2, s21.meta)
                ast2
                  .visit(s3, v3)
                  .flatMap({ s31 =>
                    val t    = Topology(meta = s31.meta, ast = s31.ast)
                    val ast3 = s31.ast

                    val v4 = ASTSerializeVisitor.make(t.meta)
                    val s4 = ASTSerializeState.make()

                    ast3.visit(s4, v4)
                  })
              }
          }
      }
