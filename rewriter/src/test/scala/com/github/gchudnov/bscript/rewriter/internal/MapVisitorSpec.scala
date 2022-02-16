package com.github.gchudnov.bscript.rewriter.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, SymbolRef, TypeRef }
import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.rewriter.internal.RTypeNames
import com.github.gchudnov.bscript.rewriter.TestSpec

final class MapVisitorSpec extends TestSpec:
  "MapVisitor" when {
    val typeNames = RTypeNames.make()

    /**
     * {{{
     *   // globals
     *   {
     *     struct A {
     *       int x;
     *       B b;
     *     };
     *     struct B { int y; };
     *
     *     A a;
     *     a;
     *   }
     * }}}
     */
    val ast0 = Block(
      StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef("B"), "b"))),
      Var(SymbolRef("a"))
    )

    "AST is mapped" should {

      "produce a new AST" in {
        val f = (n: AST) =>
          n match
            case x: Var =>
              Block(x)
            case x =>
              x

        val expected = Block(
          StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef("B"), "b"))),
          Block(Var(SymbolRef("a")))
        )

        val errOrRes = eval(ast0, f)
        errOrRes match
          case Right(actual) =>
            actual mustBe (expected)

          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }

  private def eval(ast: AST, f: (AST) => AST): Either[Throwable, AST] =
    val mapVisitor = MapVisitor.make(f)
    val mapState   = MapState.make()
    ast.visit(mapState, mapVisitor)
