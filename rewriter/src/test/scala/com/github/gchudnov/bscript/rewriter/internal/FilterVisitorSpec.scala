package com.github.gchudnov.bscript.rewriter.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, SymbolRef, TypeRef }
import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.rewriter.internal.RTypeNames
import com.github.gchudnov.bscript.rewriter.TestSpec

final class FilterVisitorSpec extends TestSpec:
  "FilterVisitor" when {
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
     *
     *     void g(int x) { a.x = x; }
     *     void f(int x) { int y = 1; g(2*x); a.b.y = 2; }
     *     void main() { f(3); }
     *
     *     main();
     *     a;
     *   }
     * }}}
     */
    val ast0 = Block(
      StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef("B"), "b"))),
      StructDecl("B", List(FieldDecl(TypeRef(typeNames.i32Type), "y"))),
      VarDecl(TypeRef("A"), "a", Init(TypeRef("A"))),
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
        "f",
        List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
        Block(
          VarDecl(TypeRef(typeNames.i32Type), "y", IntVal(1)),
          Call(SymbolRef("g"), List(Mul(IntVal(2), Var(SymbolRef("x"))))),
          Assign(
            Access(Access(Var(SymbolRef("a")), Var(SymbolRef("b"))), Var(SymbolRef("y"))),
            IntVal(2)
          )
        )
      ),
      MethodDecl(
        TypeRef(typeNames.voidType),
        "main",
        List.empty[ArgDecl],
        Block(
          Call(SymbolRef("f"), List(IntVal(3)))
        )
      ),
      Call(SymbolRef("main"), List.empty[Expr]),
      Var(SymbolRef("a"))
    )

    "AST is filtered" should {
      "retain only nodes that match the predicate" in {
        val pred = (n: AST) =>
          n match
            case _: MethodDecl => false
            case _: Call       => false
            case _             => true

        val expected = Block(
          StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef("B"), "b"))),
          StructDecl("B", List(FieldDecl(TypeRef(typeNames.i32Type), "y"))),
          VarDecl(TypeRef("A"), "a", Init(TypeRef("A"))),
          Var(SymbolRef("a"))
        )

        val errOrRes = eval(ast0, pred)
        errOrRes match
          case Right(optAst) =>
            optAst match
              case Some(actual) =>
                actual mustBe (expected)
              case None =>
                fail("Expected Some(AST), got None")
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "whole AST might be filtered out" in {
        val pred = (n: AST) => false

        val expected: Option[AST] = None

        val errOrRes = eval(ast0, pred)
        errOrRes match
          case Right(actual) =>
            actual mustBe (expected)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }

  private def eval(ast: AST, pred: (AST) => Boolean): Either[Throwable, Option[AST]] =
    val filterVisitor = FilterVisitor.make(pred)
    val filterState   = FilterState.make()
    ast.visit(filterState, filterVisitor)
