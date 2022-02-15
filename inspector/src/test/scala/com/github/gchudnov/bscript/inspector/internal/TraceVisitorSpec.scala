package com.github.gchudnov.bscript.inspector.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, SymbolRef, TypeRef }
import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.inspector.internal.InspectorTypeNames
import com.github.gchudnov.bscript.inspector.TestSpec


final class TraceVisitorSpec extends TestSpec {
  "TraceVisitor" when {
    val typeNames = InspectorTypeNames.make()

    "AST is augmented" should {

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
       * 
       *   // after transformation
       *   {
       *      main();
       *   }
       * 
       *   // becomes
       *   {
       *      { trace("main-enter"); r = main(); trace("main-exit"); r; }
       *   }
       * }}}
       */  
      "produce a new AST" in {
        val t = Block(
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

        // TODO: add a test to verify that AST was modified
      }
    }
  }
}
