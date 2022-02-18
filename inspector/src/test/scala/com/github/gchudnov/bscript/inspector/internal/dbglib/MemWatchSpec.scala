package com.github.gchudnov.bscript.inspector.internal.dbglib

import com.github.gchudnov.bscript.inspector.TestSpec
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, SymbolRef, TypeRef }
import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.inspector.internal.InspectorTypeNames
import com.github.gchudnov.bscript.inspector.internal.dbglib.MemWatch
import com.github.gchudnov.bscript.inspector.internal.dbglib.{ MemWatchDiff, MemWatchStashEntry }
import com.github.gchudnov.bscript.interpreter.memory.CellPath

final class MemWatchSpec extends TestSpec:
  val typeNames = InspectorTypeNames.make()

  "MemWatch" when {

    "a path is watched" should {

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
      "augment the AST with tracing capabilities" in {
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

        val errOrRes = eval(CellPath("a"), ast0)

        errOrRes match
          case Right(actual) =>
            val block      = actual.asInstanceOf[Block]
            val methodDecl = block.statements(0).asInstanceOf[MethodDecl]
            val callBlock  = block.statements(7).asInstanceOf[Block] // NOTE: `Call(SymbolRef("main"), List.empty[Expr])` is wrapped in a `Block`

            methodDecl.name mustBe ("memWatch")

          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }

  def eval(path: CellPath, ast0: AST): Either[Throwable, AST] =
    MemWatch.make(path, ast0, typeNames)
